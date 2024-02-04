##############################
# This is a shiny app for simulating a contrarian strategy 
# using the Hampel filter  over returns.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Model and data setup

## VTI ETF daily bars
# symbol <- "VTI"
# closep <- log(Cl(rutils::etfenv$VTI))

## SPY ETF minute bars - works really well !!!
# symbol <- "SPY"
# closep <- log(Cl(HighFreq::SPY["2011"])["T09:31:00/T15:59:00"])

## Load QM futures 5-second bars
# symbol <- "ES"  # S&P500 Emini futures
symbol <- "QM"  # oil
load(file=paste0("/Users/jerzy/Develop/data/ib_data/", symbol, "_ohlc.RData"))
nrows <- NROW(ohlc)
closep <- log(Cl(ohlc))
# Or random prices
# closep <- xts(cumsum(rnorm(NROW(ohlc))), index(ohlc))

## Load combined futures data
# com_bo <- HighFreq::SPY
# load(file="/Users/jerzy/Develop/data/combined.RData")
# symbol <- "UX1"
# symbolv <- unique(rutils::get_name(colnames(com_bo)))
# closep <- log(na.omit(com_bo[, "UX1.Close"]))
# TU1: lookb=14, threshold=2.0, lagg=1
# TU1: lookb=30, threshold=9.2, lagg=1


## Load VX futures daily bars
# symbol <- "VX"
# load(file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")
# closep <- log(Cl(vix_env$chain_ed))

retv <- rutils::diffit(closep)

captiont <- paste("Contrarian Strategy for", symbol, "Using the Hampel Filter Over Returns")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
    column(width=12,
           h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
           actionButton("recalcb", "Recalculate the Model"))
  ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input end points interval
    # column(width=2, selectInput("interval", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input look-back interval
    column(width=2, sliderInput("lookb", label="Lookback", min=3, max=30, value=9, step=1)),
    # Input lag trade parameter
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1)),
    # Input threshold interval
    column(width=2, sliderInput("threshold", label="threshold", min=1.0, max=10.0, value=1.8, step=0.2))
    # Input the weight decay parameter
    # column(width=2, sliderInput("lambda", label="Weight decay:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input model weights type
    # column(width=2, selectInput("typev", label="Portfolio weights type",
    #                             choices=c("max_sharpe", "min_var", "min_varpca", "rank"), selected="rank")),
    # Input number of eigenvalues for regularized matrix inverse
    # column(width=2, sliderInput("dimax", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
    # Input the shrinkage intensity
    # column(width=2, sliderInput("alpha", label="Shrinkage intensity",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input the percentile
    # column(width=2, sliderInput("quant", label="percentile:", min=0.01, max=0.45, value=0.1, step=0.01)),
    # Input the strategy coefficient: coeff=1 for momentum, and coeff=-1 for contrarian
    # column(width=2, selectInput("coeff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-ask spread
    # column(width=2, numericInput("bidask", label="bid-ask:", value=0.001, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # Recalculate the data and rerun the model
  datav <- shiny::reactive({
    # Get model parameters from input argument
    lookb <- isolate(input$lookb)
    lagg <- isolate(input$lagg)
    # dimax <- isolate(input$dimax)
    threshold <- isolate(input$threshold)
    # look_lag <- isolate(input$look_lag
    # lambda <- isolate(input$lambda)
    # typev <- isolate(input$typev)
    # alpha <- isolate(input$alpha)
    # quant <- isolate(input$quant)
    # coeff <- as.numeric(isolate(input$coeff))
    # bidask <- isolate(input$bidask)
    # Model is recalculated when the recalcb variable is updated
    input$recalcb

    
    # lookb <- 11
    # half_window <- lookb %/% 2
    
    # Rerun the model
    medianv <- TTR::runMedian(retv, n=lookb)
    medianv[1:lookb, ] <- 1
    madv <- TTR::runMAD(retv, n=lookb)
    madv[1:lookb, ] <- 1
    zscores <- ifelse(madv != 0, (retv-medianv)/madv, 0)
    zscores[1:lookb, ] <- 0
    # mad_zscores <- TTR::runMAD(zscores, n=lookb)
    # mad_zscores[1:lookb, ] <- 0
    mad_zscores <- 1
    
    # Calculate posv and pnls from z-scores and rangev
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    # threshold <- 3*mad(zscores)
    # posv <- ifelse(zscores > threshold, -1, posv)
    # posv <- ifelse(zscores < (-threshold), 1, posv)
    posv <- ifelse(zscores > threshold*mad_zscores, -1, posv)
    posv <- ifelse(zscores < (-threshold*mad_zscores), 1, posv)
    posv <- zoo::na.locf(posv, na.rm=FALSE)
    posv <- rutils::lagit(posv, lagg=lagg)
    
    # retv <- rutils::diffit(closep)
    pnls <- cumsum(posv*retv)
    pnls <- cbind(pnls, cumsum(retv))
    colnames(pnls) <- c("Strategy", "Index")
    # pnls[rutils::calc_endpoints(pnls, interval="minutes")]
    # pnls[rutils::calc_endpoints(pnls, interval="hours")]
    pnls
  })  # end reactive code
  
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    colnamev <- colnames(datav())
    dygraphs::dygraph(datav(), main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="blue")
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
