##############################
# This is a shiny app for simulating a contrarian strategy 
# using the Hampel filter over prices.
# It uses reactive code to avoid unnecessary calculations.
# It flips the position only if the indicator persists over several 
# consecutive periods equal to lagg.
# This is the best performing version.
# 
# Just press the "Run App" button on upper right of this panel.
##############################


# References about reactive code.
# https://community.rstudio.com/t/reactivevalues-vs-reactive-and-eventreactive-a-general-question/27449/5
# https://riptutorial.com/shiny/example/32343/observeevent
# https://riptutorial.com/shiny/example/32341/eventreactive
# https://stackoverflow.com/questions/39170161/r-shiny-eventreactive-with-two-controls

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Model and data setup

## VTI ETF daily bars
# symbol <- "VTI"
# closep <- log(Cl(rutils::etfenv$VTI))

## SPY ETF 1-minute bars - works really well !!!
symbol <- "SPY"
ohlc <- HighFreq::SPY["2011"]["T09:31:00/T15:59:00"]
nrows <- NROW(ohlc)
closep <- log(Cl(ohlc))


## Load 1-minute bars
# captiont <- "Strategy for 1-minute LODE Bars"
# symbol <- "LODE"
# ohlc <- data.table::fread(file="/Volumes/external/Develop/Predictive/data/lode_oneminutebars.csv", sep=",")
# nrows <- NROW(ohlc)
# dates <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out=nrows)
# closep <- log(ohlc$close)
# closep <- xts::xts(closep, dates)
# retv <- rutils::diffit(closep)


## Load QM futures 5-second bars
# symbol <- "ES"  # S&P500 Emini futures
# symbol <- "QM"  # oil
# load(file=paste0("/Users/jerzy/Develop/data/ib_data/", symbol, "_ohlc.RData"))
# closep <- log(Cl(ohlc))
# Or random prices
# closep <- xts(cumsum(rnorm(nrows)), index(ohlc))

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

captiont <- paste("Contrarian Strategy for", symbol, "Using the Hampel Filter Over Prices")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  # fluidRow(
  # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
  #   column(width=12,
  #          h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
  #          actionButton("recalcb", "Recalculate the Model"))
  # ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input end points interval
    # column(width=2, selectInput("interval", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input look-back interval
    column(width=2, sliderInput("lookb", label="Lookback", min=3, max=30, value=15, step=1)),
    # Input lag trade parameter
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1)),
    # Input threshold interval
    column(width=2, sliderInput("threshold", label="threshold", min=0.5, max=3.0, value=1.0, step=0.1))
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
  # datav <- shiny::reactive({
  # Get model parameters from input argument
  # lookb <- isolate(input$lookb)
  # lagg <- isolate(input$lagg)
  # dimax <- isolate(input$dimax)
  # threshold <- isolate(input$threshold)
  # look_lag <- isolate(input$look_lag
  # lambda <- isolate(input$lambda)
  # typev <- isolate(input$typev)
  # alpha <- isolate(input$alpha)
  # quant <- isolate(input$quant)
  # coeff <- as.numeric(isolate(input$coeff))
  # bidask <- isolate(input$bidask)
  # Model is recalculated when the recalcb variable is updated
  # input$recalcb
  
  
  # lookb <- 11
  # half_window <- lookb %/% 2
  
  # Calculate zscores if new lookb value
  zscores <- eventReactive(input$lookb, {
    cat("Calculating zscores\n")
    # Calculate the zscores
    medianv <- TTR::runMedian(closep, n=input$lookb)
    medianv[1:input$lookb, ] <- 1
    # madv <- TTR::runMAD(retv, n=lookb)
    # madv[1:lookb, ] <- 1
    # zscores <- ifelse(madv != 0, (closep-medianv)/madv, 0)
    # Don't divide zscores by the madv because it's redundant since zscores is divided by the mad_zscores.
    zscores <- (closep-medianv)
    # zscores[1:lookb, ] <- 0
    mad_zscores <- TTR::runMAD(zscores, n=10*input$lookb)
    mad_zscores[1:(10*input$lookb), ] <- 0
    zscores <- ifelse(mad_zscores != 0, zscores/mad_zscores, 0)
  })  # end eventReactive
  
  # Plot histogram of zscores
  # range(zscores)
  # zscores <- zscores[zscores > quantile(zscores, 0.05)]
  # zscores <- zscores[zscores < quantile(zscores, 0.95)]
  # x11(width=6, height=5)
  # hist(zscores, xlim=c(quantile(zscores, 0.05), quantile(zscores, 0.95)), breaks=50, main=paste("Z-scores for", "lookb =", lookb))
  
  # Calculate posv if new lookb or threshold values
  datav <- eventReactive(list(input$lookb, input$threshold), {
    cat("Calculating posv\n")
    # Determine if the zscores have exceeded the threshold
    indic <- rep(0, nrows)
    # indic[1] <- 0
    indic <- ifelse(zscores() > input$threshold, -1, indic)
    indic <- ifelse(zscores() < (-input$threshold), 1, indic)
    # Calculate number of consecutive indicators in same direction.
    # This is designed to avoid trading on microstructure noise.
    # indic <- ifelse(indic == indic_lag, indic, indic)
    indics <- HighFreq::roll_sum(tseries=matrix(indic), lookb=input$lagg)
    indics[1:input$lagg] <- 0
    
    # Calculate posv and pnls from indics.
    # posv <- rep(NA_integer_, nrows)
    # posv[1] <- 0
    # threshold <- 3*mad(zscores)
    # Flip position only if the indics is at least equal to lagg.
    # Otherwise keep previous position.
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    posv <- ifelse(indics >= input$lagg, 1, posv)
    posv <- ifelse(indics <= (-input$lagg), -1, posv)
    # posv <- ifelse(zscores > threshold, -1, posv)
    # posv <- ifelse(zscores < (-threshold), 1, posv)
    posv <- zoo::na.locf(posv, na.rm=FALSE)
    posv <- rutils::lagit(posv, lagg=1)
    
    # Number of trades
    ntrades <- sum(abs(rutils::diffit(posv)))# / nrows
    captiont <- paste("Number of trades =", ntrades)
    
    pnls <- cumsum(posv*retv)
    pnls <- cbind(pnls, cumsum(retv))
    colnames(pnls) <- c("Strategy", "Index")
    # pnls[rutils::calc_endpoints(pnls, interval="minutes")]
    # pnls[rutils::calc_endpoints(pnls, interval="hours")]
    list(caption=captiont, pnls=pnls)
  })  # end eventReactive
  
  # })  # end reactive code
  
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    captiont <- datav()$caption
    pnls <- datav()$pnls
    colnamev <- colnames(pnls)
    dygraphs::dygraph(pnls, main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="blue")
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
