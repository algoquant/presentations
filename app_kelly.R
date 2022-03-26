##############################
# This is a shiny app for simulating a Kelly strategy.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(shiny)
library(dygraphs)
library(HighFreq)

# Model and data setup

returns <- na.omit(rutils::etfenv$returns[, "VTI"])


## Load QM futures 5-second bars
# symbol <- "ES"  # S&P500 Emini futures
# symbol <- "QM"  # oil
# load(file=paste0("C:/Develop/data/ib_data/", symbol, "_ohlc.RData"))
# prices <- Cl(ohlc)
# Or random prices
# prices <- xts(exp(cumsum(rnorm(NROW(ohlc)))), index(ohlc))

## Load VX futures 5-second bars
# symbol <- "VX"
# load(file="C:/Develop/data/vix_data/vix_cboe.RData")
# prices <- Cl(vix_env$chain_ed)

## VTI ETF daily bars
# symbol <- "VTI"
# prices <- Cl(rutils::etfenv$VTI)

## SPY ETF minute bars
# symbol <- "SPY"
# prices <- Cl(HighFreq::SPY["2011"])["T09:31:00/T15:59:00"]

# returns <- rutils::diffit(log(prices))

captiont <- paste("VTI Strategy Using Rolling Kelly Weight")
# captiont <- paste("Contrarian Strategy for", symbol, "Using the Hampel Filter Over Prices")

## End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel(captiont),
  
  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the re_calculate variable is updated
    # column(width=12, 
    #        h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
    #        actionButton("re_calculate", "Recalculate the Model"))
  ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input end points interval
    # column(width=2, selectInput("interval", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Lookback", min=100, max=500, value=200, step=1)),
    # Input look-back lag interval
    # column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1)),
    # Input threshold interval
    # column(width=2, sliderInput("threshold", label="threshold", min=1.0, max=10.0, value=1.8, step=0.2))
    # Input the weight decay parameter
    # column(width=2, sliderInput("lambda", label="Weight decay:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input model weights type
    # column(width=2, selectInput("typev", label="Portfolio weights type",
    #                             choices=c("max_sharpe", "min_var", "min_varpca", "rank"), selected="rank")),
    # Input number of eigenvalues for regularized matrix inverse
    # column(width=2, sliderInput("max_eigen", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
    # Input the shrinkage intensity
    # column(width=2, sliderInput("alpha", label="Shrinkage intensity",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input the percentile
    # column(width=2, sliderInput("quant", label="percentile:", min=0.01, max=0.45, value=0.1, step=0.01)),
    # Input the strategy coefficient: coeff=1 for momentum, and coeff=-1 for contrarian
    # column(width=2, selectInput("coeff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    # column(width=2, numericInput("bid_offer", label="bid-offer:", value=0.001, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
  
)  # end fluidPage interface


## Define the server code
servfunc <- function(input, output) {
  
  # Recalculate the data and rerun the model
  datav <- reactive({
    # Get model parameters from input argument
    look_back <- input$look_back
    # lagg <- input$lagg
    # max_eigen <- isolate(input$max_eigen)
    # threshold <- input$threshold
    # look_lag <- isolate(input$look_lag
    # lambda <- isolate(input$lambda)
    # typev <- isolate(input$typev)
    # alpha <- isolate(input$alpha)
    # quant <- isolate(input$quant)
    # coeff <- as.numeric(isolate(input$coeff))
    # bid_offer <- isolate(input$bid_offer)
    # Model is recalculated when the re_calculate variable is updated
    # input$re_calculate

    
    # look_back <- 11
    # half_window <- look_back %/% 2
    
    # Rerun the VTI model
    # var_rolling <- roll::roll_var(returns, width=look_back)
    # weights <- roll::roll_sum(returns, width=look_back)/look_back
    # weights <- weights/var_rolling
    # weights <- zoo::na.locf(weights, fromLast=TRUE)
    # weights <- drop(HighFreq::lag_vec(weights))
    # weights <- 10*weights/sum(abs(range(weights)))
    # wealth <- cumprod(1 + weights*returns)

    # Rerun the VTI and IEF model
    var_rolling <- roll::roll_var(returns, width=look_back)
    weights <- roll::roll_sum(returns, width=look_back)/look_back
    weights <- weights/var_rolling
    weights <- zoo::na.locf(weights, fromLast=TRUE)
    # Calculate compounded wealth from returns
    weights <- HighFreq::lagit(weights)
    # weights <- 10*weights/sum(abs(range(weights)))
    weights <- apply(weights, 2, function(x) 10*x/sum(abs(range(x))))
    wealth <- cumprod(1 + rowSums(weights*returns))
    wealth <- xts(wealth, index(returns))
    
        
    # Calculate posit and pnls from z-scores and rangev
    # posit <- rep(NA_integer_, NROW(prices))
    # posit[1] <- 0
    # threshold <- 3*mad(zscores)
    # posit <- ifelse(zscores > threshold, -1, posit)
    # posit <- ifelse(zscores < (-threshold), 1, posit)
    # posit <- ifelse(zscores > threshold*mad_zscores, -1, posit)
    # posit <- ifelse(zscores < (-threshold*mad_zscores), 1, posit)
    # posit <- na.locf(posit)
    # positions_lag <- rutils::lagit(posit, lagg=lagg)
    # pnls <- cumsum(positions_lag*returns)
    pnls <- cbind(wealth, cumsum(returns))
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
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue")
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfunc)
