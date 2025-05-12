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

# retp <- na.omit(rutils::etfenv$returns$VTI)


## Load QM futures 5-second bars
# symboln <- "ES"  # S&P500 Emini futures
# symboln <- "QM"  # oil
# load(file=paste0("/Users/jerzy/Develop/data/ib_data/", symboln, "_ohlc.RData"))
# pricev <- Cl(ohlc)
# Or random prices
# pricev <- xts(exp(cumsum(rnorm(NROW(ohlc)))), index(ohlc))

## Load VX futures 5-second bars
# symboln <- "VX"
# load(file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")
# pricev <- Cl(vix_env$chain_ed)

## VTI ETF daily bars
# symboln <- "VTI"
# pricev <- Cl(rutils::etfenv$VTI)

## SPY ETF minute bars
# symboln <- "SPY"
# pricev <- Cl(HighFreq::SPY["2011"])["T09:31:00/T15:59:00"]

# retp <- rutils::diffit(log(pricev))

captiont <- paste("Trend Following Strategy Using Running Kelly Ratio")
# captiont <- paste("Contrarian Strategy for", symboln, "Using the Hampel Filter Over Prices")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
    # column(width=12, 
    #        h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
    #        actionButton("recalcb", "Recalculate the Model"))
  ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symboln", label="Symbol", choices=rutils::etfenv$symbolv, selected="VTI")),
    # Input end points interval
    # column(width=2, selectInput("interval", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input lambda decay parameter
    column(width=2, sliderInput("lambdaf", label="lambda:", min=0.9, max=0.999, value=0.98, step=0.001)),
    # Input look-back interval
    # column(width=2, sliderInput("lookb", label="Lookback", min=3, max=300, value=200, step=1)),
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
  dygraphs::dygraphOutput("dyplot", width="90%", height="650px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # Create an empty list of reactive values.
  values <- reactiveValues()

  # Load the log returns
  retp <- shiny::reactive({
    
    symboln <- input$symboln
    cat("Recalculating returns for ", symboln, "\n")
    
    ohlc <- get(symboln, rutils::etfenv)
    rutils::diffit(log(quantmod::Cl(ohlc)))
    
  })  # end Load the log returns
  
  
  # Recalculate the data and rerun the model
  pnls <- shiny::reactive({

    symboln <- input$symboln
    cat("Recalculating the PnLs for ", symboln, "\n")
    # Get model parameters from input argument
    # lookb <- input$lookb
    lambdaf <- input$lambdaf
    
    # Rerun the model
    retp <- retp()
    varv <- HighFreq::run_var(retp, lambda=lambdaf)
    meanv <- varv[, 1]
    varv <- varv[, 2]
    weightv <- meanv/varv
    weightv[1:11] <- 0
    # weightv <- zoo::na.locf(weightv, fromLast=TRUE)
    # Calculate compounded wealth from returns
    weightv <- rutils::lagit(weightv)
    # weightv <- 10*weightv/sum(abs(range(weightv)))
    # weightv <- apply(weightv, 2, function(x) 10*x/sum(abs(range(x))))
    # wealthv <- cumprod(1 + rowSums(weightv*retp))
    # Calculate number of trades
    # values$ntrades <- sum(abs(flipi)>0)
    
    pnls <- (weightv*retp)
    pnls <- sd(retp)*pnls/sd(pnls)
    
    pnls <- cbind(retp, pnls)
    colnames(pnls) <- c("Index", "Strategy")

    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    
    pnls
    
  })  # end reactive code
  
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    # Get Sharpe ratios
    sharper <- values$sharper
    # Get number of trades
    # ntrades <- values$ntrades

    symboln <- input$symboln
    captiont <- paste("Strategy for", symboln, "/ \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "))
    
    pnls <- pnls()
    colv <- colnames(pnls)
    colorv <- c("blue", "red")
    endw <- rutils::calc_endpoints(pnls, interval="weeks")
    dygraphs::dygraph(cumsum(pnls)[endw], main=captiont) %>%
      dyOptions(colors=colorv, strokeWidth=1) %>%
      dyLegend(show="always", width=300)
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
