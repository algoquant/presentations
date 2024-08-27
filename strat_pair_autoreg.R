##############################
# This is a shiny app for backtesting an ETF pair strategy
# using a fixed beta of returns and autoregressive trading logic.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs only once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Load daily S&P500 stock prices and returns
# load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# symbolv <- sort(colnames(pricestock))
# symbol1 <- "AAPL"
# 
# symbolsetf <- colnames(rutils::etfenv$prices)
# symbol2 <- "XLK"

symbolv <- rutils::etfenv$symbolv

symbol1 <- "XLK"
symbol2 <- "VTI"

# captiont <- paste("Stat-arb Portfolio Strategy app_statarb_strat.R")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Autoregressive Pair Strategy With Fixed Beta"),
  
  # create single row with four slider inputs
  fluidRow(
    # Input stock symbol
    column(width=1, selectInput("symbol1", label="Stock1", choices=symbolv, selected=symbol1)),
    # Input ETF symbol
    column(width=1, selectInput("symbol2", label="Stock2", choices=symbolv, selected=symbol2)),
    # Input beta parameter
    column(width=2, sliderInput("betac", label="beta:", min=0.1, max=2.0, value=1.0, step=0.01)),
    # Input ar1 parameter
    column(width=2, sliderInput("ar1", label="ar1:", min=-1.0, max=0.0, value=-1.0, step=0.01)),
    # Input ar2 parameter
    column(width=2, sliderInput("ar2", label="ar2:", min=-1.0, max=1.0, value=-1.0, step=0.01)),
    # Input lambda decay parameter
    # column(width=3, sliderInput("lambda", label="lambda:", min=0.01, max=0.99, value=0.5, step=0.01)),
    # Input threshold level
    # column(width=2, sliderInput("threshd", label="Threshold level", min=0.1, max=3.0, value=1.0, step=0.1)),
    # Input lag trade parameter
    # column(width=1, sliderInput("lagg", label="Confirmation", min=1, max=3, value=1, step=1)),
    # Input trending or reverting (contrarian) strategy
    # column(width=1, selectInput("coeff", label="Trend (1) Revert (-1)", choices=c(1, -1), selected=(-1)))
  ),  # end fluidRow
  
  # create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  # mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface


## Define the server code
servfun <- shiny::shinyServer(function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Rerun the strategy
  wealthv <- shiny::reactive({
    
    cat("Recalculating strategy", "\n")
    # Get model parameters from input argument
    symbol1 <- input$symbol1
    symbol2 <- input$symbol2
    # lagg <- input$lagg
    # coeff <- as.numeric(input$coeff)
    
    # retv <- na.omit(rutils::etfenv$returns[, c("XLK", "VTI")])
    retv <- na.omit(rutils::etfenv$returns[, c(symbol1, symbol2)])
    # retv <- na.omit(mget(c("XLK", "VTI"), as.environment(rutils::etfenv$returns)))
    # nrows <- nrow(retv)
    # retp <- retv$symbol1 - input$betac*retv$symbol2
    retp <- retv[, 1] - input$betac*retv[, 2]
    retl <- rutils::lagit(retp, lagg=1)
    retll <- rutils::lagit(retl, lagg=1)
    posv <- input$ar1*retl + input$ar2*retll
    
    # Calculate number of trades
    values$ntrades <- sum(abs(rutils::diffit(posv)) > 0)
    
    pnls <- posv*retp
    wealthv <- cbind(retp, pnls)
    colnames(wealthv) <- c("stock", "Strategy")
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    
    wealthv
    
  })  # end reactive code
  
  
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    
    # symbol1 <- "XLK"
    # symbol2 <- "VTI"
    wealthv <- wealthv()
    colnamev <- colnames(wealthv)
    
    captiont <- paste(paste0(paste(colnamev[1:2], "Sharpe =", values$sharper), collapse=" / "), "/ \n",
                      "Number of trades=", values$ntrades)

    endd <- rutils::calc_endpoints(wealthv, interval="weeks")
    dygraphs::dygraph(cumsum(wealthv), main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
      # dySeries(name=colnamev[3], axis="y", strokeWidth=2, col="green") %>%
      dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red") %>%
      dyLegend(show="always", width=500)
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
