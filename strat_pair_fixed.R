##############################
# This is a shiny app for backtesting a stock versus ETF pairs strategy
# using a fixed beta of returns and Bollinger trading logic.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs only once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Load daily S&P500 stock prices and returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
symbolstocks <- sort(colnames(pricestock))
symbolstock <- "AAPL"

symbolsetf <- colnames(rutils::etfenv$prices)
symboletf <- "XLK"


# captiont <- paste("Stat-arb Portfolio Strategy app_statarb_strat.R")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Stock vs ETF Pairs Strategy Using a Fixed Beta"),
  
  # create single row with four slider inputs
  fluidRow(
    # Input stock symbol
    column(width=1, selectInput("symbolstock", label="Stock", choices=symbolstocks, selected=symbolstock)),
    # Input ETF symbol
    column(width=1, selectInput("symboletf", label="ETF", choices=symbolsetf, selected=symboletf)),
    # Input beta parameter
    column(width=3, sliderInput("betac", label="beta:", min=0.1, max=3.0, value=1.0, step=0.1)),
    # Input lambda decay parameter
    column(width=3, sliderInput("lambda", label="lambda:", min=0.01, max=0.99, value=0.5, step=0.01)),
    # Input threshold level
    column(width=2, sliderInput("threshd", label="Threshold level", min=0.1, max=3.0, value=1.0, step=0.1)),
    # Input lag trade parameter
    column(width=1, sliderInput("lagg", label="Confirmation", min=1, max=3, value=1, step=1)),
    # Input trending or reverting (contrarian) strategy
    column(width=1, selectInput("coeff", label="Trend (1) Revert (-1)", choices=c(1, -1), selected=(-1)))
  ),  # end fluidRow
  
  # create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  # mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface


## Define the server code
servfun <- shiny::shinyServer(function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Recalculate the prices
  pricev <- shiny::reactive({
    
    cat("Recalculating the prices", "\n")
    
    # Recalculate the prices
    pricev <- get(input$symbolstock, pricestock)
    pricetf <- get(input$symboletf, rutils::etfenv$prices)
    pricev <- log(na.omit(cbind(pricev, pricetf)))
    colnames(pricev) <- c("Stock", "ETF")
    pricev

  })  # end reactive code
  
  
  # Recalculate the returns
  retv <- shiny::reactive({
    
    cat("Recalculating the returns", "\n")
    rutils::diffit(pricev())
    
  })  # end reactive code
  
  
  # Recalculate the residuals
  resids <- shiny::reactive({
    
    cat("Recalculating the residuals", "\n")
    pricev <- pricev()
    
    # Get model parameters from input argument
    (pricev$Stock - input$betac*pricev$ETF)

  })  # end reactive code
  
  
  # Recalculate the trailing means of residuals
  meanv <- shiny::reactive({
    
    cat("Recalculating the trailing means", "\n")
    # Recalculate the trailing means of residuals
    HighFreq::run_mean(resids(), lambda=input$lambda)

  })  # end reactive code
  
  
  # Recalculate the trailing standard deviation of residuals
  vars <- shiny::reactive({
    
    cat("Recalculating the trailing standard deviation", "\n")
    # Recalculate the trailing variance of residuals
    sqrt(HighFreq::run_var(resids(), lambda=input$lambda))
    
  })  # end reactive code
  
  
  # Rerun the strategy
  wealthv <- shiny::reactive({
    
    cat("Recalculating strategy", "\n")
    # Get model parameters from input argument
    symbolstock <- input$symbolstock
    symboletf <- input$symboletf
    lagg <- input$lagg
    coeff <- as.numeric(input$coeff)
    
    residm <- resids() - meanv()
    retv <- retv()
    nrows <- NROW(retv)
    threshd <- input$threshd*rutils::lagit(vars())

    # Calculate indicator
    indic <- integer(nrows)
    indic <- ifelse(residm < (-threshd), -coeff, indic)
    indic <- ifelse(residm > threshd, coeff, indic)
    indic <- HighFreq::roll_sum(indic, lagg)
    # Calculate positions from indicator
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    posv <- ifelse(indic == lagg, 1, posv)
    posv <- ifelse(indic == (-lagg), -1, posv)
    posv <- zoo::na.locf(posv)
    # Lag the positions to trade in next period
    posv <- rutils::lagit(posv, lagg=1)
    
    # Calculate number of trades
    values$ntrades <- sum(abs(rutils::diffit(posv)) > 0)
    
    pnls <-  posv*(retv$Stock - input$betac*retv$ETF)
    wealthv <- cbind(retv$Stock, pnls)
    colnames(wealthv) <- c(symbolstock, "Strategy")
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    
    wealthv
    
  })  # end reactive code
  
  
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    
    symbolstock <- input$symbolstock
    symboletf <- input$symboletf
    wealthv <- wealthv()
    colnamev <- colnames(wealthv)
    
    captiont <- paste(paste0(paste(colnamev[1:2], "Sharpe =", values$sharper), collapse=" / "), "/ \n",
                      "Number of trades=", values$ntrades)

    endd <- rutils::calc_endpoints(wealthv, interval="weeks")
    dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
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
