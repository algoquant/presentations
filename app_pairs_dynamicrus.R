##############################
# This is a shiny app for backtesting a single stock strategy
# using a dynamic beta.
# Trades single stock without limits - adds to positions as long as signal persists.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs only once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Load daily S&P500 stock prices
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
symbolstocks <- sort(colnames(prices))
symbolstock <- "AAPL"

symbolsetf <- colnames(rutils::etfenv$prices)
symboletf <- "XLK"


# captiont <- paste("Stat-arb Portfolio Strategy app_statarb_strat.R")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Stock vs ETF Pairs Strategy Using a Dynamic Beta"),
  
  # create single row with four slider inputs
  fluidRow(
    # Input stock symbol
    column(width=1, selectInput("symbolstock", label="Stock", choices=symbolstocks, selected=symbolstock)),
    # Input ETF symbol
    column(width=1, selectInput("symboletf", label="ETF", choices=symbolsetf, selected=symboletf)),
    # Input beta parameter
    column(width=3, sliderInput("beta", label="beta:", min=0.1, max=3.0, value=1.0, step=0.1)),
    # Input lambda decay parameter
    column(width=3, sliderInput("lambda", label="lambda:", min=0.01, max=0.99, value=0.3, step=0.01)),
    # Input threshold parameter
    column(width=2, sliderInput("threshold", label="Threshold", min=0.1, max=3.0, value=1.0, step=0.1)),
    # Input lag trade parameter
    column(width=1, sliderInput("lagg", label="Lag", min=1, max=3, value=1, step=1)),
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
  
  # Prepare the prices
  pricev <- shiny::reactive({
    
    cat("Preparing the prices", "\n")
    # Get model parameters from input argument
    symbolstock <- input$symbolstock
    # symboletf <- input$symboletf

    # Prepare the prices
    pricev <- get(symbolstock, prices)
    # pricetf <- get(symboletf, rutils::etfenv$prices)
    pricev <- na.omit(pricev)
    pricev
    
  })  # end reactive code
  
  # Prepare the prices
  retv <- shiny::reactive({
    
    cat("Preparing the returns", "\n")
    rutils::diffit(log(pricev()))

  })  # end reactive code
  
  
  # Recalculate the residuals
  resids <- shiny::reactive({
    
    cat("Recalculating the residuals", "\n")
    # Get model parameters from input argument
    lambda <- input$lambda
    pricev <- pricev()

    # Recalculate the residuals
    meanv <- HighFreq::run_mean(pricev, lambda=lambda)
    resids <- (pricev - meanv)
    colnames(resids) <- "resids"
    # resids - min(resids) + 1
    resids
    
  })  # end reactive code
  
  
  # Recalculate the trailing means of residuals
  meanv <- shiny::reactive({
    
    cat("Recalculating the trailing means", "\n")
    # Get model parameters from input argument
    lambda <- input$lambda

    # Recalculate the trailing means of residuals
    HighFreq::run_mean(resids(), lambda=lambda)

  })  # end reactive code
  
  
  # Recalculate the trailing variance of residuals
  vars <- shiny::reactive({
    
    cat("Recalculating the trailing variance", "\n")
    # Get model parameters from input argument
    lambda <- input$lambda

    # Recalculate the trailing variance of residuals
    HighFreq::run_var(resids(), lambda=lambda)

  })  # end reactive code
  
  
  # Rerun the strategy
  wealthv <- shiny::reactive({
    
    cat("Recalculating strategy", "\n")
    # Get model parameters from input argument
    symbolstock <- input$symbolstock
    symboletf <- input$symboletf
    lagg <- input$lagg
    threshold <- input$threshold
    coeff <- as.numeric(input$coeff)
    
    pricev <- pricev()
    retv <- retv()
    nrows <- NROW(pricev)
    resids <- resids()
    meanv <- meanv()
    vars <- vars()
    # datev <- zoo::index(pricev)
    # meanv <- HighFreq::lagit(meanv)
    # threshd <- threshold*HighFreq::lagit(sqrt(vars))
    threshd <- threshold*sqrt(vars)
    
    indicv <- integer(nrows)
    indicv <- ifelse(resids < (meanv - threshd), 1, indicv)
    indicv <- ifelse(resids > (meanv + threshd), -1, indicv)
    indicv <- rutils::lagit(indicv)
    posits <- cumsum(indicv)
    pnls <- posits*retv
    
    
    # costb <- -cumsum(indicv*(pricev[, 1] - betas*pricev[, 2]))
    # indicv <- rutils::lagit(indicv)
    # posits <- cumsum(indicv)
    # positetf <- -cumsum(indicv*betas)
    # pv <- (posits*pricev[, 1] + positetf*pricev[, 2])
    # npv <- (pv - costb)
    # pnls <- rutils::diffit(npv)
    
    # Rescale strategy cashflows to volatility of ETF
    retetf <- log(pricev[, 1])
    pnls <- pnls*sd(retetf)/sd(pnls)
    pnls <- cumsum(pnls)
    # plot(pnls, t="l")
    
    values$ntrades <- sum(abs(indicv))
    
    wealthv <- cbind(retetf, pnls)
    colnames(wealthv) <- c(symbolstock, "Strategy")

    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(rutils::diffit(wealthv), function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    
    # wealthv <- cbind(retetf, pnls)
    # colnames(wealthv) <- c(symbolstock, "Strategy")
    # 
    wealthv
    
  })  # end reactive code
  
  
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    
    symbolstock <- input$symbolstock
    symboletf <- input$symboletf
    wealthv <- wealthv()
    colnamev <- colnames(wealthv)
    
    captiont <- paste("Strategy for", symbolstock, "vs", symboletf, "/ \n", 
                      paste0(c("Index SR=", "Strategy SR="), values$sharper, collapse=" / "), "/ \n",
                      "Number of trades=", values$ntrades)
    
    endd <- rutils::calc_endpoints(wealthv, interval="weeks")
    dygraphs::dygraph(wealthv[endd], main=captiont) %>%
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
