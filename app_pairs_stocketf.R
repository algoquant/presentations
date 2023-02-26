##############################
# This is a shiny app for backtesting a stock versus ETF pairs strategy
# using a fixed beta of prices.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs only once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Load S&P500 prices
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")

symbolstocks <- sort(colnames(prices))
symbolstock <- "AAPL"

symbolsetf <- colnames(rutils::etfenv$prices)
symboletf <- "XLK"


## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Stock vs ETF Pairs Strategy Using a Fixed Beta of Prices"),
  
  # create single row with four slider inputs
  fluidRow(
    # Input stock symbol
    column(width=1, selectInput("symbolstock", label="Stock", choices=symbolstocks, selected=symbolstock)),
    # Input ETF symbol
    column(width=1, selectInput("symboletf", label="ETF", choices=symbolsetf, selected=symboletf)),
    # Input lambda decay parameter
    column(width=3, sliderInput("lambda", label="lambda:", min=0.01, max=0.99, value=0.1, step=0.01)),
    # Input threshold parameter
    column(width=2, sliderInput("threshold", label="Threshold", min=0.1, max=3.0, value=1.5, step=0.1)),
    # Input lag trade parameter
    column(width=1, sliderInput("lagg", label="Confirmation", min=1, max=3, value=2, step=1)),
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
  
  # Recalculate the data
  datav <- shiny::reactive({
    cat("Recalculating data", "\n")
    # Get model parameters from input argument
    symbolstock <- input$symbolstock
    symboletf <- input$symboletf

    # Prepare data
    closep <- get(symbolstock, prices)
    closetf <- get(symboletf, rutils::etfenv$prices)
    closep <- log(na.omit(cbind(closep, closetf)))
    colnames(closep) <- c("Stock", "ETF")
    # Calculate the regression coefficients of Stock ~ ETF
    betav <- drop(cov(closep$Stock, closep$ETF)/var(closep$ETF))
    # Calculate the regression residuals
    datav <- cbind(closep, (closep$Stock - betav*closep$ETF))
    colnames(datav)[3] <- "residv"
    datav
  })  # end reactive code
  
  
  # Rerun the strategy
  wealthv <- shiny::reactive({
    cat("Recalculating strategy", "\n")
    # Get model parameters from input argument
    symbolstock <- input$symbolstock
    symboletf <- input$symboletf
    lambda <- input$lambda
    lagg <- input$lagg
    threshold <- input$threshold
    coeff <- as.numeric(input$coeff)
    
    # Prepare data
    datav <- datav()
    nrows <- NROW(datav)
    datev <- zoo::index(datav)
    
    retp <- rutils::diffit(datav$ETF)
    residv <- datav$residv
    retsres <- rutils::diffit(residv)
    vars <- HighFreq::run_var(retsres, lambda=lambda)

    resids <- HighFreq::run_mean(retsres, lambda=lambda)
    resids <- resids/sqrt(vars)
    resids[1] <- 0
    # resids <- xts::xts(resids, datev)
    
    # Calculate indicator
    indic <- integer(nrows)
    indic <- ifelse(resids < -threshold, 1, indic)
    indic <- ifelse(resids > threshold, -1, indic)
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
    # Calculate positions from indicator
    posit <- rep(NA_integer_, nrows)
    posit[1] <- 0
    posit <- ifelse(indic == lagg, 1, posit)
    posit <- ifelse(indic == (-lagg), -1, posit)
    posit <- zoo::na.locf(posit)
    # Lag the positions to trade in next period
    posit <- rutils::lagit(posit, lagg=1)
    
    # Calculate number of trades
    values$ntrades <- sum(abs(rutils::diffit(posit)) > 0)
    
    pnls <- retsres*posit
    wealthv <- cbind(retp, pnls)
    colnames(wealthv) <- c(symboletf, "Strategy")
    
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
    
    captiont <- paste("Strategy for", symbolstock, "vs", symboletf, "/ \n", 
                      paste0(c("Index SR=", "Strategy SR="), values$sharper, collapse=" / "), "/ \n",
                      "Number of trades=", values$ntrades)
    
    endd <- rutils::calc_endpoints(wealthv, interval="months")
    dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
      # dySeries(name=colnamev[3], axis="y", label=colnamev[3], strokeWidth=2, col="green") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
      dyLegend(show="always", width=500)
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
