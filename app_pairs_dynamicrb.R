##############################
# This is a shiny app for backtesting a stock versus ETF pairs strategy
# using a dynamic beta of prices and Bollinger trading logic.
# Uses separate lambda decay for beta.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Load daily S&P500 stock prices
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")

symbolstocks <- sort(colnames(prices))
symbolstock <- "AAPL"

symbolsetf <- sort(colnames(rutils::etfenv$prices))
symboletf <- "XLK"


## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Stock vs ETF Pairs Strategy Using a Dynamic Beta of Prices"),
  
  # create single row with four slider inputs
  fluidRow(
    # Input stock symbol
    column(width=1, selectInput("symbolstock", label="Stock", choices=symbolstocks, selected=symbolstock)),
    # Input ETF symbol
    column(width=1, selectInput("symboletf", label="ETF", choices=symbolsetf, selected=symboletf)),
    # Input lambda decay parameter
    column(width=2, sliderInput("lambda", label="lambda:", min=0.1, max=0.99, value=0.3, step=0.01)),
    # Input beta decay parameter
    column(width=2, sliderInput("lambdab", label="lambda beta:", min=0.1, max=0.99, value=0.95, step=0.01)),
    # Input threshold level
    column(width=2, sliderInput("threshd", label="Threshold level", min=0.1, max=2.0, value=1.0, step=0.1)),
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
    
    cat("Recalculating prices", "\n")
    # Prepare prices
    pricev <- get(input$symbolstock, prices)
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
  
  
  # Recalculate the betas
  betav <- shiny::reactive({
    
    cat("Recalculating the betas", "\n")
    # Calculate the regression coefficients of Stock ~ ETF
    covars <- HighFreq::run_covar(pricev(), lambda=input$lambdab)
    # covars <- HighFreq::run_covar(retv(), lambda=input$lambdab)
    covars[1, ] <- 1
    covars[, 1]/covars[, 3]
    # rutils::lagit(covars[, 1]/covars[, 3])
    # rep(1, NROW(retv))
    
  })  # end reactive code
  
  
  # Recalculate the cointegrated portfolio prices
  pricec <- shiny::reactive({
    
    cat("Recalculating the residuals", "\n")
    # Recalculate the cointegrated portfolio prices
    pricev <- pricev()
    (pricev$Stock - betav()*pricev$ETF)

  })  # end reactive code
  
  
  # Recalculate the trailing means of residuals
  meanv <- shiny::reactive({
    
    cat("Recalculating the trailing means", "\n")
    # Recalculate the means of cointegrated portfolio prices
    meanv <- HighFreq::run_mean(pricec(), lambda=input$lambda)
    
  })  # end reactive code
  
  
  # Recalculate the trailing variance of residuals
  vars <- shiny::reactive({
    
    cat("Recalculating the trailing variance", "\n")
    # Recalculate the trailing variance of residuals
    sqrt(HighFreq::run_var(pricec(), lambda=input$lambda))
    
  })  # end reactive code
  
  
  # Rerun the strategy
  wealthv <- shiny::reactive({
    cat("Recalculating strategy", "\n")
    # Get model parameters from input argument
    symbolstock <- input$symbolstock
    symboletf <- input$symboletf
    # lambda <- input$lambda
    lagg <- input$lagg
    coeff <- as.numeric(input$coeff)
    
    # Prepare data
    # pricev <- pricev()
    # Recalculate the cointegrated portfolio prices in excess of its mean
    pricec <- pricec() - meanv()
    retv <- retv()
    # betav <- betav()
    nrows <- NROW(pricev)

    threshd <- input$threshd*rutils::lagit(vars())
    # threshd <- input$threshd*vars()
    
    # Calculate indicator
    indic <- integer(nrows)
    indic <- ifelse(pricec > threshd, coeff, indic)
    indic <- ifelse(pricec < -threshd, -coeff, indic)
    indic <- roll::roll_sum(indic, width=lagg, min_obs=1)
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
    
    pnls <- posv*(retv$Stock - betav()*retv$ETF)
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
    
    captiont <- paste("Strategy for", symbolstock, "vs", symboletf, "/ \n", 
                      paste0(c("Stock SR=", "Strategy SR="), values$sharper, collapse=" / "), "/ \n",
                      "Number of trades=", values$ntrades)
    
    endd <- rutils::calc_endpoints(wealthv, interval="weeks")
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
