##############################
# This is a shiny app for backtesting a Bollinger band strategy.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Load daily S&P500 stock prices
# load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# symbolstocks <- sort(colnames(prices))
# symbolstock <- "AAPL"

## Load daily ETF prices
symbolstocks <- sort(colnames(rutils::etfenv$prices))
symbolstock <- "VTI"


## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Bollinger Band Strategy"),
  
  # create single row with four slider inputs
  fluidRow(
    # Input stock symbol
    column(width=1, selectInput("symbolstock", label="Stock", choices=symbolstocks, selected=symbolstock)),
    # Input ETF symbol
    # column(width=1, selectInput("symboletf", label="ETF", choices=symbolsetf, selected=symboletf)),
    # Input lambda decay parameter
    column(width=2, sliderInput("lambda", label="lambda:", min=0.01, max=0.99, value=0.1, step=0.01)),
    # Input beta decay parameter
    # column(width=2, sliderInput("lambdab", label="lambda beta:", min=0.1, max=0.99, value=0.3, step=0.01)),
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
    
    cat("Recalculating prices", "\n")
    # Get model parameters from input argument
    symbolstock <- input$symbolstock
    # symboletf <- input$symboletf
    lambda <- input$lambda
    
    # Prepare prices
    # pricev <- log(na.omit(get(symbolstock, prices)))
    pricev <- log(na.omit(get(symbolstock, rutils::etfenv$prices)))
    # pricev <- log(quantmod::Cl(HighFreq::SPY))
    # pricev <- log(quantmod::Cl(spyohlc))
    # pricev <- log(na.omit(cbind(pricev, pricetf)))
    # colnames(pricev) <- c("Stock", "ETF")
    pricev
    
  })  # end reactive code

  
  # Prepare the returns
  retv <- shiny::reactive({
    
    cat("Preparing the returns", "\n")
    retv <- rutils::diffit(pricev())
    retv[1, ] <- retv[2, ]
    # colnames(retv) <- c("Stock", "ETF")
    retv
    
  })  # end reactive code
  

  # Recalculate the trailing variance of prices
  vars <- shiny::reactive({
    
    cat("Recalculating the trailing variance", "\n")
    # Get model parameters from input argument
    lambda <- input$lambda
    
    # Recalculate the trailing variance of residuals
    sqrt(HighFreq::run_var(pricev(), lambda=lambda))
    
  })  # end reactive code
  
  
  # Rerun the strategy
  wealthv <- shiny::reactive({
    cat("Recalculating strategy", "\n")
    # Get model parameters from input argument
    symbolstock <- input$symbolstock
    # symboletf <- input$symboletf
    # lambda <- input$lambda
    lagg <- input$lagg
    # threshd <- input$threshd
    coeff <- as.numeric(input$coeff)
    
    # Prepare data
    pricev <- pricev()
    # pricec <- pricec()
    retv <- retv()
    # betas <- betas()
    nrows <- NROW(pricev)
    
    threshd <- input$threshd*rutils::lagit(vars())
    
    # threshd <- input$threshd*vars()
    
    # Calculate the prices in excess of the trailing mean
    meanv <- HighFreq::run_mean(pricev, lambda=input$lambda)
    pricem <- zoo::coredata(pricev - meanv)
    
    # Calculate the indicator from Bollinger bands
    indic <- integer(nrows)
    indic <- ifelse(pricem > threshd, coeff, indic)
    indic <- ifelse(pricem < -threshd, -coeff, indic)
    indic <- HighFreq::roll_sum(indic, lagg)
    # Calculate positions from indicator
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    posv <- ifelse(indic == lagg, 1, posv)
    posv <- ifelse(indic == (-lagg), -1, posv)
    posv <- zoo::na.locf(posv)
    # Lag the positions to trade in next period
    posv <- rutils::lagit(posv, lagg=1)
    
    # Alternative code
    # posv <- integer(nrows) # Risk positions
    # posv[1] <- 0 # Initial position
    # # Calculate the positions from Bollinger bands
    # for (it in 2:nrows) {
    #   if (pricem[it-1] > threshd[it-1]) {
    #     # Enter short
    #     posv[it] <- (-1)
    #   } else if (pricem[it-1] < (-threshd[it-1])) {
    #     # Enter long
    #     posv[it] <- 1
    #   } else if ((posv[it-1] < 0) && (pricem[it-1] < 0)) {
    #     # Unwind short
    #     posv[it] <- 0
    #   } else if ((posv[it-1] > 0) && (pricem[it-1] > 0)) {
    #     # Unwind long
    #     posv[it] <- 0
    #   } else {
    #     # Do nothing
    #     posv[it] <- posv[it-1]
    #   }  # end if
    # }  # end for
    
    # Calculate number of trades
    values$ntrades <- sum(abs(rutils::diffit(posv)) > 0)
    
    # Calculate the PnLs
    # reti <- (retv$Stock - rutils::lagit(betas)*retv$ETF)
    pnls <- retv*posv
    wealthv <- cbind(retv, pnls)
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
      dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
      dyLegend(show="always", width=200)
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
