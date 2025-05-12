##############################
# This is a shiny app for simulating a binary Bollinger strategy
# using the price z-scores as trading signals.
# The binary Bollinger strategy switches between 1 share long stock 
# position and 1 share short, depending on the z-score.
# If the z-score is greater than the threshold, then it sells 1 share 
# of stock, and if the z-score is less than minus the threshold, 
# then it buys 1 share of stock.  It holds its position until the 
# opposite trading signal.
# The price z-scores are equal to the excess prices (current price 
# minus trailing mean price) divided by the trailing volatility of 
# prices.  
# 
# Just press the "Run App" button on the upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Load daily ETF prices

## Calculate the daily VTI prices
# pricev <- log(na.omit(rutils::etfenv$prices$VTI))
# dataf <- "Days"

## Calculate the intraday VTI returns
ohlc <- (rutils::etfenv$VTI)
openp <- quantmod::Op(ohlc)
# highp <- quantmod::Hi(ohlc)
# lowp <- quantmod::Lo(ohlc)
closep <- quantmod::Cl(ohlc)
retd <- (closep - openp)
# colnames(retd) <- "intraday"
# reton <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))
# colnames(reton) <- "overnight"
pricev <- cumsum(retd)
dataf <- "Intraday"


## Load intraday tick stock prices

# Load SPY OHLC minute bars
# pricev <- quantmod::Cl(HighFreq::SPY)
# load(file="/Users/jerzy/Develop/data/SPY_minute_202306.RData")
# load(file="/Users/jerzy/Develop/data/SPY_minute_bars_2019_2023.RData")
# pricev <- quantmod::Cl(ohlc)
# colnames(pricev) <- "SPY"
# dataf <- "Minutes"
# load(file="/Users/jerzy/Develop/data/QQQ_bigticks_202305.RData")
# load(file="/Users/jerzy/Develop/data/SPY_second_202306.RData")
# load(file="/Users/jerzy/Develop/data/SPY_second_2023.RData")
# dataf <- "Seconds"
# load(file="/Users/jerzy/Develop/data/SPY_5second_202306.RData")
# dataf <- "5-Seconds"
# load(file="/Users/jerzy/Develop/data/SPY_10second_202306.RData")
# load(file="/Users/jerzy/Develop/data/SPY_10second_2023.RData")
# dataf <- "10-Seconds"

# Subset prices to market hours only
# pricev <- pricev[, 1]
# pricev <- pricev["T09:30:00/T16:00:00"]
# pricev <- pricev["T09:30:00/T10:30:00"]
# pricev <- pricev["T10:00:00/T16:00:00"]

symboln <- rutils::get_name(colnames(pricev))
nrows <- NROW(pricev)

# Calculate the returns
retp <- rutils::diffit(pricev)


# Calculate the end points for the start of every day
# datev <- .index(retp)
# datev <- rutils::diffit(datev)
# datev <- which(datev > 1000)
# Set large overnight returns to zero
# retp[abs(retp) > 10*sd(retp)] <- 0
# retp[abs(retp) > 3*sd(retp)] <- 0
# retp[datev] <- 0
# pricev <- cumsum(retp)

captiont <- paste("Bollinger Band Strategy For", symboln, dataf)

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  # create single row with four slider inputs
  fluidRow(
    # Input stock symbol
    # column(width=1, selectInput("symboln", label="Stock", choices=symbolns, selected=symboln)),
    # Input ETF symbol
    # column(width=1, selectInput("symboletf", label="ETF", choices=symbolsetf, selected=symboletf)),
    # Input lambda decay parameter
    # column(width=2, sliderInput("lambda", label="lambda:", min=0.01, max=0.99, value=0.99, step=0.01)),
    column(width=2, sliderInput("lambdaf", label="lambda:", min=0.1, max=0.99, value=0.2, step=0.01)),
    # Input beta decay parameter
    # column(width=2, sliderInput("lambdab", label="lambda beta:", min=0.1, max=0.99, value=0.3, step=0.01)),
    # Input z-score threshold level
    column(width=2, sliderInput("threshz", label="z-score threshold", min=0.1, max=2.0, value=1.0, step=0.1)),
    # Input volatility threshold level
    # column(width=2, sliderInput("threshv", label="volatility threshold", min=0.0, max=0.5, value=0.0, step=0.01)),
    # Input look-back parameter
    # column(width=2, sliderInput("lookb", label="Look-back", min=3, max=11, value=11, step=1)),
    # Input lag trade parameter
    column(width=1, sliderInput("lagg", label="lag", min=1, max=3, value=1, step=1)),
    # Input the bid-ask spread
    column(width=1, numericInput("bidask", label="Bid-ask:", value=0.01, step=0.01)),
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
  
  # Recalculate the trailing volatility
  # volt <- shiny::reactive({
  #   
  #   cat("Recalculating the trailing volatility", "\n")
  #   # sqrt(HighFreq::run_var(retp, lambda=lambdaf))
  #   volv <- sqrt(HighFreq::roll_var(retp, lookb=input$lookb))
  #   (volv >= input$threshv)
  #   
  # })  # end reactive code
  
  
  # Recalculate the trailing price z-scores
  zscorev <- shiny::reactive({
    
    cat("Recalculating the trailing price z-scores", "\n")
    lambdaf <- input$lambdaf

    ## Calculate the stochastic oscillator - doesn't perform any better than the price z-scores
    # Calculate the trailing maximums and minimums
    # pricmax <- HighFreq::run_max(pricev, lambda=lambdaf)
    # pricmin <- HighFreq::run_min(pricev, lambda=lambdaf)
    # Calculate the stochastic oscillator between -0.5 and +0.5
    # zscorev <- (pricev - pricmin)
    # volv <- (pricmax - pricmin)
    # zscorev <- ifelse(volv > 0.01, zscorev/volv, zscorev/0.01)
    # zscorev - 0.5
    
    varv <- rutils::lagit(HighFreq::run_var(pricev, lambda=lambdaf))
    zscorev <- (pricev - varv[, 1])
    volv <- sqrt(varv[, 2])
    zscorev <- ifelse(volv > 0, zscorev/volv, 0)
    zscorev
    # volt()*zscorev
    
  })  # end reactive code
  
  
  # Rerun the strategy
  wealthv <- shiny::reactive({
    cat("Recalculating strategy", "\n")
    # Get model parameters from input argument
    lagg <- input$lagg
    coeff <- as.numeric(input$coeff)
    threshz <- input$threshz
    
    # Calculate the prices in excess of the trailing mean
    zscorev <- zscorev()
    
    # Calculate the indicator from Bollinger bands
    indic <- integer(nrows)
    indic <- ifelse(zscorev > threshz, coeff, indic)
    indic <- ifelse(zscorev < -threshz, -coeff, indic)
    if (lagg > 1)
      indic <- HighFreq::roll_sum(matrix(indic), lagg)
    # Calculate positions from indicator
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    posv <- ifelse(indic == lagg, 1, posv)
    posv <- ifelse(indic == (-lagg), -1, posv)
    posv <- zoo::na.locf(posv)
    
    # posv <- volt()*posv
    # Lag the positions to trade in next period
    posv <- rutils::lagit(posv, lagg=1)
    
    # Calculate number of trades
    tradez <- abs(rutils::diffit(posv))
    values$ntrades <- sum(tradez > 0)

    # Calculate the PnLs
    # reti <- (retp$Stock - rutils::lagit(betas)*retp$ETF)
    pnls <- retp*posv
    
    # Calculate the transaction costs
    costs <- 0.5*input$bidask*tradez
    pnls <- (pnls - costs)
    
    wealthv <- cbind(retp, pnls)
    colnames(wealthv) <- c(symboln, "Strategy")
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    
    wealthv
    
  })  # end reactive code
  
  
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    
    # symboln <- input$symboln
    # symboletf <- input$symboletf
    wealthv <- wealthv()
    colnamev <- colnames(wealthv)
    
    captiont <- paste(paste0(paste(colnamev[1:2], "Sharpe =", values$sharper), collapse=" / "), "/ \n",
                      "Number of trades=", values$ntrades)
    
    # endd <- rutils::calc_endpoints(wealthv, interval="days")
    endd <- rutils::calc_endpoints(wealthv, interval="minutes")
    dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
    # dygraphs::dygraph(cumsum(wealthv), main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
      dyLegend(show="always", width=300)
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
