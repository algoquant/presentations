##############################
# This is a shiny app for simulating a contrarian (mean reverting) 
# intraday strategy using z-scores and thresholds. 
# Buy stock if the z-score is less than minus the threshold.
# Sell short stock if the z-score is greater than the threshold.
# The z-score is equal to the current price minus the EMA price, 
# divided by the trailing volatility of prices.
# The daily simulations are performed in Rcpp.
#
# Just press the "Run App" button on the upper right of the RStudio panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

# Compile this file in R by running this command:
# Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/back_test.cpp")

# Load the price data - pricel is a list of daily prices.
load("/Users/jerzy/Develop/data/SPY_second_202311.RData")
# load("/Users/jerzy/Develop/data/XLK_second_202311.RData")
# load("/Users/jerzy/Develop/data/AAPL_second_202311.RData")
# load("/Users/jerzy/Develop/data/SPY_second_202310.RData")
# load("/Users/jerzy/Develop/data/SPY_minute_202311.RData")
# pricev <- pricel[[17]]["T09:30:00/T10:30:00"][, 1]
# retv <- rutils::diffit(pricev)

# pricel <- lapply(pricel, function(pricev) {
#   pricev <- pricev["T09:30:00/T16:00:00"][, 1]
#   retv <- rutils::diffit(pricev)
#   retv[abs(retv) > 0.1] <- 0
#   cumsum(retv)
# }) # end lapply

symboln <- colnames(pricel[[1]][, 1])
captiont <- paste("Mean Reverting Strategy For", symboln, "Using Z-Scores")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbol
    # column(width=2, selectInput("symbol", label="Symbol", choices=rutils::etfenv$symbolv, selected="VTI")),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-ask spread
    column(width=2, numericInput("bidask", label="Bid-ask:", value=0.02, step=0.01)),
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=4, value=2, step=1))
  ),  # end fluidRow

  fluidRow(
    # Input the EMA decay factor and z-scores thresholds
    column(width=2, sliderInput("lambdaf", label="Lambda decay:", min=0.95, max=0.999, value=0.98, step=0.001)),
    column(width=2, sliderInput("threshv", label="Threshold value:", min=0.5, max=5.0, value=1.0, step=0.1)),
    column(width=2, sliderInput("threshd", label="Threshold double:", min=1.0, max=5.0, value=2.0, step=0.1)),
    column(width=2, sliderInput("threshbad", label="Threshold bad:", min=2.0, max=6.0, value=2.0, step=0.1)),
    column(width=2, sliderInput("varin", label="Initial variance:", min=1.0, max=10.0, value=1.0, step=0.1))
    # column(width=2, sliderInput("lambdas", label="Slow lambda:", min=0.8, max=0.999, value=0.99, step=0.001)),
    # Input the EMA loadings
    # column(width=2, sliderInput("loadf", label="Fast load:", min=(-1.0), max=1.0, value=(-1.0), step=0.1)),
    # column(width=2, sliderInput("loads", label="Slow load:", min=(-1.0), max=1.0, value=(0.0), step=0.1)),
    # Input the trade lag
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="550px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()

  # Load the closing prices
  # closep <- shiny::reactive({
  #   
  #   symbol <- input$symbol
  #   cat("Loading data for ", symbol, "\n")
  #   
  #   ohlc <- get(symbol, rutils::etfenv)
  #   # quantmod::Cl(ohlc["2010/2019"])
  #   quantmod::Cl(ohlc)
  #   
  # })  # end Load the closing prices
  
  # Load the log returns
  # retv <- shiny::reactive({
  #   
  #   cat("Recalculating returns for ", input$symbol, "\n")
  #   
  #   rutils::diffit(log(closep()))
  # 
  # })  # end Load the log returns
  

  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    cat("Recalculating strategy for ", input$symbol, "\n")
    # Get model parameters from input argument
    # closep <- closep()
    # threshv <- input$threshv
    # varin <- input$varin
    # lambdaf <- input$lambdaf
    # lambdas <- input$lambdas
    # loadf <- input$loadf
    # loads <- input$loads
    # look_back <- input$look_back
    bidask <- input$bidask
    lagg <- input$lagg

    # Calculate cumulative returns
    # retv <- retv()
    # retc <- cumsum(retv)
    # nrows <- NROW(pricev)

    # Calculate EMA prices
    # EMAf <- HighFreq::run_mean(retv, lambda=lambdaf)
    # volf <- sqrt(HighFreq::run_var(retv, lambda=lambdaf))
    # volf[1:7, ] <- 1.0
    # EMAs <- HighFreq::run_mean(retv, lambda=lambdas)
    # vols <- sqrt(HighFreq::run_var(retv, lambda=lambdas))
    # vols[1:7, ] <- 1.0
    
    # Determine dates when the EMAs have crossed
    # crossi <- sign(EMAf - EMAs)
    
    # Calculate cumulative sum of EMA crossing indicator
    # crossc <- HighFreq::roll_sum(tseries=crossi, look_back=lagg)
    # crossc[1:lagg] <- 0
    # Calculate the positions
    # Flip position only if the crossi and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # posv <- (loadf*EMAf + loads*EMAs)
    
    # Calculate the positions of the mean-reversion strategy
    # stratm <- bollinger_strat(pricev, input$lambdaf, input$threshv, input$varin)
    # posv <- stratm[, 2]
    
    ntrades <- 0
    pnls <- lapply(pricel, function(pricev) {
      # Calculate EMA prices
      # retv[abs(retv) > 0.1] <- 0
      pricev <- pricev[, 1]["T09:30:00/T16:00:00"]
      stratm <- contrastrat(pricev, input$lambdaf, input$threshv, input$threshd, input$threshbad, input$varin)
      # Calculate strategy pnls
      # pnls <- posv*retv
      pnls <- stratm[, 1]
      posv <- stratm[, 2]
      # cat("posv =", tail(posv), "\n")
      retv <- rutils::diffit(pricev)
      # retv <- stratm[, 4]
      # Calculate indicator of flipped positions
      flipi <- rutils::diffit(posv)
      # Calculate the number of trades
      ntrades <<- ntrades + sum(abs(flipi) > 0)
      # Calculate transaction costs
      costv <- 0.5*bidask*abs(flipi)
      pnls <- (pnls - costv)
      pnls <- cbind(retv, pnls)
      # pnls <- xts::xts(pnls, zoo::index(pricev))
      pnls
    }) # end lapply
    pnls <- do.call(rbind, pnls)
    
    
    # Calculate indicator of flipped positions
    # flipi <- rutils::diffit(posv)
    # Calculate the number of trades
    # values$ntrades <- sum(abs(flipi) > 0)
    values$ntrades <- ntrades
    
    # Add buy/sell indicators for annotations
    # longi <- (flipi > 0)
    # shorti <- (flipi < 0)
    
    # Lag the positions to trade in next period
    # posv <- rutils::lagit(posv, lagg=1)
    
    # Calculate strategy pnls
    # pnls <- posv*retv
    
    # Calculate transaction costs
    # costs <- 0.5*input$bidask*abs(flipi)
    # pnls <- (pnls - costs)

    # Scale the pnls so they have same SD as the returns
    # pnls <- pnls*sd(retv[retv<0])/sd(pnls[pnls<0])
    
    # Bind together strategy pnls
    # pnls <- cbind(retv, pnls)
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)

    # Bind strategy pnls with indicators
    pnls <- cumsum(pnls)
    # pnls <- cbind(pnls, retv[longi], retv[shorti])
    # colnames(pnls) <- c(paste(input$symbol, "Returns"), "Strategy", "Buy", "Sell")
    colnames(pnls) <- c(symboln, "Strategy")
    
    pnls

  })  # end recalculate the strategy
  

  # Plot the cumulative strategy pnls
  output$dyplot <- dygraphs::renderDygraph({
    
    # Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Get Sharpe ratios
    sharper <- values$sharper
    # Get number of trades
    ntrades <- values$ntrades
    
    captiont <- paste(paste0(c(paste(symboln, "SR="), "Strategy SR="), sharper, collapse=" / "), "/ \n",
                      "Number of trades=", ntrades, "/ \n",
                      "PnL per trade=", round(pnls[NROW(pnls), colnamev[2]]/ntrades, 3))
    
    # Plot with annotations
    add_annotations <- input$add_annotations
    
    # Return to the output argument a dygraph plot with two y-axes
    if (add_annotations == "True") {
      dygraphs::dygraph(pnls, main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red") %>%
        dySeries(name=colnamev[3], axis="y", drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=colnamev[4], axis="y", drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == "False") {
      dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red")
    }  # end if
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
