##############################
# This is a shiny app for simulating a reversion to open strategy 
# for intraday stock prices.
# Runs the C++ function revert_to_open() from /Users/jerzy/Develop/Rcpp/back_test.cpp
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

# Compile this file in R by running this command:
# Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/back_test.cpp")

# Load the intraday returns.
# load("/Users/jerzy/Develop/data/SPY_second_20231113.RData")
# load("/Users/jerzy/Develop/data/SPY_minute_20231123.RData")


###############
# Load the minute prices of two stocks.
# Below is an example of how to load the minute prices of two stocks.

# Load the VTI OHLC prices, and calculate the high-low range
# ohlc <- rutils::etfenv$VTI["2023"]
# hilovti <- (ohlc[, 2] - ohlc[, 3])

# Load the XLK OHLC prices, and calculate the high-low range
# load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
# ohlc <- sp500env$XLK["2023"]
# hiloxlk <- (ohlc[, 2] - ohlc[, 3])
# all.equal(index(hilovti), index(hiloxlk))

# Calculate the beta of XLK with respect to VTI
# betac <- cov(hilovti[, 1], hiloxlk[, 1])/var(hilovti[, 1])
# betac <- drop(betac)

# Load the VTI and XLK minute prices
# load("/Users/jerzy/Develop/data/XLK_minute_202405.RData")
# pricetarg <- pricel
# symboltarg <- colnames(pricetarg[[1]])
# load("/Users/jerzy/Develop/data/SPY_minute_202405.RData")
# priceref <- pricel

# Calculate list of XLK minute prices hedged with VTI
# pricel <- lapply(seq_along(pricespy), function(it) {
#   (pricetarg[[it]] - betac*pricespy[[it]])
# }) # end lapply


###############
# Load the minute prices of two stocks for several months.

# load("/Users/jerzy/Develop/data/SPY_minute_202404.RData")
# priceref <- pricel
# load("/Users/jerzy/Develop/data/SPY_minute_202405.RData")
# priceref <- c(priceref, pricel)
# load("/Users/jerzy/Develop/data/SPY_minute_202406.RData")
# priceref <- c(priceref, pricel)
# load("/Users/jerzy/Develop/data/SPY_minute_202407.RData")
# priceref <- c(priceref, pricel)
# NROW(priceref)
# load("/Users/jerzy/Develop/data/VXX_minute_202404.RData")
# pricetarg <- pricel
# load("/Users/jerzy/Develop/data/VXX_minute_202405.RData")
# pricetarg <- c(pricetarg, pricel)
# load("/Users/jerzy/Develop/data/VXX_minute_202406.RData")
# pricetarg <- c(pricetarg, pricel)
# load("/Users/jerzy/Develop/data/VXX_minute_202407.RData")
# pricetarg <- c(pricetarg, pricel)
# NROW(pricetarg)



# symboltarg <- colnames(pricetarg[[1]])
symbolref <- colnames(priceref[[1]])
# captiont <- paste("Daily Ratchet Strategy For", symbolref)
symbolpair <- paste0(symboltarg, "/", symbolref)
captiont <- paste("Daily Ratchet Revert to Open for", symbolpair)

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbol
    # column(width=2, selectInput("symboltarg", label="Symbol", choices=c("SPY", "XLK"), selected="SPY")),
    # Input the beta parameter
    column(width=3, sliderInput("betac", label="beta:", min=-1.0, max=2.0, value=0.68, step=0.01)),
    # Input the lambda decay factor
    # column(width=2, sliderInput("lambdaf", label="Lambda:", min=0.9, max=0.999, value=0.99, step=0.001)),
    # Input the Z-score factor
    column(width=3, sliderInput("zfact", label="Z-factor", min=0.1, max=5.0, value=1.0, step=0.1)),
    # Input the Z-score factor
    # column(width=2, sliderInput("poslimit", label="Pos limit", min=1, max=100, value=100, step=1)),
    # Input add annotations Boolean
    # column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-ask spread
    column(width=1, numericInput("bidask", label="Bid-ask:", value=0.1, step=0.001))
  ),  # end fluidRow

  # fluidRow(
    # Input the EMA decays
    # column(width=2, sliderInput("posmax", label="Posmax:", min=0.01, max=10, value=2, step=0.5)),
    # column(width=2, sliderInput("betac", label="Beta:", min=-1.0, max=2.0, value=1.0, step=0.1))
    # column(width=2, sliderInput("varin", label="Initial variance:", min=0.01, max=1.0, value=0.25, step=0.01)),
    # Input the trade lag
    # column(width=2, sliderInput("lagg", label="lagg", min=1, max=4, value=2, step=1))
  # ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()
  # Load the closing prices
  # closep <- shiny::reactive({
  #   
  #   symboltarg <- symboltarg
  #   cat("Loading data for ", symboltarg, "\n")
  #   
  #   # ohlc <- get(symboltarg, rutils::etfenv)
  #   # quantmod::Cl(ohlc["2010/2019"])
  #   pricel[[51]][, 1]
  # 
  # })  # end Load the closing prices
  
  # Load the log returns
  # retv <- shiny::reactive({
  #   
  #   cat("Recalculating returns for ", symboltarg, "\n")
  #   
  #   rutils::diffit(closep())
  # 
  # })  # end Load the log returns
  
  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    cat("Recalculating strategy for", symbolref, "\n")
    # Get model parameters from input argument
    # closep <- closep()
    # posmax <- input$posmax
    # lambdas <- input$lambdas
    # loads <- input$loads
    bidask <- input$bidask
    # lookb <- input$lookb
    lagg <- input$lagg
    
    # Calculate cumulative returns
    # retv <- retv()
    # retc <- cumsum(retv)
    # nrows <- NROW(retv)
    
    # Calculate list of XLK minute prices hedged with VTI
    # betac <- input$betac
    pricel <- lapply(seq_along(priceref), function(it) {
      (pricetarg[[it]] - input$betac*priceref[[it]])
    }) # end lapply
    
    
    # Determine dates when the EMAs have crossed
    # crossi <- sign(emaf - emas)
    
    # Calculate cumulative sum of EMA crossing indicator
    # crossc <- HighFreq::roll_sum(tseries=crossi, lookb=lagg)
    # crossc[1:lagg] <- 0
    # Calculate the positions
    # Flip position only if the crossi and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    
    ntrades <- 0
    pnls <- lapply(pricel, function(pricev) {
      # pricev <- pricev[, 1]
      retv <- rutils::diffit(pricev)
      pospnls <- ratchetx(pricev, zfact=input$zfact)
      # pospnls <- ratchet(pricev, lambdaf=input$lambdaf, zfact=input$zfact)
      # pospnls <- ratchet(pricev, zfact=input$zfact, poslimit=input$poslimit)
      # posv <- bollinger_brackets(retv, input$posmax, input$betac, input$varin)
      # cat("posv =", tail(posv), "\n")
      # Calculate strategy pnls
      pnls <- pospnls[, 1]
      # Calculate indicator of flipped positions
      posv <- pospnls[, 2]
      flipi <- rutils::diffit(posv)
      # Calculate the number of trades
      ntrades <<- ntrades + sum(abs(flipi) > 0)
      # Calculate transaction costs
      costv <- 0.5*bidask*abs(flipi)
      pnls <- (pnls - costv)
      pnls <- cbind(retv, pnls)
      pnls
    }) # end lapply
    pnls <- do.call(rbind, pnls)

    # Add buy/sell indicators for annotations
    # longi <- (flipi > 0)
    # shorti <- (flipi < 0)
    # 
    values$ntrades <- ntrades
    
    
    # Scale the pnls so they have same SD as the returns
    # pnls <- pnls*sd(retv[retv<0])/sd(pnls[pnls<0])
    
    # Bind together strategy pnls
    # pnls <- cbind(retv, pnls)
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)

    # Bind strategy pnls with indicators
    pnls <- cumsum(pnls)
    # pnls <- cbind(pnls, retc[longi], retc[shorti])
    # colnames(pnls) <- c(paste(input$symbol, "Returns"), "Strategy", "Buy", "Sell")
    # colnames(pnls) <- c(paste(symboltarg, "Returns"), "Strategy")
    colnames(pnls) <- c("Pair", "Strategy")
    
    # cat("pnls =", tail(pnls[, 2]), "\n")
    pnls

  })  # end Recalculate the strategy
  

  # Plot the cumulative strategy pnls
  output$dyplot <- dygraphs::renderDygraph({
    
    # Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Get Sharpe ratios
    sharper <- values$sharper
    # Get number of trades
    ntrades <- values$ntrades
    
    captiont <- paste("Strategy for", symbolref, "/ \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
                      "Number of trades=", ntrades)
    
    # Plot with annotations
    # add_annotations <- input$add_annotations
    add_annotations <- FALSE
    
    # Return to the output argument a dygraph plot with two y-axes
    if (isTRUE(add_annotations)) {
      dygraphs::dygraph(pnls, main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red") %>%
        dySeries(name=colnamev[3], axis="y", drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=colnamev[4], axis="y", drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else {
      # endd <- rutils::calc_endpoints(pnls, interval="minutes")
      dygraphs::dygraph(pnls, main=captiont) %>%
      # dygraphs::dygraph(pnls[endd, 1:2], main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red")
    }  # end if
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
