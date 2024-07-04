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

# Load the AAPL OHLC prices, and calculate the high-low range
# load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
# ohlc <- sp500env$AAPL["2023"]
# hiloaapl <- (ohlc[, 2] - ohlc[, 3])
# all.equal(index(hilovti), index(hiloaapl))

# Calculate the beta of AAPL with respect to VTI
# betac <- cov(hilovti[, 1], hiloaapl[, 1])/var(hilovti[, 1])
# betac <- drop(betac)

# Load the VTI and AAPL minute prices
# load("/Users/jerzy/Develop/data/SPY_minute_202405.RData")
# pricespy <- pricel
# load("/Users/jerzy/Develop/data/AAPL_minute_202405.RData")
# pricaapl <- pricel

# Calculate list of AAPL minute prices hedged with VTI
# pricel <- lapply(seq_along(pricespy), function(it) {
#   (pricaapl[[it]] - betac*pricespy[[it]])
# }) # end lapply



symboln <- rutils::get_name(colnames(pricel[[1]][, 1]))
captiont <- paste("Reversion to Open Strategy")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbol
    # column(width=2, selectInput("symboln", label="Symbol", choices=c("SPY", "AAPL"), selected="SPY")),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-ask spread
    column(width=2, numericInput("bidask", label="Bid-ask:", value=0.000, step=0.001))
  ),  # end fluidRow

  fluidRow(
    # Input the EMA decays
    column(width=2, sliderInput("posmax", label="Posmax:", min=0.01, max=10, value=2, step=0.5)),
    column(width=2, sliderInput("betac", label="Beta:", min=-1.0, max=2.0, value=1.0, step=0.1))
    # column(width=2, sliderInput("varin", label="Initial variance:", min=0.01, max=1.0, value=0.25, step=0.01)),
    # Input the trade lag
    # column(width=2, sliderInput("lagg", label="lagg", min=1, max=4, value=2, step=1))
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
  #   symboln <- symboln
  #   cat("Loading data for ", symboln, "\n")
  #   
  #   # ohlc <- get(symboln, rutils::etfenv)
  #   # quantmod::Cl(ohlc["2010/2019"])
  #   pricel[[51]][, 1]
  # 
  # })  # end Load the closing prices
  
  # Load the log returns
  # retv <- shiny::reactive({
  #   
  #   cat("Recalculating returns for ", symboln, "\n")
  #   
  #   rutils::diffit(closep())
  # 
  # })  # end Load the log returns
  
  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    cat("Recalculating strategy for", symboln, "\n")
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
    
    # Calculate list of AAPL minute prices hedged with VTI
    # betac <- input$betac
    pricel <- lapply(seq_along(priceqqq), function(it) {
      (pricexlk[[it]] - input$betac*priceqqq[[it]])
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
      pricev <- pricev[, 1]
      retv <- rutils::diffit(pricev)
      pospnls <- revert_to_open(pricev, input$posmax)
      # posv <- bollinger_brackets(retv, input$posmax, input$betac, input$varin)
      # cat("posv =", tail(posv), "\n")
      # Calculate strategy pnls
      pnls <- pospnls[, 1]
      # Calculate indicator of flipped positions
      flipi <- rutils::diffit(pospnls[, 2])
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
    # colnames(pnls) <- c(paste(symboln, "Returns"), "Strategy")
    colnames(pnls) <- c(paste(symboln, "Returns"), "Strategy")
    
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
    
    captiont <- paste("Strategy for", symboln, "/ \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
                      "Number of trades=", ntrades)
    
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
      endd <- rutils::calc_endpoints(pnls, interval="minutes")
      dygraphs::dygraph(pnls[endd, 1:2], main=captiont) %>%
      # dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red")
    }  # end if
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
