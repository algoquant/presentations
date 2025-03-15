##############################
# This is a shiny app for simulating the ratchet strategy 
# for a triplet of stocks.
# It can simulate the greedy ratchet using the function 
# ratchetg(), or the patient ratchet using the 
# function ratchet().
# 
# It bets on the portfolio price reverting to the moving 
# average price.
# The strategy calculates the z-score equal to the difference 
# between the current price minus the moving average price, 
# divided by the price volatility.
# If the z-score is positive, the strategy sells shares short, 
# proportional to the z-score.
# If the z-score is negative, the strategy buys shares, 
# proportional to the z-score.
# The strategy accumulates an inventory of shares. 
# It continues selling shares as the z-score keeps rising,
# and it continues buying as the z-score keeps dropping.
# The strategy waits to sell its inventory only after the 
# z-score has changed its sign, but not before that.


# Runs the C++ function ratchetg() from /Users/jerzy/Develop/Rcpp/back_test.cpp
#
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

# Load the intraday minute prices
# pricev <- lapply(4:7, function(x) {
#   load(paste0("/Users/jerzy/Develop/data/SPY_minute_20240", x, ".RData"))
#   do.call(rbind, pricel)
# }) # end lapply
# pricev <- do.call(rbind, pricev)
# priceref1 <- pricev
# 
# pricev <- lapply(4:7, function(x) {
#   load(paste0("/Users/jerzy/Develop/data/TLT_minute_20240", x, ".RData"))
#   do.call(rbind, pricel)
# }) # end lapply
# pricev <- do.call(rbind, pricev)
# priceref2 <- pricev
# 
# pricev <- lapply(4:7, function(x) {
#   load(paste0("/Users/jerzy/Develop/data/XLK_minute_20240", x, ".RData"))
#   do.call(rbind, pricel)
# }) # end lapply
# pricev <- do.call(rbind, pricev)
# pricetarg <- pricev


# Calculate the symbol names
nrows <- NROW(priceref1)
symboltarg <- rutils::get_name(colnames(pricetarg))
symbolref1 <- rutils::get_name(colnames(priceref1))
symbolref2 <- rutils::get_name(colnames(priceref2))
symbolpair <- paste0(symboltarg, "/", symbolref1, "/", symbolref2)
captiont <- paste0("Ratchet Strategy For ", symbolpair)

## Calculate the overnight dates - the first time stamp of the day
daton <- xts::.index(priceref1)
daton <- rutils::diffit(daton)
daton <- which(daton > 1000)


## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbol
    # column(width=2, selectInput("symbolref1", label="Symbol", choices=rutils::etfenv$symbolv, selected="VTI")),
    # Input the beta parameter
    column(width=2, sliderInput("betac1", label="beta1:", min=0.0, max=2.0, value=0.68, step=0.01)),
    column(width=2, sliderInput("betac2", label="beta2:", min=0.0, max=2.0, value=0.68, step=0.01)),
    # Input the lambda decay factor
    column(width=2, sliderInput("lambdaf", label="Lambda:", min=0.8, max=0.999, value=0.9, step=0.001)),
    # Input the Z-score factor
    column(width=2, sliderInput("zfact", label="Z-factor", min=0.1, max=5.0, value=1.0, step=0.1)),
    # Input the volatility threshold floor
    # column(width=2, sliderInput("volf", label="volatility floor", min=0.0, max=0.5, value=0.0, step=0.05)),
    # Input the bid-ask spread
    column(width=1, numericInput("bidask", label="Bid-ask:", value=0.1, step=0.01)),
    # Input trending or reverting (contrarian) strategy
    # column(width=1, selectInput("coeff", label="Trend (1) Revert (-1)", choices=c(1, -1), selected=(-1)))
    # Input add annotations Boolean
    # column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False"))
  ),  # end fluidRow

  # fluidRow(
    # column(width=2, sliderInput("varf", label="Variance factor:", min=1.0, max=100.0, value=10.0, step=0.1)),
    # column(width=2, sliderInput("varin", label="Initial variance:", min=0.01, max=1.0, value=0.25, step=0.01)),
    # column(width=2, sliderInput("lambdas", label="Slow lambda:", min=0.8, max=0.999, value=0.99, step=0.001)),
    # Input the EMA loadings
    # column(width=2, sliderInput("loadf", label="Fast load:", min=(-1.0), max=1.0, value=(-1.0), step=0.1)),
    # column(width=2, sliderInput("loads", label="Slow load:", min=(-1.0), max=1.0, value=(0.0), step=0.1)),
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
  #   symbolref1 <- input$symbolref1
  #   cat("Loading data for ", symbolref1, "\n")
  #   
  #   ohlc <- get(symbolref1, rutils::etfenv)
  #   # quantmod::Cl(ohlc["2010/2019"])
  #   quantmod::Cl(ohlc)
  #   
  # })  # end Load the closing prices
  
  # Load the log returns
  # retv <- shiny::reactive({
  #   
  #   cat("Recalculating returns for ", input$symbolref1, "\n")
  #   
  #   rutils::diffit(log(closep()))
  # 
  # })  # end Load the log returns
  

  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    cat("Recalculating strategy for", symbolpair, "\n")
    # Get model parameters from input argument
    # closep <- closep()
    # varf <- input$varf
    # varin <- input$varin
    # lambdaf <- input$lambdaf
    # lambdas <- input$lambdas
    # loadf <- input$loadf
    # loads <- input$loads
    # lookb <- input$lookb
    lagg <- input$lagg

    # Calculate EMA prices
    # emaf <- HighFreq::run_mean(retv, lambda=lambdaf)
    # volf <- sqrt(HighFreq::run_var(retv, lambda=lambdaf))
    # volf[1:7, ] <- 1.0
    # emas <- HighFreq::run_mean(retv, lambda=lambdas)
    # vols <- sqrt(HighFreq::run_var(retv, lambda=lambdas))
    # vols[1:7, ] <- 1.0
    
    # Determine dates when the EMAs have crossed
    # crossi <- sign(emaf - emas)
    
    # Calculate cumulative sum of EMA crossing indicator
    # crossc <- HighFreq::roll_sum(tseries=crossi, lookb=lagg)
    # crossc[1:lagg] <- 0
    # Calculate the positions
    # Flip position only if the crossi and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # posv <- (loadf*emaf + loads*emas)
    
    # Calculate the pair prices
    pricev <- pricetarg - input$betac1*priceref1 - input$betac2*priceref2
    retv <- rutils::diffit(pricev)

    # Calculate the positions of the mean-reversion strategy
    # pospnls <- ratchetg(pricev, input$lambdaf)
    # pospnls <- ratchet(pricev, pricop=pricop, lambdaf=input$lambdaf, volf=input$volf, zfact=input$zfact)
    pospnls <- ratchet(pricev, lambdaf=input$lambdaf, zfact=input$zfact)
    # Lag the positions to trade in next period
    # posv <- rutils::lagit(pospnls[, 2], lagg=1)
    # Calculate indicator of flipped positions
    flipi <- rutils::diffit(pospnls[, 2])
    # Calculate the number of trades
    values$ntrades <- sum(abs(flipi) > 0)
    
    # Add buy/sell indicators for annotations
    longi <- (flipi > 0)
    shorti <- (flipi < 0)
    
    
    # Calculate strategy PnLs
    # posv <- pospnls[, 2]
    # pnls <- as.numeric(input$coeff)*posv*retv
    pnls <- pospnls[, 1]
    # Calculate the overnight PnLs
    pnlon <- pnls[daton]
    values$pnlon <- round(sum(pnlon), 1)
    
    
    # Calculate transaction costs
    costs <- 0.5*input$bidask*abs(flipi)
    pnls <- (pnls - costs)

    # Scale the PnLs so they have same SD as the returns
    # pnls <- pnls*sd(retv[retv<0])/sd(pnls[pnls<0])
    
    # Bind together strategy PnLs
    pnls <- cbind(retv, pnls)
    # pnls <- pnls[daton]

    # Calculate the Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)

    # Bind strategy PnLs with indicators
    pnls <- cumsum(pnls)
    # pnls <- cbind(pnls, retc[longi], retc[shorti])
    # colnames(pnls) <- c(paste(input$symbolref1, "Returns"), "Strategy", "Buy", "Sell")
    colnames(pnls) <- c("Pair", "Strategy")
    
    pnls

  })  # end Recalculate the strategy
  

  # Plot the cumulative strategy PnLs
  output$dyplot <- dygraphs::renderDygraph({
    
    # Get the PnLs
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Get Sharpe ratios
    sharper <- values$sharper

    captiont <- paste0(captiont, " / ", 
                      paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), " / ",
                      "Trades=", values$ntrades, " / Overnight=", values$pnlon)
    
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
      dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red") %>%
        dyLegend(show="always", width=500)
    }  # end if
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
