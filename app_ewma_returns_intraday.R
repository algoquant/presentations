##############################
# This is a shiny app for simulating an autoregressive strategy 
# using the trailing fast and slow Sharpe ratios.
# The trailing Sharpe ratio is equal to the EWMA of returns 
# divided by the trailing volatility of intraday returns.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

# Load the intraday returns.
load("/Users/jerzy/Develop/data/SPY_second_20231113.RData")

# symbol <- "AAPL"
captiont <- paste("Autoregressive Strategy Using Trailing Fast And Slow Sharpe Ratios")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol", choices=c("SPY", "AAPL"), selected="SPY")),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-ask spread
    column(width=2, numericInput("bidask", label="Bid-ask:", value=0.002, step=0.001))
  ),  # end fluidRow

  fluidRow(
    # Input the EWMA decays
    column(width=2, sliderInput("lambdaf", label="Fast lambda:", min=0.01, max=0.99, value=0.9, step=0.01)),
    column(width=2, sliderInput("lambdas", label="Slow lambda:", min=0.99, max=0.9999, value=0.9993, step=0.0001)),
    # Input the EWMA loadings
    column(width=2, sliderInput("loadf", label="Fast load:", min=(-1.0), max=1.0, value=(-1.0), step=0.1)),
    column(width=2, sliderInput("loads", label="Slow load:", min=(-1.0), max=1.0, value=(0.0), step=0.1)),
    # Input the trade lag
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=4, value=2, step=1))
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
  #   # ohlc <- get(symbol, rutils::etfenv)
  #   # quantmod::Cl(ohlc["2010/2019"])
  #   pricel[[51]][, 1]
  # 
  # })  # end Load the closing prices
  
  # Load the log returns
  # retv <- shiny::reactive({
  #   
  #   cat("Recalculating returns for ", input$symbol, "\n")
  #   
  #   rutils::diffit(closep())
  # 
  # })  # end Load the log returns
  

  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    cat("Recalculating strategy for", input$symbol, "\n")
    # Get model parameters from input argument
    # closep <- closep()
    lambdaf <- input$lambdaf
    lambdas <- input$lambdas
    loadf <- input$loadf
    loads <- input$loads
    bidask <- input$bidask
    # look_back <- input$look_back
    lagg <- input$lagg
    
    # Calculate cumulative returns
    # retv <- retv()
    # retc <- cumsum(retv)
    # nrows <- NROW(retv)
    

    # Determine dates when the EWMAs have crossed
    # crossi <- sign(ewmaf - ewmas)
    
    # Calculate cumulative sum of EWMA crossing indicator
    # crossc <- HighFreq::roll_sum(tseries=crossi, look_back=lagg)
    # crossc[1:lagg] <- 0
    # Calculate the positions
    # Flip position only if the crossi and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    
    ntrades <- 0
    pnls <- lapply(retl, function(retv) {
      # Calculate EWMA prices
      ewmaf <- HighFreq::run_mean(retv, lambda=lambdaf)
      volf <- sqrt(HighFreq::run_var(retv, lambda=lambdaf))
      volf[1:7, ] <- 1.0
      ewmas <- HighFreq::run_mean(retv, lambda=lambdas)
      vols <- sqrt(HighFreq::run_var(retv, lambda=lambdas))
      # vols <- sqrt(HighFreq::run_mean(retv^2, lambda=lambdas))
      vols[1:7, ] <- 1.0
      # cat("head(vols) =", head(vols, 11), "\n")
      # cat("tail(vols) =", tail(vols), "\n")
      posv <- 100*(loadf*ewmaf/volf + loads*ewmas/vols)
      posv <- round(posv)
      # posv[posv == 0] <- NA
      posv[1] <- 0
      posv <- zoo::na.locf(posv)
      # Lag the positions to trade in next period
      posv <- rutils::lagit(posv, lagg=1)
      # cat("posv =", tail(posv), "\n")
      # Calculate strategy pnls
      pnls <- posv*retv
      # Calculate indicator of flipped positions
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
    # colnames(pnls) <- c(paste(symboln, "Returns"), "Strategy")
    colnames(pnls) <- c(paste(input$symbol, "Returns"), "Strategy")
    
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
    
    captiont <- paste("Strategy for", input$symbol, "/ \n", 
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
