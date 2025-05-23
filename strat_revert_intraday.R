##############################
# This is a shiny app for simulating a reversion to open strategy 
# for intraday stock prices for a single day.
# Runs the C++ function ratchet() from /Users/jerzy/Develop/Rcpp/back_test.cpp
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

# load("/Users/jerzy/Develop/data/SPY_minute_202407.RData")
pricev <- pricel[[1]][, 1]
retp <- rutils::diffit(pricev)
# Calculate cumulative returns
datev <- index(pricev)
retc <- cumsum(retp)
nrows <- NROW(pricev)
symboln <- rutils::get_name(colnames(pricev))


captiont <- paste("Reversion to Open Strategy")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbol
    # column(width=2, selectInput("symboln", label="Symbol", choices=rutils::etfenv$symbolv, selected="VTI")),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-ask spread
    column(width=2, numericInput("bidask", label="Bid-ask:", value=0.0000, step=0.0001))
  ),  # end fluidRow

  fluidRow(
    # Input the EMA decays
    column(width=2, sliderInput("lambdaf", label="Fast lambda:", min=0.01, max=0.9, value=0.6, step=0.01)),
    # column(width=2, sliderInput("varf", label="Variance factor:", min=1.0, max=100.0, value=10.0, step=0.1)),
    # column(width=2, sliderInput("varin", label="Initial variance:", min=0.01, max=1.0, value=0.25, step=0.01)),
    # column(width=2, sliderInput("lambdas", label="Slow lambda:", min=0.8, max=0.999, value=0.99, step=0.001)),
    # Input the EMA loadings
    # column(width=2, sliderInput("loadf", label="Fast load:", min=(-1.0), max=1.0, value=(-1.0), step=0.1)),
    # column(width=2, sliderInput("loads", label="Slow load:", min=(-1.0), max=1.0, value=(0.0), step=0.1)),
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
  #   symboln <- input$symboln
  #   cat("Loading data for ", symboln, "\n")
  #   
  #   ohlc <- get(symboln, rutils::etfenv)
  #   # quantmod::Cl(ohlc["2010/2019"])
  #   quantmod::Cl(ohlc)
  #   
  # })  # end Load the closing prices
  
  # Load the log returns
  # retp <- shiny::reactive({
  #   
  #   cat("Recalculating returns for ", input$symboln, "\n")
  #   
  #   rutils::diffit(log(closep()))
  # 
  # })  # end Load the log returns
  

  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    cat("Recalculating strategy for ", input$symboln, "\n")
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
    # emaf <- HighFreq::run_mean(retp, lambda=lambdaf)
    # volf <- sqrt(HighFreq::run_var(retp, lambda=lambdaf))
    # volf[1:7, ] <- 1.0
    # emas <- HighFreq::run_mean(retp, lambda=lambdas)
    # vols <- sqrt(HighFreq::run_var(retp, lambda=lambdas))
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
    
    # Calculate the positions of the mean-reversion strategy
    pospnls <- ratchet(pricev, input$lambdaf)
    # Lag the positions to trade in next period
    # posv <- rutils::lagit(pospnls[, 2], lagg=1)
    # Calculate indicator of flipped positions
    flipi <- rutils::diffit(pospnls[, 2])
    # Calculate the number of trades
    values$ntrades <- sum(abs(flipi) > 0)
    
    # Add buy/sell indicators for annotations
    longi <- (flipi > 0)
    shorti <- (flipi < 0)
    
    
    # Calculate strategy pnls
    # pnls <- posv*retp
    pnls <- pospnls[, 1]
    
    # Calculate transaction costs
    costs <- 0.5*input$bidask*abs(flipi)
    pnls <- (pnls - costs)

    # Scale the pnls so they have same SD as the returns
    # pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
    
    # Bind together strategy pnls
    pnls <- cbind(retp, pnls)
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)

    # Bind strategy pnls with indicators
    pnls <- cumsum(pnls)
    pnls <- cbind(pnls, retc[longi], retc[shorti])
    colnames(pnls) <- c(paste(input$symboln, "Returns"), "Strategy", "Buy", "Sell")

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
    
    captiont <- paste("Strategy for", input$symboln, "/ \n", 
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
