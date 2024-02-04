##############################
# This is a shiny app for simulating a dual EWMA moving average 
# crossover strategy for stocks.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

captiont <- paste("Dual EWMA Moving Average Crossover Strategy")

## Setup for Polygon
startd <- as.Date("1990-01-01")
# startd <- as.Date("2022-03-01")
endd <- Sys.Date()
tspan <- "day"
# tspan <- "minute"
apikey <- "UJcr9ctoMBXEBK1Mqu_KQAkUuBxLvEtE"

## Load S&P500 symbols
sp500table <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv")
symbolv <- sp500table$Ticker
symbol <- "AAPL"


## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol", choices=symbolv, selected=symbol)),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-ask spread
    column(width=2, numericInput("bidask", label="Bid-ask:", value=0.0000, step=0.0001))
  ),  # end fluidRow

  fluidRow(
    # Input the EWMA decays
    column(width=2, sliderInput("lambdaf", label="Fast lambda:", min=0.1, max=0.3, value=0.2, step=0.001)),
    column(width=2, sliderInput("lambdas", label="Slow lambda:", min=0.0, max=0.2, value=0.1, step=0.001)),
    # Input the look-back interval
    column(width=2, sliderInput("lookb", label="Look-back", min=5, max=250, value=100, step=1)),
    # Input the trade lag
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=8, value=2, step=1))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="550px")
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()

  # Load the data
  datav <- shiny::reactive({
    
    symbol <- input$symbol
    cat("Loading data for ", symbol, "\n")
    
    # Load stock prices from Polygon
    ohlc <- rutils::getpoly(symbol=input$symbol, startd=startd, tspan=tspan, apikey=apikey)
    closep <- quantmod::Cl(ohlc)
    retv <- rutils::diffit(log(closep))
    retv <- returns/sd(retv)
    # volumes <- quantmod::Vo(ohlc)
    # cbind(retv, volumes)
    returns
    
  })  # end Load the data
  

  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    cat("Recalculating strategy for ", input$symbol, "\n")
    # Get model parameters from input argument
    lambdaf <- input$lambdaf
    lambdas <- input$lambdas
    lookb <- input$lookb
    lagg <- input$lagg

    # Calculate cumulative returns
    retv <- datav()
    retsum <- cumsum(retv)
    nrows <- NROW(retv)
    
    # Calculate EWMA weights
    weightf <- exp(-lambdaf*1:lookb)
    weightf <- weightf/sum(weightf)
    weightss <- exp(-lambdas*1:lookb)
    weightss <- weightss/sum(weightss)
    
    # Calculate EWMA prices by filtering with the weights
    # retsum <- cumsum(rets_scaled)
    ewmaf <- .Call(stats:::C_cfilter, retsum, filter=weightf, sides=1, circular=FALSE)
    ewmaf[1:(lookb-1)] <- ewmaf[lookb]
    ewmas <- .Call(stats:::C_cfilter, retsum, filter=weightss, sides=1, circular=FALSE)
    ewmas[1:(lookb-1)] <- ewmas[lookb]
    
    # Determine dates when the EWMAs have crossed
    indic <- sign(ewmaf - ewmas)
    
    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the indic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # posv <- ifelse(indic == indic_lag, indic, posv)
    
    indics <- HighFreq::roll_sum(tseries=matrix(indic), lookb=lagg)
    indics[1:lagg] <- 0
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    posv <- ifelse(indics == lagg, 1, posv)
    posv <- ifelse(indics == (-lagg), -1, posv)
    posv <- zoo::na.locf(posv, na.rm=FALSE)
    posv[1:lagg] <- 0
    
    # Calculate indicator of flipping the positions
    indic <- rutils::diffit(posv)
    # Calculate number of trades
    values$ntrades <- sum(abs(indic)>0)
    
    # Add buy/sell indicators for annotations
    longi <- (indic > 0)
    shorti <- (indic < 0)
    
    # Lag the positions to trade in next period
    posv <- rutils::lagit(posv, lagg=1)
    
    # Calculate strategy pnls
    pnls <- posv*returns
    
    # Calculate transaction costs
    costs <- 0.5*input$bidask*abs(indic)
    pnls <- (pnls - costs)

    # Scale the pnls so they have same SD as returns
    pnls <- pnls*sd(retv[returns<0])/sd(pnls[pnls<0])
    
    # Bind together strategy pnls
    pnls <- cbind(retv, pnls)
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)

    # Bind with indicators
    pnls <- cumsum(pnls)
    pnls <- cbind(pnls, retsum[longi], retsum[shorti])
    colnames(pnls) <- c(paste(input$symbol, "Returns"), "Strategy", "Buy", "Sell")

    pnls

  })  # end Recalculate the strategy
  

  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    # Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Get Sharpe ratios
    sharper <- values$sharper
    # Get number of trades
    ntrades <- values$ntrades
    
    captiont <- paste("Strategy for", input$symbol, "Returns Scaled by the Trading Volumes / \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
                      "Number of trades=", ntrades)
    
    # Plot with annotations
    add_annotations <- input$add_annotations
    
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
