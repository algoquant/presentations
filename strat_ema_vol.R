##############################
# This is a shiny app for simulating an EMA moving average 
# crossover strategy for volatility.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

captiont <- paste("EMA Moving Average Crossover Strategy for Volatility")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol",
                                choices=rutils::etfenv$symbolv, selected="VTI")),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-ask spread
    column(width=2, numericInput("bidask", label="Bid-ask:", value=0.0000, step=0.0001))
  ),  # end fluidRow

  fluidRow(
    # Input look-back intervals
    column(width=2, sliderInput("fast_back", label="fast_back:", min=1, max=21, value=5, step=1)),
    column(width=2, sliderInput("slow_back", label="slow_back:", min=11, max=251, value=151, step=1)),
    # Input the trade lag
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=8, value=2, step=1))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot", width="100%", height="600px"), height=10, width=12)

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()

  # Load the data
  ohlc <- shiny::reactive({
    
    symbol <- input$symbol
    cat("Loading data for ", symbol, "\n")
    
    get(symbol, rutils::etfenv)

  })  # end Load the data
  

  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    cat("Recalculating strategy for ", input$symbol, "\n")
    # Get model parameters from input argument
    fast_back <- input$fast_back
    slow_back <- input$slow_back
    lagg <- input$lagg

    # Calculate cumulative returns
    ohlc <- ohlc()
    closep <- quantmod::Cl(ohlc)
    retv <- rutils::diffit(log(closep))
    retv <- returns/sd(retv)
    retsum <- cumsum(retv)
    nrows <- NROW(retv)
    
    # Calculate the slow and fast volatilities
    if (fast_back > 1) {
      fast_var <- HighFreq::roll_var_ohlc(ohlc=ohlc, lookb=fast_back, scale=FALSE)
      slow_var <- HighFreq::roll_var_ohlc(ohlc=ohlc, lookb=slow_back, scale=FALSE)
    } else {
      high_low <- quantmod::Hi(ohlc) - quantmod::Lo(ohlc)
      fast_var <- as.numeric(high_low)
      slow_var <- HighFreq::roll_sum(tseries=high_low, lookb=slow_back)/slow_back
    }  # end if

    # Determine dates when the EMAs have crossed
    indic <- -sign(fast_var - slow_var)
    
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
