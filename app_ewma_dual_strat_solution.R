##############################
# This is a shiny app for simulating a dual EWMA moving average 
# crossover strategy for ETFs.
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

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol", choices=rutils::etfenv$symbolv, selected="VTI")),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-offer spread
    column(width=2, numericInput("bid_offer", label="Bid-offer:", value=0.0000, step=0.0001))
  ),  # end fluidRow

  fluidRow(
    # Input the EWMA decays
    column(width=2, sliderInput("fast_lambda", label="Fast lambda:", min=0.1, max=0.3, value=0.203, step=0.001)),
    column(width=2, sliderInput("slow_lambda", label="Slow lambda:", min=0.0, max=0.2, value=0.036, step=0.001)),
    # Input the look-back interval
    column(width=2, sliderInput("look_back", label="Look-back", min=5, max=250, value=169, step=1)),
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
    
    ohlc <- get(symbol, rutils::etfenv)
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
    fast_lambda <- input$fast_lambda
    slow_lambda <- input$slow_lambda
    look_back <- input$look_back
    lagg <- input$lagg

    # Calculate cumulative returns
    retv <- datav()
    retsum <- cumsum(retv)
    nrows <- NROW(retv)
    
    # Calculate EWMA weights
    fast_weights <- exp(-fast_lambda*1:look_back)
    fast_weights <- fast_weights/sum(fast_weights)
    slow_weights <- exp(-slow_lambda*1:look_back)
    slow_weights <- slow_weights/sum(slow_weights)
    
    # Calculate EWMA prices by filtering with the weights
    # retsum <- cumsum(rets_scaled)
    fast_ewma <- .Call(stats:::C_cfilter, retsum, filter=fast_weights, sides=1, circular=FALSE)
    fast_ewma[1:(look_back-1)] <- fast_ewma[look_back]
    slow_ewma <- .Call(stats:::C_cfilter, retsum, filter=slow_weights, sides=1, circular=FALSE)
    slow_ewma[1:(look_back-1)] <- slow_ewma[look_back]
    
    # Determine dates when the EWMAs have crossed
    indic <- sign(fast_ewma - slow_ewma)
    
    ## Backtest strategy for flipping if several consecutive positive and negative returns
    # Flip position only if the indic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # posit <- ifelse(indic == indic_lag, indic, posit)
    
    indic_sum <- HighFreq::roll_vec(tseries=matrix(indic), look_back=lagg)
    indic_sum[1:lagg] <- 0
    posit <- rep(NA_integer_, nrows)
    posit[1] <- 0
    posit <- ifelse(indic_sum == lagg, 1, posit)
    posit <- ifelse(indic_sum == (-lagg), -1, posit)
    posit <- zoo::na.locf(posit, na.rm=FALSE)
    posit[1:lagg] <- 0
    
    # Calculate indicator of flipping the positions
    indic <- rutils::diffit(posit)
    # Calculate number of trades
    values$ntrades <- sum(abs(indic)>0)
    
    # Add buy/sell indicators for annotations
    indic_buy <- (indic > 0)
    indic_sell <- (indic < 0)
    
    # Lag the positions to trade in next period
    posit <- rutils::lagit(posit, lagg=1)
    
    # Calculate strategy pnls
    pnls <- posit*returns
    
    # Calculate transaction costs
    costs <- 0.5*input$bid_offer*abs(indic)
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
    pnls <- cbind(pnls, retsum[indic_buy], retsum[indic_sell])
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
        dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red") %>%
        dySeries(name=colnamev[3], axis="y", label=colnamev[3], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=colnamev[4], axis="y", label=colnamev[4], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == "False") {
      dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red")
    }  # end if
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
