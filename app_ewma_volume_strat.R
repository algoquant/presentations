##############################
# This is a shiny app for simulating an EWMA moving average 
# crossover strategy using the trading volume.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

# captiont <- paste("EWMA Strategy for", symbol)
captiont <- paste("EWMA Moving Average Strategy Using the Trading Volumes")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
    # column(width=2,
           # h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
           # actionButton("recalcb", "Click to Recalculate")),
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol",
                                choices=c("SPY", rutils::etfenv$symbolv), selected="VTI")),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input EWMA decays
    column(width=2, sliderInput("lambdaf", label="lambdaf:", min=0.1, max=0.3, value=0.2, step=0.001)),
    column(width=2, sliderInput("lambdas", label="lambdas:", min=0.0, max=0.2, value=0.1, step=0.001)),
    # Input end points interval
    # column(width=2, selectInput("interval", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input the look-back interval
    column(width=2, sliderInput("look_back", label="Look-back", min=5, max=300, value=100, step=1)),
    # Input the trade lag
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=8, value=2, step=1)),
    # Input the exponent of volume
    column(width=2, sliderInput("exponent", label="exponent", min=0.05, max=2.0, value=1.0, step=0.05)),
    # Input the floor for volume
    column(width=2, sliderInput("floo_r", label="vol floor", min=0.01, max=0.25, value=0.1, step=0.01)),
    # Input the bid-offer spread
    column(width=2, numericInput("bid_offer", label="Bid-offer:", value=0.0000, step=0.0001))
    
  ),  # end fluidRow

  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot", width="100%", height="600px"), height=10, width=12)

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()

  # Load the data
  datav <- shiny::reactive({
    
    symbol <- input$symbol
    cat("Loading data for ", symbol, "\n")
    
    if (symbol == "SPY") {
      ## SPY ETF 1-minute bars
      ohlc <- HighFreq::SPY["2009"]
      closep <- quantmod::Cl(ohlc)
      retv <- rutils::diffit(log(closep))
      # Aggregate to daily data
      retsum <- cumsum(retv)
      retsum <- xts::to.daily(retsum)
      retv <- rutils::diffit(quantmod::Cl(retsum))
      retv <- returns/sd(retv)
      volumes <- quantmod::Vo(ohlc)
      volumes <- xts::xts(cumsum(volumes), index(datav()))
      volumes <- xts::to.daily(volumes)
      volumes <- rutils::diffit(quantmod::Cl(volumes))
      cbind(retv, volumes)
    } else {
      ohlc <- get(symbol, rutils::etfenv)
      closep <- quantmod::Cl(ohlc)
      retv <- rutils::diffit(log(closep))
      retv <- returns/sd(retv)
      volumes <- quantmod::Vo(ohlc)
      cbind(retv, volumes)
    }  # end if
    
  })  # end Load the data
  

  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    cat("Recalculating strategy for ", input$symbol, "\n")
    # Get model parameters from input argument
    lambdaf <- input$lambdaf
    lambdas <- input$lambdas
    look_back <- input$look_back
    lagg <- input$lagg
    # coeff <- input$coeff
    # input$recalcb
    
    # Get the data
    retv <- datav()[, 1]
    predv <- datav()[, 2]
    nrows <- NROW(retv)
    
    # Calculate EWMA weights
    weightf <- exp(-lambdaf*1:look_back)
    weightf <- weightf/sum(weightf)
    weightss <- exp(-lambdas*1:look_back)
    weightss <- weightss/sum(weightss)
    
    # Calculate EWMA prices by filtering with the weights
    ewmaf <- .Call(stats:::C_cfilter, predv, filter=weightf, sides=1, circular=FALSE)
    ewmaf[1:(look_back-1)] <- ewmaf[look_back]
    ewmas <- .Call(stats:::C_cfilter, predv, filter=weightss, sides=1, circular=FALSE)
    ewmas[1:(look_back-1)] <- ewmas[look_back]
    
    # Determine dates when the EWMAs have crossed
    indic <- sign(ewmaf - ewmas)
    
    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the indic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # posv <- ifelse(indic == indic_lag, indic, posv)
    
    indics <- HighFreq::roll_sum(tseries=matrix(indic), look_back=lagg)
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
    coeff <- (-1)
    # pnls <- 0.5*((coeff*posv*retv) + retv)
    pnls <- coeff*posv*returns
    
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
    retsum <- cumsum(retv)
    pnls <- cbind(pnls, retsum[longi], retsum[shorti])
    colnames(pnls) <- c(paste(input$symbol, "Returns"), "Strategy", "Buy", "Sell")

    pnls
    
  })  # end Recalculate the strategy
  

  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Get Sharpe ratios
    sharper <- values$sharper
    # Get number of trades
    ntrades <- values$ntrades

    captiont <- paste("Strategy for", input$symbol, "Trading Volumes / \n", 
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
