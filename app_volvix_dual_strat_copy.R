##############################
# This is a shiny app for simulating a contrarian strategy 
# using z-scores of regressions for VXX and SVXY prices versus 
# the rolling VTI volatility.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(rutils)
library(shiny)
library(dygraphs)

## Model and data setup

# Load the VIX data
svxy <- log(quantmod::Cl(get("SVXY", rutils::etfenv)))
dates <- zoo::index(svxy)
vxx <- log(quantmod::Cl(get("VXX", rutils::etfenv)))
vxx <- vxx[dates]
vtis <- log(get("VTI", rutils::etfenv))
vtis <- vtis[dates]
vti_close <- quantmod::Cl(vtis)

captiont <- paste("Regression Z-score of VXX and SVXY Prices Versus VTI Volatility")

# Variable setup for testing
# symbol <- "VTI"
# ohlc <- log(get(symbol, rutils::etfenv)[dates])
# lambda <- 0.85
# bid_offer <- 0.0
# look_back <- 41
# threshold <- 0.03
# coeff <- (-1)
# lagg <- 1
# closep <- quantmod::Cl(ohlc)
# returns <- rutils::diffit(closep)
# retsum <- cumsum(returns)
# nrows <- NROW(returns)


## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol to Trade",
                                choices=rutils::etfenv$symbolv, selected="VTI")),
    # Input VIX symbol
    # column(width=2, selectInput("symbol_vix", label="Symbol VIX",
    #                             choices=c("VXX", "SVXY"), selected="VXX")),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-offer spread
    column(width=2, numericInput("bid_offer", label="Bid-offer:", value=0.0, step=0.0001))
  ),  # end fluidRow

  fluidRow(
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Look-back", min=2, max=100, value=41, step=1)),
    column(width=3, sliderInput("lambda", label="lambda:", min=0.01, max=0.99, value=0.85, step=0.01)),
    # Input threshold interval
    column(width=2, sliderInput("threshold", label="Threshold", min=0.01, max=0.1, value=0.03, step=0.01)),
    # Input the strategy coefficient: coeff=1 for momentum, and coeff=-1 for contrarian
    column(width=2, selectInput("coeff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # column(width=2, sliderInput("look_back", label="look_back:", min=1, max=21, value=5, step=1)),
    # column(width=2, sliderInput("slow_back", label="slow_back:", min=11, max=251, value=151, step=1)),
    # Input the trade lag
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=8, value=1, step=1))
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
    
    log(get(symbol, rutils::etfenv)[dates])

  })  # end Load the data
  

  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    symbol <- input$symbol
    cat("Recalculating strategy for ", symbol, "\n")
    # Get model parameters from input argument
    look_back <- input$look_back
    lambda <- input$lambda
    coeff <- as.numeric(input$coeff)
    lagg <- input$lagg

    # Calculate cumulative returns
    ohlc <- ohlc()
    closep <- quantmod::Cl(ohlc)
    returns <- rutils::diffit(closep)
    # returns <- returns/sd(returns)
    retsum <- cumsum(returns)
    nrows <- NROW(returns)

    # Calculate rolling volatility
    variance <- HighFreq::run_var_ohlc(vtis, lambda=lambda)
    # variance <- HighFreq::roll_var_ohlc(ohlc=vtis, look_back=look_back, scale=FALSE)
    variance[variance == 0] <- 1
    
    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the indic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # posit <- ifelse(indic == indic_lag, indic, posit)
    
    # Flip position if the scaled returns exceed threshold
    threshold <- input$threshold
    
    # Calculate trailing z-scores of SVXY
    predictor <- cbind(sqrt(variance), vxx, vti_close)
    response <- svxy
    # zscores <- drop(HighFreq::roll_reg(response=response, predictor=predictor, look_back=look_back))
    rollreg <- HighFreq::roll_reg(response=response, predictor=predictor, intercept=TRUE, look_back=look_back)
    zscores <- rollreg[, NCOL(rollreg), drop=TRUE]
    # zscores <- HighFreq::run_reg(response=response, predictor=predictor, lambda=lambda, method="scale")
    # zscores <- zscores[, 1, drop=TRUE]
    # zscores[1:look_back] <- 0
    # zscores[is.infinite(zscores)] <- 0
    # zscores[is.na(zscores)] <- 0
    # zscores <- zscores/sqrt(look_back)
    indic <- rep(NA_integer_, nrows)
    indic[1] <- 0
    indic[zscores > threshold] <- coeff
    indic[zscores < (-threshold)] <- (-coeff)
    indic <- zoo::na.locf(indic, na.rm=FALSE)
    indic_sum <- HighFreq::roll_vec(tseries=matrix(indic), look_back=lagg)
    indic_sum[1:lagg] <- 0
    posit <- rep(NA_integer_, nrows)
    posit[1] <- 0
    posit <- ifelse(indic_sum == lagg, 1, posit)
    posit <- ifelse(indic_sum == (-lagg), -1, posit)
    posit <- zoo::na.locf(posit, na.rm=FALSE)
    posit[1:lagg] <- 0
    # positions_svxy <- posit
    
    # Calculate indicator of flipping the positions
    indic <- rutils::diffit(posit)
    # Calculate number of trades
    values$ntrades <- sum(abs(indic) > 0)
    
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
    pnls <- pnls*sd(returns[returns<0])/sd(pnls[pnls<0])
    
    # Bind together strategy pnls
    pnls <- cbind(returns, pnls)
    
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
    
    cat("Plotting for ", input$symbol, "\n")
    
    # Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Get Sharpe ratios
    sharper <- values$sharper
    # Get number of trades
    ntrades <- values$ntrades
    
    captiont <- paste("Strategy for", input$symbol, "Regression Z-score / \n", 
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
