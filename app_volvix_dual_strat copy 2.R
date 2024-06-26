##############################
# This is a shiny app for simulating a contrarian strategy 
# using the z-scores of rolling regressions of SVXY prices 
# versus VXX and the rolling volatility of SPY as predictors.
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
load(file="/Users/jerzy/Develop/data/SVXY_minute.RData")
svxy <- quantmod::Cl(ohlc)
colnames(svxy) <- "SVXY"
load(file="/Users/jerzy/Develop/data/VXX_minute.RData")
vxx <- quantmod::Cl(ohlc)
colnames(vxx) <- "VXX"
load(file="/Users/jerzy/Develop/data/SPY_minute.RData")
spy <- quantmod::Cl(ohlc)
colnames(spy) <- "SPY"

datam <- na.omit(cbind(spy, svxy, vxx))
datam <- datam["T09:30:00/T16:00:00"]
datev <- zoo::index(datam)
ohlc <- ohlc[datev]
spy <- datam$SPY
vxx <- datam$VXX
svxy <- datam$SVXY

# Calculate cumulative returns
retv <- rutils::diffit(spy)
retsum <- cumsum(retv)
nrows <- NROW(retv)
# Set to zero large overnight returns
retv[abs(retv) > 3*sd(retv)] <- 0


captiont <- paste("Regression Z-score of VXX and SVXY Prices Versus SPY Volatility")


## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbol
    # column(width=1, selectInput("symboln", label="Symbol to Trade",
    #                             choices=rutils::etfenv$symbolv, selected="VTI")),
    # Input VIX symbol
    # column(width=2, selectInput("symbol_vix", label="Symbol VIX",
    #                             choices=c("VXX", "SVXY"), selected="VXX")),
    # Input decay parameter
    column(width=2, sliderInput("lambda", label="lambda:", min=0.1, max=0.99, value=0.98, step=0.01)),
    # Input look-back interval
    # column(width=2, sliderInput("lookb", label="Look-back", min=2, max=100, value=41, step=1)),
    # Input threshold interval
    column(width=2, sliderInput("threshv", label="Threshold", min=0.1, max=1.3, value=1.0, step=0.02)),
    # Input add annotations Boolean
    column(width=1, selectInput("add_annotations", label="Buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the strategy coefficient: coeff=1 for momentum, and coeff=-1 for contrarian
    column(width=1, selectInput("coeff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    column(width=1, sliderInput("lagg", label="lagg", min=1, max=4, value=1, step=1)),
    # Input the bid-ask spread
    column(width=1, numericInput("bidask", label="Bid-ask:", value=0.0, step=0.0001))
  ),  # end fluidRow

  # fluidRow(
    # Input look-back interval
    # column(width=2, sliderInput("lookb", label="Look-back", min=2, max=100, value=41, step=1)),
    # Input threshold interval
    # column(width=2, sliderInput("threshold", label="Threshold", min=0.001, max=0.1, value=0.03, step=0.001)),
    # Input the strategy coefficient: coeff=1 for momentum, and coeff=-1 for contrarian
    # column(width=1, selectInput("coeff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # column(width=2, sliderInput("lookb", label="lookb:", min=1, max=21, value=5, step=1)),
    # column(width=2, sliderInput("slow_back", label="slow_back:", min=11, max=251, value=151, step=1)),
    # Input the trade lag
    # column(width=2, sliderInput("lagg", label="lagg", min=1, max=8, value=1, step=1))
  # ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()

  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    # symboln <- input$symboln
    # cat("Recalculating strategy for ", symboln, "\n")
    cat("Recalculating strategy\n")
    # Get model parameters from input argument
    lambda <- input$lambda
    # lookb <- input$lookb
    coeff <- as.numeric(input$coeff)
    lagg <- input$lagg

    # Calculate the rolling variance
    volv <- HighFreq::run_var_ohlc(ohlc, lambda=lambda)
    volv <- sqrt(volv)
    # volv <- HighFreq::roll_var_ohlc(ohlc=ohlc, lookb=lookb, scale=FALSE)
    # volv[volv == 0] <- 1
    
    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the indic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # posv <- ifelse(indic == indic_lag, indic, posv)
    
    # Flip position if the scaled returns exceed threshold
    threshv <- input$threshv
    
    # Calculate trailing z-scores of SVXY
    predm <- cbind(volv, vxx, spy)
    # Get rid of NAs in VXX prices - VXX prices from 2019-02 to 2019-05 are missing
    # predm <- na.omit(predm)
    predm <- lapply(predm, function(x) {
      x[is.na(x)] <- 0
      x
    }) # end lapply
    predm <- rutils::do_call(cbind, predm)
    # sum(is.na(predm))
    # Add intercept column to the predictor matrix
    predm <- cbind(rep(1, nrows), predm)
    
    # Response equals SVXY returns
    respv <- svxy
    
    controlv <- HighFreq::param_reg(residscale="scale")
    zscores <- HighFreq::run_reg(respv=respv, predm=predm, lambda=lambda, controlv=controlv)
    # rollreg <- HighFreq::roll_reg(respv=respv, predm=predm, lookb=lookb, controlv=controlv)
    zscores <- zscores[, NCOL(predm)+1, drop=TRUE]
    # zscores <- rollreg[, NCOL(rollreg), drop=TRUE]
    # zscores[1:lookb] <- 0
    # zscores[is.infinite(zscores)] <- 0
    
    # zscores[is.na(zscores)] <- 0
    # zscores <- zscores/sqrt(lookb)
    indic <- rep(NA_integer_, nrows)
    indic[1] <- 0
    indic[zscores > threshv] <- coeff
    indic[zscores < (-threshv)] <- (-coeff)
    indic <- zoo::na.locf(indic, na.rm=FALSE)
    indics <- HighFreq::roll_sum(tseries=matrix(indic), lookb=lagg)
    indics[1:lagg] <- 0
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    posv <- ifelse(indics == lagg, 1, posv)
    posv <- ifelse(indics == (-lagg), -1, posv)
    posv <- zoo::na.locf(posv, na.rm=FALSE)
    posv[1:lagg] <- 0
    # positions_svxy <- posv

    # posv <- positions_svxy + posv
    
    # Calculate indicator of flipping the positions
    indic <- rutils::diffit(posv)
    # Calculate number of trades
    values$ntrades <- sum(abs(indic) > 0)
    
    # Add buy/sell indicators for annotations
    indicl <- (indic > 0)
    indics <- (indic < 0)
    
    # Lag the positions to trade in next period
    posv <- rutils::lagit(posv, lagg=1)
    
    # Calculate strategy pnls
    pnls <- posv*retv
    
    # Calculate transaction costs
    costs <- 0.5*input$bidask*abs(indic)
    pnls <- (pnls - costs)

    # Scale the pnls so they have same SD as retv
    pnls <- pnls*sd(retv[retv<0])/sd(pnls[pnls<0])
    
    # Bind together strategy pnls
    pnls <- cbind(retv, pnls)
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)

    # Bind with indicators
    pnls <- cumsum(pnls)
    pnls <- cbind(pnls, retsum[indicl], retsum[indics])
    colnames(pnls) <- c("SPY Returns", "Strategy", "Buy", "Sell")

    pnls

  })  # end Recalculate the strategy
  

  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    cat("Plotting\n")
    
    # Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Get Sharpe ratios
    sharper <- values$sharper
    # Get number of trades
    ntrades <- values$ntrades
    
    captiont <- paste("Strategy for SPY", "Regression Z-score / \n", 
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
        dySeries(name=colnamev[4], axis="y", drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green") %>%
        dyLegend(show="always", width=300)
    } else if (add_annotations == "False") {
      dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red") %>%
        dyLegend(show="always", width=300)
    }  # end if
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
