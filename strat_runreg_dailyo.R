##############################
# This is a shiny app for simulating a contrarian strategy based 
# on the z-scores from running regressions of daily returns, using 
# the function run_reg(). 
# It trades VTI based on the z-scores from running regressions of 
# VXX returns versus the predictors SVXY returns and the rolling 
# volatilities of VTI and SVXY.
# The model flips the position only if the indicator persists over 
# several consecutive periods equal to lagg.
# It uses reactive code to avoid unnecessary calculations.
# This is the best performing univariate strategy.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(rutils)
library(shiny)
library(dygraphs)

## Model and data setup

captiont <- paste("Running Z-score of SVXY Prices Versus VXX app_runreg_daily_strat.R")

# Variable setup for testing
# symboln <- "VTI"
# symbolp <- "VXX"
# symbolr <- "SVXY"
# symbolv <- c(symbolr, symboln, symbolp)
# lambda <- 0.8
# threshv <- 1
# threshold2 <- (-1)
# coeff <- (-1)
# lagg <- 1
# pricev <- log(na.omit(rutils::etfenv$prices[, symbolv]))
# retv <- rutils::diffit(prices[, 2])
# Or
# retv <- na.omit(rutils::etfenv$returns[, symbolv])

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbols
    column(width=2, selectInput("symboln", label="Symbol to Trade",
                                choices=rutils::etfenv$symbolv, selected="VTI")),
    column(width=2, selectInput("symbolp", label="Symbol for Predictor",
                                choices=rutils::etfenv$symbolv, selected="SVXY")),
    column(width=2, selectInput("symbolr", label="Symbol for Response",
                                choices=rutils::etfenv$symbolv, selected="VXX")),
    # Input VIX symbol
    # column(width=2, selectInput("symbol_vix", label="Symbol VIX",
    #                             choices=c("VXX", "SVXY"), selected="VXX")),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-ask spread
    column(width=2, numericInput("bidask", label="Bid-ask:", value=0.0000, step=0.0001))
  ),  # end fluidRow

  fluidRow(
    # Input look-back interval
    # column(width=2, sliderInput("lookb", label="Look-back", min=2, max=100, value=50, step=1)),
    column(width=3, sliderInput("lambda", label="lambda:", min=0.98, max=0.999, value=0.989, step=0.001)),
    # Input threshold interval
    column(width=3, sliderInput("threshv", label="Threshold", min=(0.1), max=1.3, value=0.68, step=0.02)),
    # column(width=3, sliderInput("threshold2", label="Threshold2", min=(-1), max=0, value=(-0.3), step=0.1)),
    # Input the strategy coefficient: coeff=1 for momentum, and coeff=-1 for contrarian
    column(width=2, selectInput("coeff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # column(width=2, sliderInput("lookb", label="lookb:", min=1, max=21, value=5, step=1)),
    # column(width=2, sliderInput("slow_back", label="slow_back:", min=11, max=251, value=151, step=1)),
    # Input the trade lag
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=8, value=1, step=1))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot", width="100%", height="600px"), height=10, width=12)

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  ## Create an empty list of reactive values.
  globals <- reactiveValues()

  
  ## Calculate the returns
  retv <- shiny::reactive({
    
    symboln <- input$symboln # Symbol to trade
    symbolr <- input$symbolr # Symbol for response
    symbolp <- input$symbolp # Symbol for predictor
    cat("Loading the data for ", symboln, "\n")
    
    # Load the data
    symbolv <- c(symbolr, symboln, symbolp)

    na.omit(rutils::etfenv$returns[, symbolv])
    # na.omit(mget(symbolv, rutils::etfenv$returns))
    # na.omit(cbind(
    #   get(symboln, rutils::etfenv$returns),
    #   get(symbolp, rutils::etfenv$returns),
    #   get(symbolr, rutils::etfenv$returns)))
    
  })  # end Load the data
  
  ## Calculate the z-scores
  zscores <- shiny::reactive({
    
    cat("Calculating the z-scores", "\n")
    lambda <- input$lambda
    
    # Calculate the response and predictor
    retv <- retv()
    datev <- zoo::index(retv)
    respv <- retv[, 1]
    predm <- retv[, -1]

    # Calculate the trailing volatility of trading stock
    symboln <- input$symboln
    ohlc <- log(get(symboln, rutils::etfenv))
    volv <- HighFreq::run_var_ohlc(ohlc[datev], lambda=lambda)
    volv[volv == 0] <- 1
    volv <- sqrt(volv)
    # volv <- rutils::diffit(volv)
    volv[1] <- volv[2]
    predm <- cbind(predm, volv)
    
    # Calculate the trailing volatility of predictor stock
    symbolp <- input$symbolp
    ohlc <- log(get(symbolp, rutils::etfenv))
    volv <- HighFreq::run_var_ohlc(ohlc[datev], lambda=lambda)
    volv[volv == 0] <- 1
    volv <- sqrt(volv)
    # volv <- rutils::diffit(volv)
    volv[1] <- volv[2]
    predm <- cbind(predm, volv)
    
    # Add intercept column to predictor matrix
    # predm <- cbind(rep(1, NROW(predm)), predm)
    
    # symbolr <- input$symbolr
    # ohlc <- log(get(symbolr, rutils::etfenv))
    # volv <- HighFreq::run_var_ohlc(ohlc[datev], lambda=lambda)
    # volv[volv == 0] <- 1
    # volv <- rutils::diffit(volv)
    # predm <- cbind(predm, volv)
    
    # Create a list of regression parameters
    controlv <- HighFreq::param_reg(residscale="scale")
    # Calculate the trailing z-scores
    zscores <- HighFreq::run_reg(respv=respv, predm=predm, lambda=lambda, controlv=controlv)
    zscores <- zscores[, NCOL(predm)+2, drop=FALSE]
    # zscores[1:lookb] <- 0
    zscores[is.infinite(zscores)] <- 0
    zscores[is.na(zscores)] <- 0
    # Scale the zscores by the volatility of the zscores
    # meanv <- HighFreq::run_mean(zscores, lambda=lambda)
    # volat <- HighFreq::run_var(zscores, lambda=lambda)
    # volat <- sqrt(HighFreq::lagit(tseries=volat))
    # zscores <- ifelse(volat > 0, (zscores - meanv)/volat, 0)
    zscores
    
  })  # end Load the data
  

  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    symboln <- input$symboln
    cat("Recalculating strategy for ", symboln, "\n")
    # Get model parameters from input argument
    # lookb <- input$lookb
    coeff <- as.numeric(input$coeff)
    lagg <- input$lagg
    # lambda <- input$lambda
    
    retv <- retv()[, 2]
    zscores <- zscores()
    # retv <- returns/sd(retv)
    retsum <- cumsum(retv)
    nrows <- NROW(retv)

    # Calculate the trailing volatility
    # volv <- HighFreq::roll_var_ohlc(ohlc=vtis, lookb=lookb, scale=FALSE)

    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the indic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # posv <- ifelse(indic == indic_lag, indic, posv)
    
    # Flip position if the scaled returns exceed threshv
    threshv <- input$threshv
    # threshold2 <- input$threshold2
    
    # Scale the threshv by the volatility of the zscores
    # volv <- HighFreq::run_var(tseries=HighFreq::diffit(zscores), lambda=lambda)
    # volv <- HighFreq::lagit(tseries=volv)
    # threshv <- volv*threshv
    
    # zscores <- ifelse(volv > 0, zscores/sqrt(volv), 0)
    ##
    indic <- rep(NA_integer_, nrows)
    indic[1] <- 0
    indic[zscores > threshv] <- coeff
    indic[zscores < (threshv)] <- (-coeff)
    indic <- zoo::na.locf(indic, na.rm=FALSE)
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
    globals$ntrades <- sum(abs(indic)>0)
    
    # Add buy/sell indicators for annotations
    longi <- (indic > 0)
    shorti <- (indic < 0)
    
    # Lag the positions to trade in next period
    posv <- rutils::lagit(posv, lagg=1)
    
    # Calculate strategy pnls
    pnls <- posv*retv
    
    # Calculate transaction costs
    costs <- 0.5*input$bidask*abs(indic)
    pnls <- (pnls - costs)

    # Scale the pnls so they have same SD as returns
    pnls <- pnls*sd(retv[retv<0])/sd(pnls[pnls<0])
    
    # Bind together strategy pnls
    pnls <- cbind(retv, pnls)
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    globals$sharper <- round(sharper, 3)

    # Bind with indicators
    pnls <- cumsum(pnls)
    pnls <- cbind(pnls, retsum[longi], retsum[shorti])
    colnames(pnls) <- c(paste(input$symboln, "Returns"), "Strategy", "Buy", "Sell")

    pnls

  })  # end Recalculate the strategy
  

  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    cat("Plotting for ", input$symboln, "\n")

    # Get the zscores
    # zscores <- zscores()
    # retv <- retv()[, 1]
    
    # Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Get Sharpe ratios
    sharper <- globals$sharper
    # Get number of trades
    ntrades <- globals$ntrades
    
    captiont <- paste("Strategy for", input$symboln, "Regression Z-score / \n",
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
    
    # datav <- cbind(cumsum(retv), zscores)
    # colnames(datav) <- c("VTI", "Zscores")
    # colnamev <- colnames(datav)
    # dygraphs::dygraph(datav, main="VXX Zscores") %>%
    #     dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
    #     dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
    #     dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
    #     dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red")
    # dygraph(xts(zscores, index(retv)), main="VXX Zscores")

  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
