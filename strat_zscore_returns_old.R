##############################
# This is a shiny app for simulating a contrarian strategy based 
# on the z-scores from regressions of retv, using function 
# HighFreq::run_zscores(). 
# The model flips the position only if the indicator persists over 
# several consecutive periods equal to lagg.

# This is an old version which uses run_zscores()
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

captiont <- paste("Regression Z-score of SVXY Versus VXX app_zscore_returns_strat.R")

# Variables for testing
# symbolpred <- "VXX"
# symbolresp <- "SVXY"
# symbol <- "VTI"
# symbolv <- c(symbol, symbolpred, symbolresp)
# lambda <- 0.8
# threshold1 <- 1
# threshold2 <- (-1)
# coeff <- (-1)
# lagg <- 1
# retv <- na.omit(rutils::etfenv$returns[, symbolv])

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbols
    column(width=2, selectInput("symbol", label="Symbol to Trade",
                                choices=rutils::etfenv$symbolv, selected="VTI")),
    column(width=2, selectInput("symbolpred", label="Symbol for Predictor",
                                choices=rutils::etfenv$symbolv, selected="SVXY")),
    column(width=2, selectInput("symbolresp", label="Symbol for Response",
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
    column(width=3, sliderInput("lambda", label="lambda:", min=0.01, max=0.9, value=0.7, step=0.01)),
    # Input threshold interval
    column(width=3, sliderInput("threshold1", label="Threshold1", min=0, max=1, value=0.0, step=0.1)),
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
    
    symbol <- input$symbol
    symbolresp <- input$symbolresp
    symbolpred <- input$symbolpred
    cat("Loading the data for ", symbol, "\n")
    
    # Load the data
    symbolv <- c(symbol, symbolresp, symbolpred)

    na.omit(rutils::etfenv$returns[, symbolv])
    # na.omit(mget(symbolv, rutils::etfenv$returns))
    # na.omit(cbind(
    #   get(symbol, rutils::etfenv$returns),
    #   get(predv_symbol, rutils::etfenv$returns),
    #   get(respv_symbol, rutils::etfenv$returns)))
    
  })  # end Load the data
  
  ## Calculate the z-scores
  zscores <- shiny::reactive({
    
    cat("Calculating the z-scores", "\n")
    lambda <- input$lambda
    
    # Calculate the response and predictor
    retv <- returns()
    respv <- retv[, 2]
    predv <- retv[, -(1:2)]

    # Calculate the trailing z-scores
    # zscores <- drop(HighFreq::roll_zscores(respv=respv, predictor=predv, lookb=lookb))
    zscores <- HighFreq::run_zscores(respv=respv, predictor=predv, lambda=lambda, demean=FALSE)
    zscores <- zscores[, 1, drop=FALSE]
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
    
    symbol <- input$symbol
    cat("Recalculating strategy for ", symbol, "\n")
    # Get model parameters from input argument
    # lookb <- input$lookb
    coeff <- as.numeric(input$coeff)
    lagg <- input$lagg
    # lambda <- input$lambda
    
    retv <- returns()[, 1]
    zscores <- zscores()
    # retv <- returns/sd(retv)
    retsum <- cumsum(retv)
    nrows <- NROW(retv)

    # Calculate rolling volatility
    # variance <- HighFreq::roll_var_ohlc(ohlc=vtis, lookb=lookb, scale=FALSE)

    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the indic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # posv <- ifelse(indic == indic_lag, indic, posv)
    
    # Flip position if the scaled returns exceed threshold1
    threshold1 <- input$threshold1
    # threshold2 <- input$threshold2
    
    # Scale the threshold1 by the volatility of the zscores
    # variance <- HighFreq::run_var(tseries=HighFreq::diffit(zscores), lambda=lambda)
    # variance <- HighFreq::lagit(tseries=variance)
    # threshold1 <- variance*threshold1
    
    # zscores <- ifelse(variance > 0, zscores/sqrt(variance), 0)
    ##
    indic <- rep(NA_integer_, nrows)
    indic[1] <- 0
    indic[zscores > threshold1] <- coeff
    indic[zscores < (threshold1)] <- (-coeff)
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
    # posv <- -sign(zscores+threshold1)

    # Calculate trailing z-scores of VXX
    # predv <- cbind(sqrt(variance), svxy, vti_close)
    # respv <- vxx
    # zscores <- drop(HighFreq::roll_zscores(respv=respv, predictor=predv, lookb=lookb))
    # zscores[1:lookb] <- 0
    # zscores[is.infinite(zscores)] <- 0
    # zscores[is.na(zscores)] <- 0
    # zscores <- zscores/sqrt(lookb)
    # indic <- rep(NA_integer_, nrows)
    # indic[1] <- 0
    # indic[zscores > threshold1] <- coeff
    # indic[zscores < (-threshold1)] <- (-coeff)
    # indic <- zoo::na.locf(indic, na.rm=FALSE)
    # indics <- HighFreq::roll_sum(tseries=matrix(indic), lookb=lagg)
    # indics[1:lagg] <- 0
    # posv <- rep(NA_integer_, nrows)
    # posv[1] <- 0
    # posv <- ifelse(indics == lagg, 1, posv)
    # posv <- ifelse(indics == (-lagg), -1, posv)
    # posv <- zoo::na.locf(posv, na.rm=FALSE)
    # posv[1:lagg] <- 0
    
    # posv <- positions_svxy + posv
    
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
    globals$sharper <- round(sharper, 3)

    # Bind with indicators
    pnls <- cumsum(pnls)
    pnls <- cbind(pnls, retsum[longi], retsum[shorti])
    colnames(pnls) <- c(paste(input$symbol, "Returns"), "Strategy", "Buy", "Sell")

    pnls

  })  # end Recalculate the strategy
  

  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    cat("Plotting for ", input$symbol, "\n")

    # Get the zscores
    # zscores <- zscores()
    # retv <- returns()[, 1]
    
    # Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Get Sharpe ratios
    sharper <- globals$sharper
    # Get number of trades
    ntrades <- globals$ntrades
    
    captiont <- paste("Strategy for", input$symbol, "Regression Z-score / \n",
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
