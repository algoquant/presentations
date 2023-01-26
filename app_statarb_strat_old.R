##############################
# This is a shiny app for simulating a contrarian stat-arb portfolio strategy
# based on the z-scores from regressions of retv, using function
# HighFreq::run_zscores(). 
# The strategy invests in a portfolio with weights equal to the betas. 
# The model flips the position only if the indicator persists over 
# several consecutive periods equal to lagg.

# This is an old version which uses run_zscores()
# It uses reactive code to avoid unnecessary calculations.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(rutils)
library(shiny)
library(dygraphs)

## Model and data setup

captiont <- paste("Stat-arb Portfolio Strategy app_statarb_strat.R")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbols
    column(width=2, selectInput("symbol", label="Symbol for Reference",
                                choices=rutils::etfenv$symbolv, selected="VTI")),
    column(width=2, selectInput("predictor1", label="Predictor1",
                                choices=rutils::etfenv$symbolv, selected="SVXY")),
    column(width=2, selectInput("predictor2", label="Predictor2",
                                choices=rutils::etfenv$symbolv, selected="VXX")),
    # Input VIX symbol
    # column(width=2, selectInput("symbol_vix", label="Symbol VIX",
    #                             choices=c("VXX", "SVXY"), selected="VXX")),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-offer spread
    column(width=2, numericInput("bid_offer", label="Bid-offer:", value=0.0000, step=0.0001))
  ),  # end fluidRow

  fluidRow(
    # Input look-back interval
    # column(width=2, sliderInput("look_back", label="Look-back", min=2, max=100, value=50, step=1)),
    column(width=3, sliderInput("lambda", label="lambda:", min=0.01, max=0.9, value=0.73, step=0.01)),
    # Input threshold interval
    column(width=3, sliderInput("threshold", label="Threshold", min=0.5, max=2.0, value=0.8, step=0.1)),
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

  ## Create an empty list of reactive values.
  values <- reactiveValues()

  
  ## Calculate the returns
  retv <- shiny::reactive({
    
    symbol <- input$symbol
    predictor1 <- input$predictor1
    predictor2 <- input$predictor2
    cat("Loading the data for ", symbol, "\n")
    
    # Load the data
    symbolv <- c(symbol, predictor1, predictor2)

    na.omit(rutils::etfenv$returns[, symbolv])
    # na.omit(mget(symbolv, rutils::etfenv$returns))
    # na.omit(cbind(
    #   get(symbol, rutils::etfenv$returns),
    #   get(predv1, rutils::etfenv$returns),
    #   get(predv2, rutils::etfenv$returns)))
    
  })  # end Load the data
  
  ## Calculate the z-scores
  zscores <- shiny::reactive({
    
    cat("Calculating the z-scores", "\n")
    lambda <- input$lambda
    
    # Calculate the response and predictor
    retv <- returns()
    respv <- retv[, 1]
    predv <- retv[, -1]

    # Calculate the trailing z-scores
    # zscores <- drop(HighFreq::roll_zscores(respv=respv, predictor=predv, look_back=look_back))
    zscores <- HighFreq::run_zscores(respv=respv, predictor=predv, lambda=lambda)
    # zscores <- zscores[, 1, drop=FALSE]
    # zscores[1:look_back] <- 0
    # zscores[is.infinite(zscores)] <- 0
    # zscores[is.na(zscores)] <- 0
    zscores
    
  })  # end Load the data
  

  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    symbol <- input$symbol
    cat("Recalculating strategy for ", symbol, "\n")
    # Get model parameters from input argument
    # look_back <- input$look_back
    coeff <- as.numeric(input$coeff)
    lagg <- input$lagg
    lambda <- input$lambda
    
    retv <- returns()
    zscores <- zscores()
    # retv <- returns/sd(retv)
    retsum <- xts::xts(cumsum(rowSums(retv)), zoo::index(retv))
    nrows <- NROW(retv)

    # Calculate rolling volatility
    # variance <- HighFreq::roll_var_ohlc(ohlc=vtis, look_back=look_back, scale=FALSE)

    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the indic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # posit <- ifelse(indic == indic_lag, indic, posit)
    
    # Flip position if the scaled returns exceed threshold
    threshold <- input$threshold
    
    # Scale the threshold by the volatility of the zscores
    variance <- HighFreq::run_var(tseries=HighFreq::diffit(zscores[, 1, drop=FALSE]), lambda=lambda)
    variance <- HighFreq::lagit(tseries=variance)
    threshold <- variance*threshold
    
    # zscores <- zscores/sqrt(look_back)
    indic <- rep(NA_integer_, nrows)
    indic[1] <- 0
    indic[zscores[, 1] > threshold] <- coeff
    indic[zscores[, 1] < (-threshold)] <- (-coeff)
    indic <- zoo::na.locf(indic, na.rm=FALSE)
    # indic_sum <- HighFreq::roll_vec(tseries=matrix(indic), look_back=lagg)
    # indic_sum[1:lagg] <- 0
    # Define z-score weights
    ncols <- (NCOL(zscores)-1)/2
    weights <- matrix(rep(NA_integer_, ncols*nrows), ncol=ncols)
    weights[1, ] <- 1
    betas <- zscores[, 2:(ncols+1)]
    indic <- (zscores[, 1] > threshold)
    weights[indic, ] <- coeff*betas[indic, ]
    indic <- (zscores[, 1] < (-threshold))
    weights[indic, ] <- -coeff*betas[indic, ]
    weights <- cbind(rep(1, nrows), weights)
    weights <- zoo::na.locf(weights, na.rm=FALSE)
    # positions_svxy <- posit
    
    # Calculate trailing z-scores of VXX
    # predv <- cbind(sqrt(variance), svxy, vti_close)
    # respv <- vxx
    # zscores <- drop(HighFreq::roll_zscores(respv=respv, predictor=predv, look_back=look_back))
    # zscores[1:look_back] <- 0
    # zscores[is.infinite(zscores)] <- 0
    # zscores[is.na(zscores)] <- 0
    # zscores <- zscores/sqrt(look_back)
    # indic <- rep(NA_integer_, nrows)
    # indic[1] <- 0
    # indic[zscores > threshold] <- coeff
    # indic[zscores < (-threshold)] <- (-coeff)
    # indic <- zoo::na.locf(indic, na.rm=FALSE)
    # indic_sum <- HighFreq::roll_vec(tseries=matrix(indic), look_back=lagg)
    # indic_sum[1:lagg] <- 0
    # posit <- rep(NA_integer_, nrows)
    # posit[1] <- 0
    # posit <- ifelse(indic_sum == lagg, 1, posit)
    # posit <- ifelse(indic_sum == (-lagg), -1, posit)
    # posit <- zoo::na.locf(posit, na.rm=FALSE)
    # posit[1:lagg] <- 0
    
    # posit <- positions_svxy + posit
    
    # Calculate indicator of flipping the positions
    indic <- rutils::diffit(indic)
    # Calculate number of trades
    values$ntrades <- sum(abs(indic)>0)
    
    # Add buy/sell indicators for annotations
    indic_buy <- (indic > 0)
    indic_sell <- (indic < 0)
    
    # Lag the weights to trade in next period
    weights <- rutils::lagit(weights, lagg=1)
    
    # Calculate strategy pnls
    pnls <- rowSums(weights*retv)
    
    # Calculate transaction costs
    costs <- 0.5*input$bid_offer*abs(indic)
    pnls <- (pnls - costs)

    # Scale the pnls so they have same SD as returns
    pnls <- pnls*sd(retv[retv[, 1]<0, 1])/sd(pnls[pnls<0])
    
    # Bind together strategy pnls
    pnls <- cbind(retv[, 1], pnls)
    
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
