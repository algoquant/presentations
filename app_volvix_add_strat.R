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

## End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
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
    column(width=2, numericInput("bid_offer", label="Bid-offer:", value=0.0000, step=0.0001))
  ),  # end fluidRow

  fluidRow(
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Look-back", min=2, max=100, value=50, step=1)),
    # Input threshold interval
    column(width=2, sliderInput("threshold", label="Threshold", min=0.01, max=0.5, value=0.1, step=0.01)),
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
    
    get(symbol, rutils::etfenv)[dates]

  })  # end Load the data
  

  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    symbol <- input$symbol
    cat("Recalculating strategy for ", symbol, "\n")
    # Get model parameters from input argument
    look_back <- input$look_back
    coeff <- as.numeric(input$coeff)
    lagg <- input$lagg

    # Calculate cumulative returns
    ohlc <- ohlc()
    closep <- log(quantmod::Cl(ohlc))
    returns <- rutils::diffit(closep)
    # returns <- returns/sd(returns)
    cum_rets <- cumsum(returns)
    nrows <- NROW(returns)

    # Calculate rolling volatility
    variance <- HighFreq::roll_var_ohlc(ohlc=vtis, look_back=look_back, scale=FALSE)

    # Calculate trailing SVXY z-scores
    predictor <- cbind(sqrt(variance), vxx, vti_close)
    response <- svxy
    svxy_scores <- drop(HighFreq::roll_zscores(response=response, predictor=predictor, look_back=look_back))
    svxy_scores[1:look_back] <- 0
    svxy_scores[is.infinite(svxy_scores)] <- 0
    svxy_scores[is.na(svxy_scores)] <- 0

    # Calculate trailing VXX z-scores
    predictor <- cbind(sqrt(variance), svxy, vti_close)
    response <- vxx
    vxx_scores <- drop(HighFreq::roll_zscores(response=response, predictor=predictor, look_back=look_back))
    vxx_scores[1:look_back] <- 0
    vxx_scores[is.infinite(vxx_scores)] <- 0
    vxx_scores[is.na(vxx_scores)] <- 0
    
    zscores <- (svxy_scores + vxx_scores)/sqrt(look_back)
    
    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the indic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is predictored to prevent whipsaws and over-trading.
    # posit <- ifelse(indic == indic_lag, indic, posit)
    
    indic <- rep(NA_integer_, nrows)
    indic[1] <- 0
    # Flip position if the scaled returns exceed threshold
    threshold <- input$threshold
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
    pnls <- pnls*sd(returns[returns<0])/sd(pnls[pnls<0])
    
    # Bind together strategy pnls
    pnls <- cbind(returns, pnls)
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)

    # Bind with indicators
    pnls <- cumsum(pnls)
    pnls <- cbind(pnls, cum_rets[indic_buy], cum_rets[indic_sell])
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
shiny::shinyApp(ui=uiface, server=servfun)
