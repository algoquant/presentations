##############################
# This is a shiny app for backtesting a Z-Scores strategy 
# trading at oversold and overbought extreme price points.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(rutils)
library(shiny)
library(dygraphs)

## Model and data setup

# Calculate SVXY and VXX prices
svxy <- log(get("SVXY", rutils::etfenv))
svxy_close <- quantmod::Cl(svxy)
nrows <- NROW(svxy)
dates <- zoo::index(svxy)
vxx <- log(get("VXX", rutils::etfenv))
vxx <- vxx[dates]
vxx_close <- quantmod::Cl(vxx)

captiont <- paste("Strategy for Oversold and Overbought Extreme Price Points")

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
    column(width=2, numericInput("bid_offer", label="Bid-offer:", value=0.0000, step=0.0001))
  ),  # end fluidRow

  fluidRow(
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Look-back", min=3, max=51, value=5, step=1)),
    # Input threshold for tops
    column(width=2, sliderInput("thresh_top", label="Threshold for Tops", min=0, max=1, value=0.1, step=0.01)),
    # Input threshold for bottoms
    column(width=2, sliderInput("thresh_bot", label="Threshold for Bottoms", min=(-1), max=0, value=(-0.1), step=0.01)),
    # Input weight for VXX
    column(width=2, sliderInput("weight_vxx", label="Weight for VXX", min=(-1), max=1, value=1, step=0.1)),
    # Input weight for SVXY
    # column(width=2, sliderInput("weightsvxy", label="Weight for SVXY", min=(-1), max=1, value=0, step=0.1)),
    # Input weight for Stock
    column(width=2, sliderInput("weightstock", label="Weight for Stock", min=(-1), max=1, value=0, step=0.1)),
    # Input weight for volatility
    column(width=2, sliderInput("weight_volat", label="Weight for Volatility", min=(-1), max=1, value=0, step=0.1)),
    # Input weight for volume
    column(width=2, sliderInput("weight_volume", label="Weight for Volume", min=(-1), max=1, value=0, step=0.1)),
    # Input the strategy coefficient: coeff=1 for momentum, and coeff=-1 for contrarian
    column(width=2, selectInput("coeff", "Coefficient:", choices=c(-1, 1), selected=(1))),
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

  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    # Get model parameters from input argument
    symbol <- input$symbol
    cat("Recalculating strategy for ", symbol, "\n")
    look_back <- input$look_back
    coeff <- as.numeric(input$coeff)
    lagg <- input$lagg
    
    # Extract log OHLC prices
    ohlc <- get(symbol, rutils::etfenv)[dates]
    closep <- log(quantmod::Cl(ohlc))
    returns <- rutils::diffit(closep)
    
    # Calculate SVXY z-scores
    in_deks <- matrix(1:nrows, nc=1)
    svxy_scores <- HighFreq::roll_reg(response=svxy_close, predictor=in_deks, look_back=look_back)
    svxy_scores <- svxy_scores[, NCOL(svxy_scores)]
    svxy_scores[1:look_back] <- 0
    svxy_scores[is.infinite(svxy_scores)] <- 0
    svxy_scores[is.na(svxy_scores)] <- 0
    svxy_scores <- svxy_scores/sqrt(look_back)
    # roll_svxy <- roll::roll_mean(svxy_close, width=look_back, min_obs=1)
    # var_rolling <- sqrt(HighFreq::roll_var_ohlc(svxy, look_back=look_back, scale=FALSE))
    # svxy_scores <- (svxy_close - roll_svxy)/var_rolling
    
    # Calculate VXX z-scores
    vxx_scores <- HighFreq::roll_reg(response=vxx_close, predictor=in_deks, look_back=look_back)
    vxx_scores <- vxx_scores[, NCOL(vxx_scores)]
    vxx_scores[1:look_back] <- 0
    vxx_scores[is.infinite(vxx_scores)] <- 0
    vxx_scores[is.na(vxx_scores)] <- 0
    vxx_scores <- vxx_scores/sqrt(look_back)
    # roll_vxx <- roll::roll_mean(vxx_close, width=look_back, min_obs=1)
    # var_rolling <- sqrt(HighFreq::roll_var_ohlc(vxx, look_back=look_back, scale=FALSE))
    # vxx_scores <- (vxx_close - roll_vxx)/var_rolling
    
    # Calculate stock z-scores
    stock_scores <- HighFreq::roll_reg(response=closep, predictor=in_deks, look_back=look_back)
    stock_scores <- stock_scores[, NCOL(stock_scores)]
    stock_scores[1:look_back] <- 0
    stock_scores[is.infinite(stock_scores)] <- 0
    stock_scores[is.na(stock_scores)] <- 0
    stock_scores <- stock_scores/sqrt(look_back)
    # roll_stock <- roll::roll_mean(closep, width=look_back, min_obs=1)
    # var_rolling <- sqrt(HighFreq::roll_var_ohlc(ohlc, look_back=look_back, scale=FALSE))
    # stock_scores <- (closep - roll_stock)/var_rolling

    # Calculate volatility z-scores
    volat <- log(quantmod::Hi(ohlc))-log(quantmod::Lo(ohlc))
    volat_scores <- HighFreq::roll_reg(response=volat, predictor=in_deks, look_back=look_back)
    volat_scores <- volat_scores[, NCOL(volat_scores)]
    volat_scores[1:look_back] <- 0
    volat_scores[is.infinite(volat_scores)] <- 0
    volat_scores[is.na(volat_scores)] <- 0
    volat_scores <- volat_scores/sqrt(look_back)
    # roll_vol <- roll::roll_mean(volat, width=look_back, min_obs=1)
    # var_rolling <- sqrt(HighFreq::roll_var(rutils::diffit(volat), look_back=look_back))
    # volat_scores <- (volat - roll_vol)/var_rolling
    
    # Calculate volume z-scores
    volumes <- quantmod::Vo(ohlc)
    volume_scores <- HighFreq::roll_reg(response=volumes, predictor=in_deks, look_back=look_back)
    volume_scores <- volume_scores[, NCOL(volume_scores)]
    volume_scores[1:look_back] <- 0
    volume_scores[is.infinite(volume_scores)] <- 0
    volume_scores[is.na(volume_scores)] <- 0
    volume_scores <- volume_scores/sqrt(look_back)
    # roll_volume <- roll::roll_mean(volumes, width=look_back, min_obs=1)
    # var_rolling <- sqrt(HighFreq::roll_var(rutils::diffit(volumes), look_back=look_back))
    # volume_scores <- (volumes - roll_volume)/var_rolling
    
    # Define predictor matrix
    predictor <- cbind(vxx_scores - svxy_scores, volat_scores, stock_scores, volume_scores)
    colnames(predictor) <- c("vxx", "stock", "volat", "volume")
    
    # Get weights parameters from input argument
    weights <- c(input$weight_vxx, input$weightstock, input$weight_volat, input$weight_volume)
    names(weights) <- c("vxx", "stock", "volat", "volume")
    
    # Simulate strategy
    # score <- xts(predictor %*% weights, order.by=dates)
    score <- drop(predictor %*% weights)
    # Calculate the vectors of tops and bottoms
    tops <- (score > input$thresh_top)
    bottoms <- (score < input$thresh_bot)

    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the indic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is predictored to prevent whipsaws and over-trading.
    # posit <- ifelse(indic == indic_lag, indic, posit)
    
    indic <- rep(NA_integer_, nrows)
    indic[1] <- 0
    indic[bottoms] <- coeff
    indic[tops] <- (-coeff)
    indic <- zoo::na.locf(indic, na.rm=FALSE)
    indic_sum <- roll::roll_sum(indic, width=lagg, min_obs=1)
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
    if (values$ntrades > 0)
      pnls <- pnls*sd(returns[returns<0])/sd(pnls[pnls<0])
    
    # Bind together strategy pnls
    pnls <- cbind(returns, pnls)
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)

    # Bind with indicators
    pnls <- cumsum(pnls)
    if (values$ntrades > 1) {
      cum_rets <- cumsum(returns)
      pnls <- cbind(pnls, cum_rets[indic_buy], cum_rets[indic_sell])
      colnames(pnls) <- c(paste(input$symbol, "Returns"), "Strategy", "Buy", "Sell")
    }  # end if

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
    # ntrades <- values$ntrades
    
    captiont <- paste("Strategy for", input$symbol, "Regression Z-score / \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
                      "Number of trades=", values$ntrades)
    
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
