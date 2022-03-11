##############################
# This is a shiny app for simulating a contrarian strategy based 
# on the z-scores from regressions of returns, using function 
# HighFreq::run_zscores(). 
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

cap_tion <- paste("Regression Z-score of SVXY Versus VXX")

## End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel(cap_tion),

  fluidRow(
    # Input stock symbols
    column(width=2, selectInput("symbol", label="Symbol to Trade",
                                choices=rutils::etfenv$symbolv, selected="VTI")),
    column(width=2, selectInput("predictor_symbol", label="Symbol for Predictor",
                                choices=rutils::etfenv$symbolv, selected="SVXY")),
    column(width=2, selectInput("response_symbol", label="Symbol for Response",
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
    column(width=3, sliderInput("lambdav", label="lambdav:", min=0.01, max=0.9, value=0.25, step=0.01)),
    # Input threshold interval
    column(width=3, sliderInput("threshold", label="Threshold", min=0.1, max=2.0, value=1.0, step=0.1)),
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
servfunc <- function(input, output) {

  ## Create an empty list of reactive values.
  value_s <- reactiveValues()

  
  ## Calculate the returns
  returns <- reactive({
    
    symbol <- input$symbol
    predictor_symbol <- input$predictor_symbol
    response_symbol <- input$response_symbol
    cat("Loading the data for ", symbol, "\n")
    
    # Load the data
    symbolv <- c(symbol, predictor_symbol, response_symbol)

    na.omit(rutils::etfenv$returns[, symbolv])
    # na.omit(mget(symbolv, rutils::etfenv$returns))
    # na.omit(cbind(
    #   get(symbol, rutils::etfenv$returns),
    #   get(predictor_symbol, rutils::etfenv$returns),
    #   get(response_symbol, rutils::etfenv$returns)))
    
  })  # end Load the data
  
  ## Calculate the z-scores
  z_scores <- reactive({
    
    cat("Calculating the z-scores", "\n")
    lambdav <- input$lambdav
    
    # Calculate the response and predictor
    returns <- returns()
    response <- returns[, 2]
    predictor <- returns[, -(1:2)]

    # Calculate the trailing z-scores
    # z_scores <- drop(HighFreq::roll_zscores(response=response, predictor=predictor, look_back=look_back))
    z_scores <- HighFreq::run_zscores(response=response, predictor=predictor, lambda=lambdav, demean=FALSE)
    z_scores <- z_scores[, 1, drop=FALSE]
    # z_scores[1:look_back] <- 0
    z_scores[is.infinite(z_scores)] <- 0
    z_scores[is.na(z_scores)] <- 0
    z_scores
    
  })  # end Load the data
  

  # Recalculate the strategy
  pnls <- reactive({
    
    symbol <- input$symbol
    cat("Recalculating strategy for ", symbol, "\n")
    # Get model parameters from input argument
    # look_back <- input$look_back
    coeff <- as.numeric(input$coeff)
    lagg <- input$lagg
    # lambdav <- input$lambdav
    
    returns <- returns()[, 1]
    z_scores <- z_scores()
    # returns <- returns/sd(returns)
    cum_rets <- cumsum(returns)
   .n_rows <- NROW(returns)

    # Calculate rolling volatility
    # variance <- HighFreq::roll_var_ohlc(ohlc=vtis, look_back=look_back, scale=FALSE)

    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the indic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # position_s <- ifelse(indic == indic_lag, indic, position_s)
    
    # Flip position if the scaled returns exceed threshold
    threshold <- input$threshold
    
    # Scale the threshold by the volatility of the z_scores
    variance <- HighFreq::run_var(tseries=HighFreq::diffit(z_scores), lambda=lambdav)
    variance <- HighFreq::lagit(tseries=variance)
    threshold <- variance*threshold
    
    # z_scores <- z_scores/sqrt(look_back)
    indic <- rep(NA_integer_,.n_rows)
    indic[1] <- 0
    indic[z_scores > threshold] <- coeff
    indic[z_scores < (-threshold)] <- (-coeff)
    indic <- zoo::na.locf(indic, na.rm=FALSE)
    indic_sum <- HighFreq::roll_vec(tseries=matrix(indic), look_back=lagg)
    indic_sum[1:lagg] <- 0
    position_s <- rep(NA_integer_,.n_rows)
    position_s[1] <- 0
    position_s <- ifelse(indic_sum == lagg, 1, position_s)
    position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
    position_s <- zoo::na.locf(position_s, na.rm=FALSE)
    position_s[1:lagg] <- 0
    # positions_svxy <- position_s
    
    # Calculate trailing z-scores of VXX
    # predictor <- cbind(sqrt(variance), svx_y, vti_close)
    # response <- vx_x
    # z_scores <- drop(HighFreq::roll_zscores(response=response, predictor=predictor, look_back=look_back))
    # z_scores[1:look_back] <- 0
    # z_scores[is.infinite(z_scores)] <- 0
    # z_scores[is.na(z_scores)] <- 0
    # z_scores <- z_scores/sqrt(look_back)
    # indic <- rep(NA_integer_,.n_rows)
    # indic[1] <- 0
    # indic[z_scores > threshold] <- coeff
    # indic[z_scores < (-threshold)] <- (-coeff)
    # indic <- zoo::na.locf(indic, na.rm=FALSE)
    # indic_sum <- HighFreq::roll_vec(tseries=matrix(indic), look_back=lagg)
    # indic_sum[1:lagg] <- 0
    # position_s <- rep(NA_integer_,.n_rows)
    # position_s[1] <- 0
    # position_s <- ifelse(indic_sum == lagg, 1, position_s)
    # position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
    # position_s <- zoo::na.locf(position_s, na.rm=FALSE)
    # position_s[1:lagg] <- 0
    
    # position_s <- positions_svxy + position_s
    
    # Calculate indicator of flipping the positions
    indic <- rutils::diffit(position_s)
    # Calculate number of trades
    value_s$n_trades <- sum(abs(indic)>0)
    
    # Add buy/sell indicators for annotations
    indic_buy <- (indic > 0)
    indic_sell <- (indic < 0)
    
    # Lag the positions to trade in next period
    position_s <- rutils::lagit(position_s, lagg=1)
    
    # Calculate strategy pnls
    pnls <- position_s*returns
    
    # Calculate transaction costs
    costs <- 0.5*input$bid_offer*abs(indic)
    pnls <- (pnls - costs)

    # Scale the pnls so they have same SD as returns
    pnls <- pnls*sd(returns[returns<0])/sd(pnls[pnls<0])
    
    # Bind together strategy pnls
    pnls <- cbind(returns, pnls)
    
    # Calculate Sharpe ratios
    sharp_e <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    value_s$sharp_e <- round(sharp_e, 3)

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
    sharp_e <- value_s$sharp_e
    # Get number of trades
    n_trades <- value_s$n_trades
    
    cap_tion <- paste("Strategy for", input$symbol, "Regression Z-score / \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharp_e, collapse=" / "), "/ \n",
                      "Number of trades=", n_trades)
    
    # Plot with annotations
    add_annotations <- input$add_annotations
    
    if (add_annotations == "True") {
      dygraphs::dygraph(pnls, main=cap_tion) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red") %>%
        dySeries(name=colnamev[3], axis="y", label=colnamev[3], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=colnamev[4], axis="y", label=colnamev[4], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == "False") {
      dygraphs::dygraph(pnls[, 1:2], main=cap_tion) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red")
    }  # end if
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfunc)
