##############################
# This is a shiny app for simulating a logistic regression model
# to classify oversold and overbought extreme price points.
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
svx_y <- log(get("SVXY", rutils::etfenv))
svxy_close <- quantmod::Cl(svx_y)
nrows <- NROW(svx_y)
dates <- zoo::index(svx_y)
vx_x <- log(get("VXX", rutils::etfenv))
vx_x <- vx_x[dates]
vxx_close <- quantmod::Cl(vx_x)

cap_tion <- paste("Logistic Model for Oversold and Overbought Extreme Price Points")

## End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel(cap_tion),

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
    column(width=2, sliderInput("look_back", label="Look-back", min=3, max=51, value=21, step=2)),
    # Input confidence level for tops
    column(width=2, sliderInput("confi_top", label="Confidence for tops", min=0.01, max=0.99, value=0.98, step=0.01)),
    # Input confidence level for bottoms
    column(width=2, sliderInput("confi_bot", label="Confidence for bottoms", min=0.01, max=0.99, value=0.002, step=0.01)),
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
servfunc <- function(input, output) {

  # Create an empty list of reactive values.
  value_s <- reactiveValues()

  # Load the data
  fit_ted <- reactive({
    
    symbol <- input$symbol
    cat("Loading data for ", symbol, "\n")
    
    # Extract log OHLC prices
    ohlc <- get(symbol, rutils::etfenv)[dates]
    closep <- log(quantmod::Cl(ohlc))
    returns <- rutils::diffit(closep)
    
    cat("Recalculating GLM for ", symbol, "\n")
    # Get model parameters from input argument
    look_back <- input$look_back
    half_back <- look_back %/% 2
    
    # Calculate SVXY z-scores
    roll_svxy <- roll::roll_mean(svxy_close, width=look_back, min_obs=1)
    var_rolling <- sqrt(HighFreq::roll_var_ohlc(svx_y, look_back=look_back, scalit=FALSE))
    svxy_scores <- (svxy_close - roll_svxy)/var_rolling

    # Calculate VXX z-scores
    roll_vxx <- roll::roll_mean(vxx_close, width=look_back, min_obs=1)
    var_rolling <- sqrt(HighFreq::roll_var_ohlc(vx_x, look_back=look_back, scalit=FALSE))
    vxx_scores <- (vxx_close - roll_vxx)/var_rolling

    # Calculate the centered volatility
    volat <- roll::roll_sd(returns, width=look_back, min_obs=1)
    volat <- rutils::lagit(volat, lagg=(-half_back))
    
    # Calculate the z-scores of prices
    mid_p <- 1.n_rows  # mid point
    startp <- (mid_p - half_back)  # start point
    startp[1:half_back] <- 1
    endp <- (mid_p + half_back)  # end point
    endp[.n_rows-half_back+1).n_rows] <-.n_rows
    closep <- as.numeric(closep)
    z_scores <- (2*closep[mid_p] - closep[startp] - closep[endp])
    z_scores <- ifelse(volat > 0, z_scores/volat, 0)

    # Calculate the vectors of tops and bottoms
    thresh_top <- quantile(z_scores, input$confi_top)
    top_s <- (z_scores > thresh_top)
    thresh_bot <- quantile(z_scores, input$confi_bot)
    bottom_s <- (z_scores < thresh_bot)

    # Calculate volatility z-scores
    volat <- log(quantmod::Hi(ohlc))-log(quantmod::Lo(ohlc))
    roll_vol <- roll::roll_mean(volat, width=look_back, min_obs=1)
    volat_scores <- (volat - roll_vol)/roll_vol
    
    # Calculate volume z-scores
    volumes <- quantmod::Vo(ohlc)
    volume_mean <- roll::roll_mean(volumes, width=look_back, min_obs=1)
    volume_sd <- roll::roll_sd(rutils::diffit(volumes), width=look_back, min_obs=1)
    volume_scores <- (volumes - volume_mean)/volume_sd

    # Define design matrix
    design <- cbind(vxx_scores, svxy_scores, volat_scores, volume_scores)
    colnames(design) <- c("vxx", "svxy", "volat", "volume")

    # Fit logistic regression for tops
    glm_tops <- glm(top_s ~ design, family=binomial(logit))

    # Fit in-sample logistic regression for bottoms
    glm_bottoms <- glm(bottom_s ~ design, family=binomial(logit))

    # Calculate tops and bottoms from logistic regression
    fit_ted <- glm_tops$fitted.values
    thresh_top <- quantile(fit_ted, input$confi_top)
    top_s <- (fit_ted > thresh_top)
    fit_ted <- glm_bottoms$fitted.values
    thresh_bot <- quantile(fit_ted, input$confi_bot)
    bottom_s <- (fit_ted < thresh_bot)
    
    # Return fitted values
    fit_ted <- cbind(returns, top_s, bottom_s)
    colnames(fit_ted) <- c("returns", "tops", "bottoms")
    fit_ted
    
  })  # end Load the data
  

  # Recalculate the strategy
  pnls <- reactive({
    
    symbol <- input$symbol
    cat("Recalculating strategy for ", symbol, "\n")
    # Get model parameters from input argument
    coeff <- as.numeric(input$coeff)
    lagg <- input$lagg

    # Extract fitted values
    top_s <- as.logical(fit_ted()$tops)
    bottom_s <- as.logical(fit_ted()$bottoms)
    returns <- fit_ted()$returns
    
    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the indic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # position_s <- ifelse(indic == indic_lag, indic, position_s)
    
    indic <- rep(NA_integer_,.n_rows)
    indic[1] <- 0
    indic[bottom_s] <- coeff
    indic[top_s] <- (-coeff)
    indic <- zoo::na.locf(indic, na.rm=FALSE)
    indic_sum <- roll::roll_sum(indic, width=lagg, min_obs=1)
    indic_sum[1:lagg] <- 0
    
    position_s <- rep(NA_integer_,.n_rows)
    position_s[1] <- 0
    position_s <- ifelse(indic_sum == lagg, 1, position_s)
    position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
    position_s <- zoo::na.locf(position_s, na.rm=FALSE)
    position_s[1:lagg] <- 0

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
    cum_rets <- cumsum(returns)
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
