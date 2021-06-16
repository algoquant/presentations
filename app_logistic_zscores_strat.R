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
svx_y <- log(get("SVXY", rutils::etf_env))
svxy_close <- quantmod::Cl(svx_y)
n_rows <- NROW(svx_y)
date_s <- zoo::index(svx_y)
vx_x <- log(get("VXX", rutils::etf_env))
vx_x <- vx_x[date_s]
vxx_close <- quantmod::Cl(vx_x)

cap_tion <- paste("Logistic Model for Oversold and Overbought Extreme Price Points")

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(cap_tion),

  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("sym_bol", label="Symbol to Trade",
                                choices=rutils::etf_env$sym_bols, selected="VTI")),
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
    # Input the strategy coefficient: co_eff=1 for momentum, and co_eff=-1 for contrarian
    column(width=2, selectInput("co_eff", "Coefficient:", choices=c(-1, 1), selected=(1))),
    # column(width=2, sliderInput("look_back", label="look_back:", min=1, max=21, value=5, step=1)),
    # column(width=2, sliderInput("slow_back", label="slow_back:", min=11, max=251, value=151, step=1)),
    # Input the trade lag
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=8, value=1, step=1))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph", width="100%", height="600px"), height=10, width=12)

)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {

  # Create an empty list of reactive values.
  value_s <- reactiveValues()

  # Load the data
  fit_ted <- reactive({
    
    sym_bol <- input$sym_bol
    cat("Loading data for ", sym_bol, "\n")
    
    # Extract log OHLC prices
    oh_lc <- get(sym_bol, rutils::etf_env)[date_s]
    clos_e <- log(quantmod::Cl(oh_lc))
    re_turns <- rutils::diff_it(clos_e)
    
    cat("Recalculating GLM for ", sym_bol, "\n")
    # Get model parameters from input argument
    look_back <- input$look_back
    half_back <- look_back %/% 2
    
    # Calculate SVXY z-scores
    roll_svxy <- roll::roll_mean(svxy_close, width=look_back, min_obs=1)
    var_rolling <- sqrt(HighFreq::roll_var_ohlc(svx_y, look_back=look_back, scal_e=FALSE))
    svxy_scores <- (svxy_close - roll_svxy)/var_rolling

    # Calculate VXX z-scores
    roll_vxx <- roll::roll_mean(vxx_close, width=look_back, min_obs=1)
    var_rolling <- sqrt(HighFreq::roll_var_ohlc(vx_x, look_back=look_back, scal_e=FALSE))
    vxx_scores <- (vxx_close - roll_vxx)/var_rolling

    # Calculate the centered volatility
    vol_at <- roll::roll_sd(re_turns, width=look_back, min_obs=1)
    vol_at <- rutils::lag_it(vol_at, lagg=(-half_back))
    
    # Calculate the z-scores of prices
    mid_p <- 1:n_rows  # mid point
    start_p <- (mid_p - half_back)  # start point
    start_p[1:half_back] <- 1
    end_p <- (mid_p + half_back)  # end point
    end_p[(n_rows-half_back+1):n_rows] <- n_rows
    clos_e <- as.numeric(clos_e)
    z_scores <- (2*clos_e[mid_p] - clos_e[start_p] - clos_e[end_p])
    z_scores <- ifelse(vol_at > 0, z_scores/vol_at, 0)

    # Calculate the vectors of tops and bottoms
    thresh_top <- quantile(z_scores, input$confi_top)
    top_s <- (z_scores > thresh_top)
    thresh_bot <- quantile(z_scores, input$confi_bot)
    bottom_s <- (z_scores < thresh_bot)

    # Calculate volatility z-scores
    vol_at <- log(quantmod::Hi(oh_lc))-log(quantmod::Lo(oh_lc))
    roll_vol <- roll::roll_mean(vol_at, width=look_back, min_obs=1)
    volat_scores <- (vol_at - roll_vol)/roll_vol
    
    # Calculate volume z-scores
    vol_ume <- quantmod::Vo(oh_lc)
    volume_mean <- roll::roll_mean(vol_ume, width=look_back, min_obs=1)
    volume_sd <- roll::roll_sd(rutils::diff_it(vol_ume), width=look_back, min_obs=1)
    volume_scores <- (vol_ume - volume_mean)/volume_sd

    # Define design matrix
    de_sign <- cbind(vxx_scores, svxy_scores, volat_scores, volume_scores)
    colnames(de_sign) <- c("vxx", "svxy", "volat", "volume")

    # Fit logistic regression for tops
    glm_tops <- glm(top_s ~ de_sign, family=binomial(logit))

    # Fit in-sample logistic regression for bottoms
    glm_bottoms <- glm(bottom_s ~ de_sign, family=binomial(logit))

    # Calculate tops and bottoms from logistic regression
    fit_ted <- glm_tops$fitted.values
    thresh_top <- quantile(fit_ted, input$confi_top)
    top_s <- (fit_ted > thresh_top)
    fit_ted <- glm_bottoms$fitted.values
    thresh_bot <- quantile(fit_ted, input$confi_bot)
    bottom_s <- (fit_ted < thresh_bot)
    
    # Return fitted values
    fit_ted <- cbind(re_turns, top_s, bottom_s)
    colnames(fit_ted) <- c("returns", "tops", "bottoms")
    fit_ted
    
  })  # end Load the data
  

  # Recalculate the strategy
  pnl_s <- reactive({
    
    sym_bol <- input$sym_bol
    cat("Recalculating strategy for ", sym_bol, "\n")
    # Get model parameters from input argument
    co_eff <- as.numeric(input$co_eff)
    lagg <- input$lagg

    # Extract fitted values
    top_s <- as.logical(fit_ted()$tops)
    bottom_s <- as.logical(fit_ted()$bottoms)
    re_turns <- fit_ted()$returns
    
    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the in_dic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # position_s <- ifelse(in_dic == indic_lag, in_dic, position_s)
    
    in_dic <- rep(NA_integer_, n_rows)
    in_dic[1] <- 0
    in_dic[bottom_s] <- co_eff
    in_dic[top_s] <- (-co_eff)
    in_dic <- zoo::na.locf(in_dic, na.rm=FALSE)
    indic_sum <- roll::roll_sum(in_dic, width=lagg, min_obs=1)
    indic_sum[1:lagg] <- 0
    
    position_s <- rep(NA_integer_, n_rows)
    position_s[1] <- 0
    position_s <- ifelse(indic_sum == lagg, 1, position_s)
    position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
    position_s <- zoo::na.locf(position_s, na.rm=FALSE)
    position_s[1:lagg] <- 0

    # Calculate indicator of flipping the positions
    in_dic <- rutils::diff_it(position_s)
    # Calculate number of trades
    value_s$n_trades <- sum(abs(in_dic)>0)
    
    # Add buy/sell indicators for annotations
    indic_buy <- (in_dic > 0)
    indic_sell <- (in_dic < 0)
    
    # Lag the positions to trade in next period
    position_s <- rutils::lag_it(position_s, lagg=1)
    
    # Calculate strategy pnl_s
    pnl_s <- position_s*re_turns
    
    # Calculate transaction costs
    cost_s <- 0.5*input$bid_offer*abs(in_dic)
    pnl_s <- (pnl_s - cost_s)

    # Scale the pnl_s so they have same SD as re_turns
    pnl_s <- pnl_s*sd(re_turns[re_turns<0])/sd(pnl_s[pnl_s<0])
    
    # Bind together strategy pnl_s
    pnl_s <- cbind(re_turns, pnl_s)
    
    # Calculate Sharpe ratios
    sharp_e <- sqrt(252)*sapply(pnl_s, function(x) mean(x)/sd(x[x<0]))
    value_s$sharp_e <- round(sharp_e, 3)

    # Bind with indicators
    pnl_s <- cumsum(pnl_s)
    cum_rets <- cumsum(re_turns)
    pnl_s <- cbind(pnl_s, cum_rets[indic_buy], cum_rets[indic_sell])
    colnames(pnl_s) <- c(paste(input$sym_bol, "Returns"), "Strategy", "Buy", "Sell")

    pnl_s

  })  # end Recalculate the strategy
  

  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph({
    
    cat("Plotting for ", input$sym_bol, "\n")
    
    # Get the pnl_s
    pnl_s <- pnl_s()
    col_names <- colnames(pnl_s)
    
    # Get Sharpe ratios
    sharp_e <- value_s$sharp_e
    # Get number of trades
    n_trades <- value_s$n_trades
    
    cap_tion <- paste("Strategy for", input$sym_bol, "Regression Z-score / \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharp_e, collapse=" / "), "/ \n",
                      "Number of trades=", n_trades)
    
    # Plot with annotations
    add_annotations <- input$add_annotations
    
    if (add_annotations == "True") {
      dygraphs::dygraph(pnl_s, main=cap_tion) %>%
        dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
        dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="blue") %>%
        dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="red") %>%
        dySeries(name=col_names[3], axis="y", label=col_names[3], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=col_names[4], axis="y", label=col_names[4], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == "False") {
      dygraphs::dygraph(pnl_s[, 1:2], main=cap_tion) %>%
        dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
        dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="blue") %>%
        dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="red")
    }  # end if
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
