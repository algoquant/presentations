##############################
# This is a shiny app for simulating a contrarian strategy based 
# on the z-scores from regressions of returns, using function 
# HighFreq::run_reg(). 
# The model flips the position only if the indicator persists over 
# several consecutive periods equal to lagg.
# This is the new version which uses run_reg()
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

cap_tion <- paste("Regression Z-score of SVXY Versus VXX app_zscore_returns_strat.R")

# Variable setup for testing
# predictor_symbol <- "VXX"
# response_symbol <- "SVXY"
# sym_bol <- "VTI"
# sym_bols <- c(sym_bol, predictor_symbol, response_symbol)
# lamb_da <- 0.8
# thresh_old1 <- 1
# thresh_old2 <- (-1)
# co_eff <- (-1)
# lagg <- 1
# re_turns <- na.omit(rutils::etf_env$re_turns[, sym_bols])

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(cap_tion),

  fluidRow(
    # Input stock symbols
    column(width=2, selectInput("sym_bol", label="Symbol to Trade",
                                choices=rutils::etf_env$sym_bols, selected="VTI")),
    column(width=2, selectInput("predictor_symbol", label="Symbol for Predictor",
                                choices=rutils::etf_env$sym_bols, selected="SVXY")),
    column(width=2, selectInput("response_symbol", label="Symbol for Response",
                                choices=rutils::etf_env$sym_bols, selected="VXX")),
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
    column(width=3, sliderInput("lamb_da", label="lamb_da:", min=0.01, max=0.99, value=0.81, step=0.01)),
    # Input threshold interval
    column(width=3, sliderInput("thresh_old1", label="Threshold1", min=(0.1), max=1.3, value=1.1, step=0.02)),
    # column(width=3, sliderInput("thresh_old2", label="Threshold2", min=(-1), max=0, value=(-0.3), step=0.1)),
    # Input the strategy coefficient: co_eff=1 for momentum, and co_eff=-1 for contrarian
    column(width=2, selectInput("co_eff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
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

  ## Create an empty list of reactive values.
  globals <- reactiveValues()

  
  ## Calculate the returns
  re_turns <- reactive({
    
    sym_bol <- input$sym_bol
    response_symbol <- input$response_symbol
    predictor_symbol <- input$predictor_symbol
    cat("Loading the data for ", sym_bol, "\n")
    
    # Load the data
    sym_bols <- c(sym_bol, response_symbol, predictor_symbol)

    na.omit(rutils::etf_env$re_turns[, sym_bols])
    # na.omit(mget(sym_bols, rutils::etf_env$re_turns))
    # na.omit(cbind(
    #   get(sym_bol, rutils::etf_env$re_turns),
    #   get(predictor_symbol, rutils::etf_env$re_turns),
    #   get(response_symbol, rutils::etf_env$re_turns)))
    
  })  # end Load the data
  
  ## Calculate the z-scores
  z_scores <- reactive({
    
    cat("Calculating the z-scores", "\n")
    lamb_da <- input$lamb_da
    
    # Calculate the res_ponse and predic_tor
    re_turns <- re_turns()
    res_ponse <- re_turns[, 2]
    predic_tor <- re_turns[, -(1:2)]

    # Calculate the trailing z-scores
    # z_scores <- drop(HighFreq::roll_zscores(response=res_ponse, predictor=predic_tor, look_back=look_back))
    z_scores <- HighFreq::run_reg(response=res_ponse, predictor=predic_tor, lambda=lamb_da, method="standardize")
    z_scores <- z_scores[, 1, drop=FALSE]
    # z_scores[1:look_back] <- 0
    z_scores[is.infinite(z_scores)] <- 0
    z_scores[is.na(z_scores)] <- 0
    # Scale the z_scores by the volatility of the z_scores
    # mea_n <- HighFreq::run_mean(z_scores, lambda=lamb_da)
    # vol_at <- HighFreq::run_var(z_scores, lambda=lamb_da)
    # vol_at <- sqrt(HighFreq::lag_it(tseries=vol_at))
    # z_scores <- ifelse(vol_at > 0, (z_scores - mea_n)/vol_at, 0)
    z_scores
    
  })  # end Load the data
  

  # Recalculate the strategy
  pnl_s <- reactive({
    
    sym_bol <- input$sym_bol
    cat("Recalculating strategy for ", sym_bol, "\n")
    # Get model parameters from input argument
    # look_back <- input$look_back
    co_eff <- as.numeric(input$co_eff)
    lagg <- input$lagg
    # lamb_da <- input$lamb_da
    
    re_turns <- re_turns()[, 1]
    z_scores <- z_scores()
    # re_turns <- re_turns/sd(re_turns)
    cum_rets <- cumsum(re_turns)
    n_rows <- NROW(re_turns)

    # Calculate rolling volatility
    # vari_ance <- HighFreq::roll_var_ohlc(ohlc=vt_i, look_back=look_back, scale=FALSE)

    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the in_dic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # position_s <- ifelse(in_dic == indic_lag, in_dic, position_s)
    
    # Flip position if the scaled returns exceed thresh_old1
    thresh_old1 <- input$thresh_old1
    # thresh_old2 <- input$thresh_old2
    
    # Scale the thresh_old1 by the volatility of the z_scores
    # vari_ance <- HighFreq::run_var(tseries=HighFreq::diff_it(z_scores), lambda=lamb_da)
    # vari_ance <- HighFreq::lag_it(tseries=vari_ance)
    # thresh_old1 <- vari_ance*thresh_old1
    
    # z_scores <- ifelse(vari_ance > 0, z_scores/sqrt(vari_ance), 0)
    ##
    in_dic <- rep(NA_integer_, n_rows)
    in_dic[1] <- 0
    in_dic[z_scores > thresh_old1] <- co_eff
    in_dic[z_scores < (thresh_old1)] <- (-co_eff)
    in_dic <- zoo::na.locf(in_dic, na.rm=FALSE)
    indic_sum <- HighFreq::roll_vec(tseries=matrix(in_dic), look_back=lagg)
    indic_sum[1:lagg] <- 0
    position_s <- rep(NA_integer_, n_rows)
    position_s[1] <- 0
    position_s <- ifelse(indic_sum == lagg, 1, position_s)
    position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
    position_s <- zoo::na.locf(position_s, na.rm=FALSE)
    position_s[1:lagg] <- 0

    # positions_svxy <- position_s
    # position_s <- -sign(z_scores+thresh_old1)

    # Calculate trailing z-scores of VXX
    # predic_tor <- cbind(sqrt(vari_ance), svx_y, vti_close)
    # res_ponse <- vx_x
    # z_scores <- drop(HighFreq::roll_zscores(response=res_ponse, predictor=predic_tor, look_back=look_back))
    # z_scores[1:look_back] <- 0
    # z_scores[is.infinite(z_scores)] <- 0
    # z_scores[is.na(z_scores)] <- 0
    # z_scores <- z_scores/sqrt(look_back)
    # in_dic <- rep(NA_integer_, n_rows)
    # in_dic[1] <- 0
    # in_dic[z_scores > thresh_old1] <- co_eff
    # in_dic[z_scores < (-thresh_old1)] <- (-co_eff)
    # in_dic <- zoo::na.locf(in_dic, na.rm=FALSE)
    # indic_sum <- HighFreq::roll_vec(tseries=matrix(in_dic), look_back=lagg)
    # indic_sum[1:lagg] <- 0
    # position_s <- rep(NA_integer_, n_rows)
    # position_s[1] <- 0
    # position_s <- ifelse(indic_sum == lagg, 1, position_s)
    # position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
    # position_s <- zoo::na.locf(position_s, na.rm=FALSE)
    # position_s[1:lagg] <- 0
    
    # position_s <- positions_svxy + position_s
    
    # Calculate indicator of flipping the positions
    in_dic <- rutils::diff_it(position_s)
    # Calculate number of trades
    globals$n_trades <- sum(abs(in_dic)>0)
    
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
    globals$sharp_e <- round(sharp_e, 3)

    # Bind with indicators
    pnl_s <- cumsum(pnl_s)
    pnl_s <- cbind(pnl_s, cum_rets[indic_buy], cum_rets[indic_sell])
    colnames(pnl_s) <- c(paste(input$sym_bol, "Returns"), "Strategy", "Buy", "Sell")

    pnl_s

  })  # end Recalculate the strategy
  

  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph({
    
    cat("Plotting for ", input$sym_bol, "\n")

    # Get the z_scores
    # z_scores <- z_scores()
    # re_turns <- re_turns()[, 1]
    
    # Get the pnl_s
    pnl_s <- pnl_s()
    col_names <- colnames(pnl_s)
    
    # Get Sharpe ratios
    sharp_e <- globals$sharp_e
    # Get number of trades
    n_trades <- globals$n_trades
    
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
    
    # da_ta <- cbind(cumsum(re_turns), z_scores)
    # colnames(da_ta) <- c("VTI", "Zscores")
    # col_names <- colnames(da_ta)
    # dygraphs::dygraph(da_ta, main="VXX Zscores") %>%
    #     dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
    #     dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
    #     dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="blue") %>%
    #     dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="red")
    # dygraph(xts(z_scores, index(re_turns)), main="VXX Zscores")

  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
