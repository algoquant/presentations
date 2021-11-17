##############################
# This is a shiny app for simulating a contrarian stat-arb portfolio strategy
# based on the z-scores from regressions of returns, using function
# HighFreq::run_zscores(). 
# The strategy invests in a portfolio with weights equal to the betas. 
# The model flips the position only if the indicator persists over 
# several consecutive periods equal to lagg.
# This is the new version which uses run_reg()
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

cap_tion <- paste("Stat-arb Portfolio Strategy app_statarb_strat.R")

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(cap_tion),

  fluidRow(
    # Input stock symbols
    column(width=2, selectInput("sym_bol", label="Symbol for Reference",
                                choices=rutils::etf_env$sym_bols, selected="VTI")),
    column(width=2, selectInput("predictor1", label="Predictor1",
                                choices=rutils::etf_env$sym_bols, selected="SVXY")),
    column(width=2, selectInput("predictor2", label="Predictor2",
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
    column(width=3, sliderInput("lamb_da", label="lamb_da:", min=0.01, max=0.9, value=0.59, step=0.01)),
    # Input threshold interval
    column(width=3, sliderInput("thresh_old", label="Threshold", min=0.3, max=2.0, value=0.8, step=0.05)),
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
  value_s <- reactiveValues()

  
  ## Calculate the returns
  re_turns <- reactive({
    
    sym_bol <- input$sym_bol
    predictor1 <- input$predictor1
    predictor2 <- input$predictor2
    cat("Loading the data for ", sym_bol, "\n")
    
    # Load the data
    sym_bols <- c(sym_bol, predictor1, predictor2)

    na.omit(rutils::etf_env$re_turns[, sym_bols])
    # na.omit(mget(sym_bols, rutils::etf_env$re_turns))
    # na.omit(cbind(
    #   get(sym_bol, rutils::etf_env$re_turns),
    #   get(predictor1, rutils::etf_env$re_turns),
    #   get(predictor2, rutils::etf_env$re_turns)))
    
  })  # end Load the data
  
  ## Calculate the rolling regressions
  regdata <- reactive({
    
    cat("Calculating the z-scores", "\n")
    lamb_da <- input$lamb_da
    
    # Calculate the res_ponse and predic_tor
    re_turns <- re_turns()
    res_ponse <- re_turns[, 1]
    predic_tor <- re_turns[, -1]

    # Calculate the trailing z-scores
    # regdata <- drop(HighFreq::roll_zscores(response=res_ponse, predictor=predic_tor, look_back=look_back))
    regdata <- HighFreq::run_reg(response=res_ponse, predictor=predic_tor, lambda=lamb_da, method="scale")
    # regdata <- regdata[, 1, drop=FALSE]
    # regdata[1:look_back] <- 0
    # regdata[is.infinite(regdata)] <- 0
    # regdata[is.na(regdata)] <- 0
    regdata
    
  })  # end Load the data
  

  # Recalculate the strategy
  pnl_s <- reactive({
    
    sym_bol <- input$sym_bol
    cat("Recalculating strategy for ", sym_bol, "\n")
    # Get model parameters from input argument
    # look_back <- input$look_back
    co_eff <- as.numeric(input$co_eff)
    lagg <- input$lagg
    lamb_da <- input$lamb_da
    
    re_turns <- re_turns()
    regdata <- regdata()
    # re_turns <- re_turns/sd(re_turns)
    cum_rets <- xts::xts(cumsum(rowSums(re_turns)), zoo::index(re_turns))
    n_rows <- NROW(re_turns)
    n_cols <- NCOL(regdata)
    
    # Calculate rolling volatility
    # vari_ance <- HighFreq::roll_var_ohlc(ohlc=vt_i, look_back=look_back, scale=FALSE)

    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the in_dic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # position_s <- ifelse(in_dic == indic_lag, in_dic, position_s)
    
    # Flip position if the scaled returns exceed thresh_old
    thresh_old <- input$thresh_old
    
    # Scale the thresh_old by the volatility of the z_scores
    z_scores <- regdata[, 1, drop=FALSE]
    vari_ance <- HighFreq::run_var(tseries=HighFreq::diff_it(z_scores), lambda=lamb_da)
    vari_ance <- HighFreq::lag_it(tseries=vari_ance)
    thresh_old <- vari_ance*thresh_old
    
    # z_scores <- z_scores/sqrt(look_back)
    in_dic <- rep(NA_integer_, n_rows)
    in_dic[1] <- 0
    in_dic[z_scores > thresh_old] <- co_eff
    in_dic[z_scores < (-thresh_old)] <- (-co_eff)
    in_dic <- zoo::na.locf(in_dic, na.rm=FALSE)
    # indic_sum <- HighFreq::roll_vec(tseries=matrix(in_dic), look_back=lagg)
    # indic_sum[1:lagg] <- 0
    # Define z-score weights
    # n_cols <- (NCOL(z_scores)-1)/2
    weight_s <- matrix(rep(NA_integer_, (n_cols-2)*n_rows), ncol=(n_cols-2))
    weight_s[1, ] <- 1
    beta_s <- regdata[, 3:n_cols]
    se_lect <- (z_scores > thresh_old)
    weight_s[se_lect, ] <- co_eff*beta_s[se_lect, ]
    se_lect <- (z_scores < (-thresh_old))
    weight_s[se_lect, ] <- -co_eff*beta_s[se_lect, ]
    weight_s <- cbind(rep(1, n_rows), weight_s)
    weight_s <- zoo::na.locf(weight_s, na.rm=FALSE)
    # positions_svxy <- position_s
    
    # Calculate trailing z-scores of VXX
    # predic_tor <- cbind(sqrt(vari_ance), svx_y, vti_close)
    # res_ponse <- vx_x
    # z_scores <- drop(HighFreq::roll_zscores(response=res_ponse, design=predic_tor, look_back=look_back))
    # z_scores[1:look_back] <- 0
    # z_scores[is.infinite(z_scores)] <- 0
    # z_scores[is.na(z_scores)] <- 0
    # z_scores <- z_scores/sqrt(look_back)
    # in_dic <- rep(NA_integer_, n_rows)
    # in_dic[1] <- 0
    # in_dic[z_scores > thresh_old] <- co_eff
    # in_dic[z_scores < (-thresh_old)] <- (-co_eff)
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
    in_dic <- rutils::diff_it(in_dic)
    # Calculate number of trades
    value_s$n_trades <- sum(abs(in_dic)>0)
    
    # Add buy/sell indicators for annotations
    indic_buy <- (in_dic > 0)
    indic_sell <- (in_dic < 0)
    
    # Lag the weights to trade in next period
    weight_s <- rutils::lag_it(weight_s, lagg=1)
    
    # Calculate strategy pnl_s
    pnl_s <- rowSums(weight_s*re_turns)
    
    # Calculate transaction costs
    cost_s <- 0.5*input$bid_offer*abs(in_dic)
    pnl_s <- (pnl_s - cost_s)

    # Scale the pnl_s so they have same SD as re_turns
    pnl_s <- pnl_s*sd(re_turns[re_turns[, 1]<0, 1])/sd(pnl_s[pnl_s<0])
    
    # Bind together strategy pnl_s
    pnl_s <- cbind(re_turns[, 1], pnl_s)
    
    # Calculate Sharpe ratios
    sharp_e <- sqrt(252)*sapply(pnl_s, function(x) mean(x)/sd(x[x<0]))
    value_s$sharp_e <- round(sharp_e, 3)

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
