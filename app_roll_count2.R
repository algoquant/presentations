##############################
# This is a shiny app for simulating a contrarian strategy 
# using the number of consecutive close_low or close_high.
# The contrarian strategy uses function roll_count().
# The function roll_count() calculates the the number of 
# consecutive TRUE elements in a Boolean vector, and resets
# the count to zero after every FALSE element.
# 
# Just press the "Run App" button on upper right of this panel.
##############################


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# You must first compile the following Rcpp file as follows:
# Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/lm_arma.cpp")


## Setup code runs once when the shiny app is started


# Load packages
library(rutils)
library(shiny)
library(dygraphs)

############## deprecated ##############
# Source the strategy functions
# source("C:/Develop/R/scripts/calc_strategy.R")
# Calculate indicator_s matrix of OHLC technical indicators
# source(file="C:/Develop/R/scripts/load_technical_indicators.R")
# vol_at[which.max(vol_at)] <- 0
# vol_at[which.max(vol_at)] <- 0
############## end deprecated ##############


# Load daily OHLC data
# com_bo <- HighFreq::SPY
# symbol_s <- c("IVW", "VTI", "IWF", "IWD", "IWB", "VYM", "DBC", "IEF", "VEU", "SVXY", "VXX")
# com_bo <- mget(symbol_s, envir=rutils::etf_env)
# com_bo <- rutils::do_call(cbind, com_bo)
# com_bo <- zoo::na.locf(com_bo, na.rm=FALSE)
# com_bo <- zoo::na.locf(com_bo, fromLast=TRUE)

# Load ES1 futures data
# load(file="C:/Develop/data/ES1.RData")
# symbol_asset <- "SPY"

# Load combined futures data
# com_bo <- HighFreq::SPY
load(file="C:/Develop/data/combined.RData")
symbol_s <- unique(rutils::get_name(colnames(com_bo)))
# UX1: close_high_thresh=2, close_low_thresh=2, lagg=1
# UX1: close_high_thresh=3, close_low_thresh=3, lagg=1

# cap_tion <- "Contrarian Strategy Using OHLC Technical Indicators"


## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("Contrarian Strategy Using OHLC Technical Indicators"),
  
  # Create row with for symbols
  fluidRow(
    # Input the symbol for signal
    column(width=3, selectInput("symbol_signal", label="symbol for signal: ",
                                choices=symbol_s, selected=symbol_s[1])),
    # Input the symbol for trading
    column(width=3, selectInput("symbol_asset", label="symbol for trading (asset): ",
                                choices=symbol_s, selected=symbol_s[1])),
    # Input the trade lag
    column(width=3, numericInput("lagg", label="trade lag:",
                                 min=1, max=33, value=3, step=1)),
  ),  # end fluidRow
  
  # Create row for thresholds
  fluidRow(
    # Input the count threshold levels
    column(width=3, numericInput("close_high_thresh", label="close high threshold:",
                                 min=0, max=10, value=2, step=1)),
    column(width=3, numericInput("close_low_thresh", label="close low threshold:",
                                 min=0, max=10, value=2, step=1))
    # Input the long look-back interval
    # column(width=4, sliderInput("look_long", label="look-back long:",
    #                             min=2, max=30, value=11, step=1))
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(dygraphOutput("dygraph"), width=12)
)  # end fluidPage interface


## Define the server code
# The function shinyServer() accepts a function 
# with the arguments "input" and "output".
ser_ver <- shiny::shinyServer(function(input, output) {

  ## Re-calculate the model with new parameters
  # The function reactive() accepts a block of expressions
  # which calculate the model, and returns the model output.
  da_ta <- reactive({
    
    ## Extract model parameters from the argument "input"
    # beta_ret <- input$beta_ret
    # en_ter <- input$en_ter
    # ex_it <- input$ex_it
    # beta_vol <- input$beta_vol
    # beta_skew <- input$beta_skew
    # beta_moment <- input$beta_moment
    # beta_ophi <- input$beta_ophi
    # beta_clhi <- input$beta_clhi
    # look_short <- input$look_short
    # look_long <- input$look_long
    symbol_signal <- input$symbol_signal
    symbol_asset <- input$symbol_asset
    lagg <- input$lagg
    close_high_thresh <- input$close_high_thresh
    close_low_thresh <- input$close_low_thresh
    # weight_s <- c(beta_ret, en_ter, beta_vol, beta_skew)
    # weight_s <- c(beta_ret, beta_vol, beta_skew, beta_moment, beta_ophi, beta_clhi)
    
    ## Set up data for signal
    oh_lc <- com_bo[, paste(symbol_signal, c("Open", "High", "Low", "Close"), sep=".")]
    oh_lc <- na.omit(oh_lc)
    in_dex <- zoo::index(oh_lc)
    clo_se <- Cl(oh_lc)
    close_num <- as.numeric(clo_se)
    # re_turns <- rutils::diff_it(clo_se)
    op_en <- Op(oh_lc)
    hi_gh <- Hi(oh_lc)
    hi_gh <- as.numeric(hi_gh)
    lo_w <- Lo(oh_lc)
    low_num <- as.numeric(lo_w)
    # Set TRUE if close is at the high
    close_high <- (close_num == hi_gh)
    # Count number of consecutive closes is at the high
    close_high_count <- roll_count(close_high)
    # Set TRUE if close is at the low
    close_low <- (close_num == low_num)
    # Count number of consecutive closes is at the low
    close_low_count <- roll_count(close_low)
    op_en <- as.numeric(op_en)
    open_high <- (op_en == hi_gh)
    open_high_count <- roll_count(open_high)
    open_low <- (op_en == low_num)
    open_low_count <- roll_count(open_low)
    
    
    # Set up data for trading
    clo_se <- com_bo[in_dex, paste(symbol_asset, "Close", sep=".")]
    clo_se <- zoo::na.locf(clo_se, fromLast=TRUE)
    re_turns <- rutils::diff_it(clo_se)

    ## Simulate strategy
    
    position_s <- rep(NA_integer_, NROW(oh_lc))
    position_s[1] <- 0
    # position_s[close_high] <- (-1)
    # position_s[close_low] <- 1
    # position_s[open_low & close_high] <- (-1)
    # position_s[open_high & close_low] <- 1
    position_s[close_high_count > close_high_thresh] <- (-1)
    position_s[close_low_count > close_low_thresh] <- 1
    position_s <- zoo::na.locf(position_s)
    position_s <- rutils::lag_it(position_s, lagg=lagg)
    # Calculate number of trades
    # turn_over <- abs(rutils::diff_it(position_s)) / 2
    # sum(turn_over)/NROW(position_s)
    # NROW(position_s)/sum(turn_over)
    
    # Calculate pnl_s
    pnl_s <- cumsum(position_s*re_turns)
    pnl_s <- cbind(clo_se, pnl_s)#[xts::endpoints(pnl_s, on="days")]
    # pnl_s <- xts::to.daily(cbind(clo_se, pnl_s))
    # colnames(pnl_s) <- c("asset", "strategy")
    colnames(pnl_s) <- c(symbol_asset, "strategy")
    pnl_s
    # calculate max_min signals
    # max_min <- roll_maxmin(close_num, look_short)
    # close_high_count <- (close_num == max_min[, 1])
    # close_low_count <- (close_num == max_min[, 2])
    
    # calculate signal
    # sig_nal <- clo_se
    # trending signal
    # signal_trend <- calc_signal(oh_lc=oh_lc, clo_se=close_num,
    #                             de_sign=de_sign,
    #                             look_short=look_short, look_long=look_long, high_freq=FALSE)
    # signal_trend <- calc_ma(oh_lc=oh_lc, clo_se=close_num,
    #                         de_sign=de_sign,
    #                         look_back=look_long, high_freq=FALSE)
    
    # mean reverting signal
    # signal_revert <- oh_lc[, 1]  # dummy signal
    # signal_revert <- calc_signal(oh_lc=oh_lc, clo_se=close_num,
    #                             de_sign=de_sign,
    #                             look_short=look_short)
    # signal_revert <- HighFreq::roll_zscores(res_ponse=close_num, 
    #                         de_sign=de_sign, 
    #                         look_back=look_short)
    # sig_nal[1:look_short, ] <- 0
    # scale sig_nal using HighFreq::roll_scale()
    # sig_nal <- roll::roll_scale(data=sig_nal, width=look_short, min_obs=1)
    # sig_nal <- HighFreq::roll_scale(mat_rix=sig_nal, look_back=look_short, use_median=TRUE)
    # sig_nal[1:look_short, ] <- 0
    # sig_nal[is.infinite(sig_nal), ] <- 0
    # sig_nal <- rutils::lag_it(sig_nal, lagg=1)
    # calculate positions, either: -1, 0, or 1
    # position_s <- -sign(sig_nal)
    # calculate positions, either: -1, 0, or 1
    # position_s <- rep(NA_integer_, NROW(oh_lc))
    # position_s[1] <- 0
    # position_s[(sig_nal < (-en_ter)) & close_low] <- 1
    # position_s[(sig_nal > en_ter) & close_high] <- (-1)
    # position_s[abs(sig_nal) < ex_it] <- 0
    # position_s <- na.locf(position_s)
    # position_s <- rutils::lag_it(position_s, lagg=1)
    # # position_s <- position_s + rutils::lag_it(position_s, lagg=1) + rutils::lag_it(position_s, lagg=2)
    # position_s <- rutils::lag_it(position_s, lagg=lagg)

    # trending signal
    # sig_nal <- HighFreq::roll_zscores(res_ponse=clo_se, 
    #                         de_sign=de_sign, 
    #                         look_back=look_long)
    # sig_nal[1:look_long, ] <- 0
    # sig_nal <- rutils::lag_it(sig_nal)
    # calculate positions, either: -1, 0, or 1
    # position_s <- position_s + sign(sig_nal)
    # position_s <- rep(NA_integer_, NROW(oh_lc))
    # position_s[1] <- 0
    # position_s[sig_nal<beta_vol] <- (-1)
    # position_s[sig_nal>beta_vol] <- 1
    # position_s <- na.locf(position_s)
    # pnl_s <- signal_revert
    # pnl_s <- cumsum(position_s*re_turns)
    # colnames(pnl_s) <- "strategy"
    
    # sim_revert(signal_revert, returns_trade, close_high, close_high_count, close_low, close_low_count, en_ter, ex_it, lagg)
    
    # sim_trend(signal_trend, returns_trade, en_ter, ex_it, close_high_trade, close_low_trade, lagg)
    
    # sim_trend(signal_trend, re_turns, close_high, close_low, en_ter, ex_it, lagg)
    # sim_revert_trending(signal_revert, signal_trend, re_turns, en_ter, ex_it, close_high_trade, close_low_trade, lagg)
    # position_s <- xts(position_s, index(oh_lc))
    # colnames(position_s) <- "strategy"
    # pnl_s
  })  # end reactive code
  
  # return the dygraph plot to output argument
  output$dygraph <- renderDygraph({
    col_names <- colnames(da_ta())
    cap_tion <- paste("Contrarian Strategy for", col_names[1], "Using OHLC Technical Indicators")
    
    # plot daily closing prices
    # dygraphs::dygraph(cbind(clo_se, da_ta())[endpoints(clo_se, on="days")], main=cap_tion) %>%
      # plot daily closing ES1 prices
      # dygraphs::dygraph(da_ta(), main=cap_tion) %>%
      dygraphs::dygraph(da_ta(), main=cap_tion) %>%
      # plot a few days with all the minute bars
      # dygraphs::dygraph(cbind(clo_se, da_ta())["2018-02-01/2018-02-07"], main=cap_tion) %>%
      # plot a few days with all the ES1 minute bars
      # dygraphs::dygraph(cbind(Cl(ohlc_trade), da_ta())["2018-02-01/2018-02-07"], main=cap_tion) %>%
      # dyAxis("y", label="asset", independentTicks=TRUE) %>%
        dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
        dySeries(name=col_names[1], axis="y", col="blue") %>%
        dySeries(name=col_names[2], axis="y2", strokeWidth=2, col="red")
  })  # end output plot

  # output$hist <- hist(da_ta())  # end output hist
  
})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
