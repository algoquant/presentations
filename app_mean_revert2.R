##############################
# This is a shiny app for simulating a mean reverting 
# strategy using static betas times OHLC technical indicators, 
# with dygraphs plot.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started


# load packages
library(shiny)
library(dygraphs)
library(rutils)

# Source the strategy functions
source("C:/Develop/R/scripts/backtest_functions.R")
# Calculate indicator_s matrix of OHLC technical indicators
# source(file="C:/Develop/R/scripts/load_technical_indicators.R")
# vol_at[which.max(vol_at)] <- 0
# vol_at[which.max(vol_at)] <- 0


# oh_lc <- HighFreq::SPY

# oh_lc <- HighFreq::SPY["2010"]
# oh_lc <- rutils::etf_env$VTI
# load recent ES1 futures data
# load(file="C:/Develop/data/ES1.RData")
# sym_bol <- "SPY"

# load recent combined futures data
load(file="C:/Develop/data/combined.RData")

# plot daily closing prices
# dygraphs::dygraph(oh_lc[endpoints(oh_lc, on="days"), c("ES1.Close", "TY1.Close")], 
#                   main="ES1 versus TY1 prices") %>%
#   dyAxis("y", label="ES1.Close", independentTicks=TRUE) %>%
#   dyAxis("y2", label="TY1.Close", independentTicks=TRUE) %>%
#   dySeries("TY1.Close", axis="y2", col=c("blue", "red"))

# set up data for signal
sym_bol <- "ES1"
oh_lc <- com_bo[, paste(sym_bol, c("Open", "High", "Low", "Close"), sep=".")]
# oh_lc <- xts::to.period(oh_lc, period="minutes", k=5)
# create random time series of minutely OHLC prices
# oh_lc <- HighFreq::random_ohlc(vol_at = 1e-03, 
#   in_dex=seq(from=(Sys.time()-5e6), length.out=5e6, by="1 sec"))
ohlc_log <- log(oh_lc)
clos_e <- Cl(ohlc_log)
close_num <- drop(clos_e)
hi_gh <- Hi(ohlc_log)
lo_w <- Lo(ohlc_log)
close_high <- (close_num == drop(hi_gh))
# close_high_count <- HighFreq::roll_count(close_high)
close_low <- (close_num == drop(lo_w))
# close_low_count <- HighFreq::roll_count(close_low)
# re_turns <- rutils::diff_it(clos_e)
# colnames(re_turns) <- "returns"
date_s <- 1:NROW(oh_lc)
# date_s <- xts::.index(oh_lc)
de_sign <- matrix(date_s, nc=1)
# date_s <- xts::.index(oh_lc)
# de_sign <- matrix(xts::.index(oh_lc), nc=1)
# indicator_s <- cbind(re_turns, z_scores, vol_at, sk_ew)

# set up data for trading
sym_bol <- "ES1"
clos_e <- com_bo[, paste(sym_bol, "Close", sep=".")]
re_turns <- rutils::diff_it(log(clos_e))
# create random time series of minutely OHLC prices
# ohlc_trade <- HighFreq::random_ohlc(vol_at = 1e-03, 
#   in_dex=seq(from=(Sys.time()-5e6), length.out=5e6, by="1 sec"))
# close_num <- drop(clos_e)
# hi_gh <- Hi(ohlc_trade)
# lo_w <- Lo(ohlc_trade)
# close_high_trade <- (close_num == drop(hi_gh))
# close_low_trade <- (close_num == drop(lo_w))
# re_turns <- rutils::diff_it(clos_e)
colnames(re_turns) <- "returns"


# End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(paste(sym_bol, "Strategy Using OHLC Technical Indicators")),
  
  # create single row with four slider inputs
  fluidRow(
    # input the trade lag
    column(width=4, numericInput("trade_lag", label="trade lag:",
                                 min=0, max=5, value=1, step=1)),
    # input the long look-back interval
    column(width=4, sliderInput("look_long", label="look-back long:",
                                min=2, max=30, value=11, step=1)),
    # input the short look-back interval
    column(width=4, sliderInput("look_short", label="look-back short:",
                                min=2, max=100, value=11, step=1)),
    # input the returns beta
    # column(width=4, sliderInput("beta_ret", label="returns beta:",
    #                             min=-20.0, max=20.0, value=0.0, step=0.1)),
    # input the trade entry level
    column(width=4, sliderInput("en_ter", label="trade enter level:",
                                min=0.1, max=5.0, value=1.0, step=0.1)),
    # input the trade exit level
    column(width=4, sliderInput("ex_it", label="trade exit level:",
                                # min=1.0, max=10.0, value=2.0, step=1.0))
                                min=0.0, max=1.0, value=0.0, step=0.1))
    # input the vol beta
    # column(width=4, sliderInput("beta_vol", label="vol beta:",
    #                             min=0.01, max=1.0, value=0.5, step=0.01)),
    # input the skew beta
    # column(width=4, sliderInput("beta_skew", label="skew beta:",
    #                             min=-20.0, max=20.0, value=-20.0, step=0.1)),
    # # input the momentum beta
    # column(width=4, sliderInput("beta_moment", label="momentum beta:",
    #                             min=-5.0, max=5.0, value=-5.0, step=0.1)),
    # # input the op_en-hi_gh beta
    # column(width=4, sliderInput("beta_ophi", label="op_en-hi_gh beta:",
    #                             min=-5.0, max=5.0, value=-5.0, step=0.1)),
    # # input the clos_e-hi_gh beta
    # column(width=4, sliderInput("beta_clhi", label="clos_e-hi_gh beta:",
    #                             min=-5.0, max=5.0, value=-5.0, step=0.1)),
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(dygraphs::dygraphOutput("dygraph"), width=12)
)  # end fluidPage interface


## Define the server code
ser_ver <- shiny::shinyServer(function(input, output) {

  ## Recalculate the model with new parameters
  da_ta <- reactive({
    # get model parameters from input argument
    # beta_ret <- input$beta_ret
    en_ter <- input$en_ter
    ex_it <- input$ex_it
    # beta_vol <- input$beta_vol
    # beta_skew <- input$beta_skew
    # beta_moment <- input$beta_moment
    # beta_ophi <- input$beta_ophi
    # beta_clhi <- input$beta_clhi
    look_short <- input$look_short
    look_long <- input$look_long
    trade_lag <- input$trade_lag
    # weight_s <- c(beta_ret, en_ter, beta_vol, beta_skew)
    # weight_s <- c(beta_ret, beta_vol, beta_skew, beta_moment, beta_ophi, beta_clhi)
    
    ## Simulate strategy
    # calculate max_min signals
    # max_min <- roll_maxmin(close_num, look_short)
    # close_high_count <- (close_num == max_min[, 1])
    # close_low_count <- (close_num == max_min[, 2])
    
    # calculate signal
    # sig_nal <- clos_e
    # trending signal
    # signal_trend <- calc_signal(ohlc=ohlc_log, clos_e=close_num,
    #                             design=de_sign,
    #                             look_short=look_short, look_long=look_long, high_freq=FALSE)
    # signal_trend <- calc_ma(ohlc=ohlc_log, clos_e=close_num,
    #                         design=de_sign,
    #                         look_back=look_long, high_freq=FALSE)
    
    # mean reverting signal
    # signal_revert <- ohlc_log[, 1]  # dummy signal
    signal_revert <- calc_signal(ohlc=ohlc_log, clos_e=close_num,
                                design=de_sign,
                                look_short=look_short)
    # signal_revert <- HighFreq::roll_zscores(response=close_num, 
    #                         design=de_sign, 
    #                         look_back=look_short)
    # sig_nal[1:look_short, ] <- 0
    # scale sig_nal using HighFreq::roll_scale()
    # sig_nal <- roll::roll_scale(data=sig_nal, width=look_short, min_obs=1)
    # sig_nal <- HighFreq::roll_scale(mat_rix=sig_nal, look_back=look_short, use_median=TRUE)
    # sig_nal[1:look_short, ] <- 0
    # sig_nal[is.infinite(sig_nal), ] <- 0
    # sig_nal <- rutils::lag_it(sig_nal, lagg=1)
    # calculate positions, either: -1, 0, or 1
    # po_sit <- -sign(sig_nal)
    # calculate positions, either: -1, 0, or 1
    # po_sit <- rep(NA_integer_, NROW(oh_lc))
    # po_sit[1] <- 0
    # po_sit[(sig_nal < (-en_ter)) & close_low] <- 1
    # po_sit[(sig_nal > en_ter) & close_high] <- (-1)
    # po_sit[abs(sig_nal) < ex_it] <- 0
    # po_sit <- na.locf(po_sit)
    # po_sit <- rutils::lag_it(po_sit, lagg=1)
    # # po_sit <- po_sit + rutils::lag_it(po_sit, lagg=1) + rutils::lag_it(po_sit, lagg=2)
    # po_sit <- rutils::lag_it(po_sit, lagg=trade_lag)

    # trending signal
    # sig_nal <- HighFreq::roll_zscores(response=clos_e, 
    #                         design=de_sign, 
    #                         look_back=look_long)
    # sig_nal[1:look_long, ] <- 0
    # sig_nal <- rutils::lag_it(sig_nal)
    # calculate positions, either: -1, 0, or 1
    # po_sit <- po_sit + sign(sig_nal)
    # po_sit <- rep(NA_integer_, NROW(oh_lc))
    # po_sit[1] <- 0
    # po_sit[sig_nal<beta_vol] <- (-1)
    # po_sit[sig_nal>beta_vol] <- 1
    # po_sit <- na.locf(po_sit)
    # pnl_s <- signal_revert
    # pnl_s <- cumsum(po_sit*re_turns)
    # colnames(pnl_s) <- "strategy"
    
    pnl_s <- sim_revert(signal_revert, re_turns, close_high, close_high_count, close_low, close_low_count, en_ter, ex_it, trade_lag)
    
    # sim_trend(signal_trend, re_turns, en_ter, ex_it, close_high_trade, close_low_trade, trade_lag)
    
    # sim_trend(signal_trend, re_turns, close_high, close_low, en_ter, ex_it, trade_lag)
    # sim_revert_trending(signal_revert, signal_trend, re_turns, en_ter, ex_it, close_high_trade, close_low_trade, trade_lag)
    # po_sit <- xts(po_sit, index(oh_lc))
    pnl_s <- cbind(clos_e, pnl_s)[xts::endpoints(pnl_s, on="days")]
    # pnl_s <- xts::to.daily(cbind(clos_e, pnl_s))
    # colnames(pnl_s) <- c("asset", "strategy")
    colnames(pnl_s) <- c(sym_bol, "strategy")
    pnl_s
  })  # end reactive code
  
  # return the dygraph plot to output argument
  output$dygraph <- dygraphs::renderDygraph({
    # plot daily closing prices
    # dygraphs::dygraph(cbind(clos_e, da_ta())[endpoints(clos_e, on="days")], main=paste(sym_bol, "Strategy Using OHLC Technical Indicators")) %>%
      # plot daily closing ES1 prices
    dygraphs::dygraph(da_ta(), main=paste(colnames(da_ta())[1], "Strategy Using OHLC Technical Indicators")) %>%
      # plot a few days with all the minute bars
      # dygraphs::dygraph(cbind(clos_e, da_ta())["2018-02-01/2018-02-07"], main=paste(sym_bol, "Strategy Using OHLC Technical Indicators")) %>%
      # plot a few days with all the ES1 minute bars
      # dygraphs::dygraph(cbind(Cl(ohlc_trade), da_ta())["2018-02-01/2018-02-07"], main=paste(sym_bol, "Strategy Using OHLC Technical Indicators")) %>%
      dyAxis("y", label=colnames(da_ta())[1], independentTicks=TRUE) %>%
      dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
      dySeries("strategy", axis="y2", col=c("blue", "red"))
  })  # end output plot

  # output$hist <- hist(da_ta())  # end output hist
  
})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
