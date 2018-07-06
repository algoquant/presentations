##############################
# This is a shiny app for simulating a contrarian strategy 
# using the number of consecutive close_low or close_high.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started


# load packages
library(shiny)
library(dygraphs)
library(rutils)

# Source the strategy functions
# source("C:/Develop/R/scripts/calc_strategy.R")
# Calculate indicator_s matrix of OHLC technical indicators
# source(file="C:/Develop/R/scripts/load_technical_indicators.R")
# vol_at[which.max(vol_at)] <- 0
# vol_at[which.max(vol_at)] <- 0


# oh_lc <- HighFreq::SPY

# oh_lc <- HighFreq::SPY["2010"]
# oh_lc <- rutils::env_etf$VTI
# load recent ES1 futures data
# load(file="C:/Develop/data/ES1.RData")
# symbol_asset <- "SPY"

# load recent combined futures data
load(file="C:/Develop/data/combined.RData")

# End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("Strategy Using OHLC Technical Indicators"),
  
  # create single row with four slider inputs
  fluidRow(
    # input the symbol for signal
    column(width=4, textInput("symbol_signal", label="symbol for signal:", value="ES1")),
    # input the symbol for trading
    column(width=4, textInput("symbol_asset", label="symbol for trading (asset):", value="ES1")),
    # input the trade lag
    column(width=4, numericInput("trade_lag", label="trade lag:",
                                 min=1, max=33, value=1, step=1)),
    # input the count threshold levels
    column(width=4, numericInput("close_high_thresh", label="close high threshold:",
                                 min=0, max=5, value=1, step=1)),
    column(width=4, numericInput("close_low_thresh", label="close low threshold:",
                                 min=0, max=5, value=1, step=1))
    # input the long look-back interval
    # column(width=4, sliderInput("look_long", label="look-back long:",
    #                             min=2, max=30, value=11, step=1))
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(dygraphOutput("dygraph"), width=12)
)  # end fluidPage interface


## Define the server code
ser_ver <- shiny::shinyServer(function(input, output) {

  ## Re-calculate the model with new parameters
  da_ta <- reactive({
    # get model parameters from input argument
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
    trade_lag <- input$trade_lag
    close_high_thresh <- input$close_high_thresh
    close_low_thresh <- input$close_low_thresh
    # weight_s <- c(beta_ret, en_ter, beta_vol, beta_skew)
    # weight_s <- c(beta_ret, beta_vol, beta_skew, beta_moment, beta_ophi, beta_clhi)
    
    # set up data for signal
    oh_lc <- com_bo[, paste(symbol_signal, c("Open", "High", "Low", "Close"), sep=".")]
    log_ohlc <- log(oh_lc)
    clo_se <- Cl(log_ohlc)
    close_num <- as.numeric(clo_se)
    # re_turns <- rutils::diff_it(clo_se)
    op_en <- Op(log_ohlc)
    hi_gh <- Hi(log_ohlc)
    high_num <- as.numeric(hi_gh)
    lo_w <- Lo(log_ohlc)
    low_num <- as.numeric(lo_w)
    close_high <- (close_num == high_num)
    close_high_count <- roll_count(close_high)
    close_low <- (close_num == low_num)
    close_low_count <- roll_count(close_low)
    open_num <- as.numeric(op_en)
    open_high <- (open_num == high_num)
    open_high_count <- roll_count(open_high)
    open_low <- (open_num == low_num)
    open_low_count <- roll_count(open_low)
    
    
    # set up data for trading
    clo_se <- com_bo[, paste(symbol_asset, "Close", sep=".")]
    re_turns <- rutils::diff_it(log(clo_se))

    ## Simulate strategy
    
    po_sit <- rep(NA_integer_, NROW(oh_lc))
    po_sit[1] <- 0
    po_sit[close_high] <- (-1)
    po_sit[close_low] <- 1
    # po_sit[open_low & close_high] <- (-1)
    # po_sit[open_high & close_low] <- 1
    # po_sit[close_high_count>close_high_thresh] <- (-1)
    # po_sit[close_low_count>close_low_thresh] <- 1
    po_sit <- zoo::na.locf(po_sit)
    po_sit <- rutils::lag_it(po_sit, lagg=trade_lag)
    pnl_s <- cumsum(po_sit*re_turns)
    pnl_s <- cbind(clo_se, pnl_s)[xts::endpoints(pnl_s, on="days")]
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
    # signal_trend <- calc_signal(oh_lc=log_ohlc, clo_se=close_num,
    #                             de_sign=de_sign,
    #                             look_short=look_short, look_long=look_long, high_freq=FALSE)
    # signal_trend <- calc_ma(oh_lc=log_ohlc, clo_se=close_num,
    #                         de_sign=de_sign,
    #                         look_back=look_long, high_freq=FALSE)
    
    # mean reverting signal
    # signal_revert <- log_ohlc[, 1]  # dummy signal
    # signal_revert <- calc_signal(oh_lc=log_ohlc, clo_se=close_num,
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
    # sig_nal <- HighFreq::roll_zscores(res_ponse=clo_se, 
    #                         de_sign=de_sign, 
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
    
    # sim_revert(signal_revert, returns_trade, close_high, close_high_count, close_low, close_low_count, en_ter, ex_it, trade_lag)
    
    # sim_trend(signal_trend, returns_trade, en_ter, ex_it, close_high_trade, close_low_trade, trade_lag)
    
    # sim_trend(signal_trend, re_turns, close_high, close_low, en_ter, ex_it, trade_lag)
    # sim_revert_trending(signal_revert, signal_trend, re_turns, en_ter, ex_it, close_high_trade, close_low_trade, trade_lag)
    # po_sit <- xts(po_sit, index(oh_lc))
    # colnames(po_sit) <- "strategy"
    # pnl_s
  })  # end reactive code
  
  # return the dygraph plot to output argument
  output$dygraph <- renderDygraph({
    # plot daily closing prices
    # dygraphs::dygraph(cbind(clo_se, da_ta())[endpoints(clo_se, on="days")], main=paste(symbol_asset, "Strategy Using OHLC Technical Indicators")) %>%
      # plot daily closing ES1 prices
      # dygraphs::dygraph(da_ta(), main="Strategy Using OHLC Technical Indicators") %>%
      dygraphs::dygraph(da_ta(), main=paste(colnames(da_ta())[1], "Strategy Using OHLC Technical Indicators")) %>%
      # plot a few days with all the minute bars
      # dygraphs::dygraph(cbind(clo_se, da_ta())["2018-02-01/2018-02-07"], main=paste(symbol_asset, "Strategy Using OHLC Technical Indicators")) %>%
      # plot a few days with all the ES1 minute bars
      # dygraphs::dygraph(cbind(Cl(ohlc_trade), da_ta())["2018-02-01/2018-02-07"], main=paste(symbol_asset, "Strategy Using OHLC Technical Indicators")) %>%
      # dyAxis("y", label="asset", independentTicks=TRUE) %>%
      dyAxis("y", label=colnames(da_ta())[1], independentTicks=TRUE) %>%
      dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
      dySeries("strategy", axis="y2", col=c("blue", "red"))
  })  # end output plot

  # output$hist <- hist(da_ta())  # end output hist
  
})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
