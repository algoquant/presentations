##############################
# This is a shiny app for simulating a mean reverting 
# strategy using static betas times OHLC technical indicators, 
# with dygraphs plot.
# Just press the "Run App" button on upper right of this panel.
##############################

## Setup code runs once when the shiny app is started


# Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Source the strategy functions
source("/Users/jerzy/Develop/R/backtest_functions.R")
# Calculate indicator_s matrix of OHLC technical indicators
# source(file="C:/Develop/R/scripts/load_technical_indicators.R")
# volat[which.max(volat)] <- 0
# volat[which.max(volat)] <- 0


# ohlc <- HighFreq::SPY

# ohlc <- HighFreq::SPY["2010"]
# ohlc <- rutils::etfenv$VTI
# load recent ES1 futures data
# load(file="C:/Develop/data/ES1.RData")
# score <- "SPY"

# load recent combined futures data
load(file="C:/Develop/data/combined.RData")

# plot daily closing prices
# dygraphs::dygraph(ohlc[endpoints(ohlc, on="days"), c("ES1.Close", "TY1.Close")], 
#                   main="ES1 versus TY1 prices") %>%
#   dyAxis("y", label="ES1.Close", independentTicks=TRUE) %>%
#   dyAxis("y2", label="TY1.Close", independentTicks=TRUE) %>%
#   dySeries("TY1.Close", axis="y2", col=c("blue", "red"))

# set up data for trading
symbol <- "ES1"
es1 <- com_bo[, paste(symbol, c("Open", "High", "Low", "Close"), sep=".")]
es1 <- log(es1)
# create random time series of minutely OHLC prices
# es1 <- HighFreq::random_ohlc(volat = 1e-03, 
#   indeks=seq(from=(Sys.time()-5e6), length.out=5e6, by="1 sec"))
closep <- Cl(es1)
close_num <- as.numeric(closep)
highp <- Hi(es1)
lowp <- Lo(es1)
es1_close_high <- (close_num == as.numeric(highp))
es1_close_low <- (close_num == as.numeric(lowp))
es1_returns <- rutils::diffit(closep)
colnames(es1_returns) <- "returns"

# set up data for signal
score <- "TU1"
ohlc <- com_bo[, paste(score, c("Open", "High", "Low", "Close"), sep=".")]
# ohlc <- xts::to.period(ohlc, period="minutes", k=5)
# create random time series of minutely OHLC prices
# ohlc <- HighFreq::random_ohlc(volat = 1e-03, 
#   indeks=seq(from=(Sys.time()-5e6), length.out=5e6, by="1 sec"))
log_ohlc <- log(ohlc)
closep <- Cl(log_ohlc)
close_num <- as.numeric(closep)
highp <- Hi(log_ohlc)
lowp <- Lo(log_ohlc)
close_high <- (close_num == as.numeric(highp))
close_high_count <- HighFreq::roll_count(close_high)
close_low <- (close_num == as.numeric(lowp))
close_low_count <- HighFreq::roll_count(close_low)
returns <- rutils::diffit(closep)
colnames(returns) <- "returns"
dates <- 1:NROW(ohlc)
# dates <- xts::.index(ohlc)
predictor <- matrix(dates, nc=1)
# dates <- xts::.index(ohlc)
# predictor <- matrix(as.numeric(xts::.index(ohlc)), nc=1)
# indicator_s <- cbind(returns, zscores, volat, skew)

# End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel(paste(score, "Strategy Using OHLC Technical Indicators")),
  
  # create single row with four slider inputs
  fluidRow(
    # input the lags
    column(width=4, numericInput("trade_lag", label="trade lag:",
                                 min=1, max=5, value=1, step=1)),
    # input the long look-back interval
    column(width=4, sliderInput("look_long", label="look-back long:",
                                min=2, max=30, value=12, step=1)),
    # input the short look-back interval
    column(width=4, sliderInput("look_short", label="look-back short:",
                                min=2, max=100, value=2, step=1)),
    # input the returns beta
    # column(width=4, sliderInput("beta_ret", label="returns beta:",
    #                             min=-20.0, max=20.0, value=0.0, step=0.1)),
    # input the trade entry level
    column(width=4, sliderInput("enter", label="trade enter level:",
                                min=0.1, max=5.0, value=4.0, step=0.1)),
    # input the trade exit level
    column(width=4, sliderInput("exit", label="trade exit level:",
                                # min=1.0, max=10.0, value=2.0, step=1.0))
                                min=0.1, max=1.0, value=0.2, step=0.1))
    # input the vol beta
    # column(width=4, sliderInput("beta_vol", label="vol beta:",
    #                             min=0.01, max=1.0, value=0.5, step=0.01)),
    # input the skew beta
    # column(width=4, sliderInput("betaskew", label="skew beta:",
    #                             min=-20.0, max=20.0, value=-20.0, step=0.1)),
    # # input the momentum beta
    # column(width=4, sliderInput("betamoment", label="momentum beta:",
    #                             min=-5.0, max=5.0, value=-5.0, step=0.1)),
    # # input the openp-highp beta
    # column(width=4, sliderInput("beta_ophi", label="openp-highp beta:",
    #                             min=-5.0, max=5.0, value=-5.0, step=0.1)),
    # # input the closep-highp beta
    # column(width=4, sliderInput("beta_clhi", label="closep-highp beta:",
    #                             min=-5.0, max=5.0, value=-5.0, step=0.1)),
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(dygraphs::dygraphOutput("dygraph"), width=12)
)  # end fluidPage interface


## Define the server code
servfun <- shiny::shinyServer(function(input, output) {

  ## Recalculate the model with new parameters
  datav <- shiny::reactive({
    # get model parameters from input argument
    # beta_ret <- input$beta_ret
    enter <- input$enter
    exit <- input$exit
    # beta_vol <- input$beta_vol
    # betaskew <- input$betaskew
    # betamoment <- input$betamoment
    # beta_ophi <- input$beta_ophi
    # beta_clhi <- input$beta_clhi
    look_short <- input$look_short
    look_long <- input$look_long
    trade_lag <- input$trade_lag
    # weights <- c(beta_ret, enter, beta_vol, betaskew)
    # weights <- c(beta_ret, beta_vol, betaskew, betamoment, beta_ophi, beta_clhi)
    
    ## Simulate strategy
    # calculate max_min signals
    # max_min <- roll_maxmin(close_num, look_short)
    # close_high_count <- (close_num == max_min[, 1])
    # close_low_count <- (close_num == max_min[, 2])
    
    # calculate signal
    # score <- closep
    # trending signal
    # signal_long <- calc_signal(ohlc=log_ohlc, closep=close_num,
    #                             predictor=predictor,
    #                             look_short=look_short, look_long=look_long, high_freq=FALSE)
    signal_long <- calc_ma(ohlc=log_ohlc, closep=close_num,
                           predictor=predictor,
                           look_back=look_long, high_freq=FALSE)
    # mean reverting signal
    # signal_short <- log_ohlc[, 1]  # dummy signal
    # signal_short <- calc_signal(ohlc=log_ohlc, closep=close_num,
    #                             predictor=predictor,
    #                             look_short=look_short, look_long=look_long, high_freq=FALSE)
    # score <- HighFreq::roll_zscores(response=close_num, 
    #                         predictor=predictor, 
    #                         look_back=look_short)
    # score[1:look_short, ] <- 0
    # scale score using HighFreq::roll_scale()
    # score <- roll::roll_scale(data=score, width=look_short, min_obs=1)
    # score <- HighFreq::roll_scale(matrixv=score, look_back=look_short, use_median=TRUE)
    # score[1:look_short, ] <- 0
    # score[is.infinite(score), ] <- 0
    # score <- rutils::lagit(score, lagg=1)
    # calculate positions, either: -1, 0, or 1
    # posit <- -sign(score)
    # calculate positions, either: -1, 0, or 1
    # posit <- rep(NA_integer_, NROW(ohlc))
    # posit[1] <- 0
    # posit[(score < (-enter)) & close_low] <- 1
    # posit[(score > enter) & close_high] <- (-1)
    # posit[abs(score) < exit] <- 0
    # posit <- na.locf(posit)
    # posit <- rutils::lagit(posit, lagg=1)
    # # posit <- posit + rutils::lagit(posit, lagg=1) + rutils::lagit(posit, lagg=2)
    # posit <- rutils::lagit(posit, lagg=trade_lag)

    # trending signal
    # score <- HighFreq::roll_zscores(response=closep, 
    #                         predictor=predictor, 
    #                         look_back=look_long)
    # score[1:look_long, ] <- 0
    # score <- rutils::lagit(score)
    # calculate positions, either: -1, 0, or 1
    # posit <- posit + sign(score)
    # posit <- rep(NA_integer_, NROW(ohlc))
    # posit[1] <- 0
    # posit[score<beta_vol] <- (-1)
    # posit[score>beta_vol] <- 1
    # posit <- na.locf(posit)
    # pnls <- signal_short
    # pnls <- cumsum(posit*returns)
    # colnames(pnls) <- "strategy"
    
    # datav <- sim_revert(signal_short, returns, close_high, close_high_count, close_low, close_low_count, enter, exit, trade_lag)
    datav <- sim_trend(signal_long, es1_returns, es1_close_high, es1_close_low, enter, exit, trade_lag)
    datav <- cbind(Cl(es1), datav)
    colnames(datav)[1] <- symbol
    datav
    # sim_trend(signal_long, returns, close_high, close_low, enter, exit, trade_lag)
    # sim_revert_trending(signal_short, signal_long, returns, close_high, close_low, enter, exit, trade_lag)
    # posit <- xts(posit, index(ohlc))
    # colnames(posit) <- "strategy"
    # pnls
  })  # end reactive code
  
  # return the dygraph plot to output argument
  output$dygraph <- dygraphs::renderDygraph({
    # plot daily closing prices
    # dygraphs::dygraph(cbind(closep, datav())[endpoints(closep, on="days")], main=paste(score, "Strategy Using OHLC Technical Indicators")) %>%
      # plot daily closing ES1 prices
      dygraphs::dygraph(datav()[endpoints(closep, on="days")], main=paste(score, "Strategy Using OHLC Technical Indicators")) %>%
      # plot a few days with all the minute bars
      # dygraphs::dygraph(cbind(closep, datav())["2018-02-01/2018-02-07"], main=paste(score, "Strategy Using OHLC Technical Indicators")) %>%
      # plot a few days with all the ES1 minute bars
      # dygraphs::dygraph(cbind(Cl(es1), datav())["2018-02-01/2018-02-07"], main=paste(score, "Strategy Using OHLC Technical Indicators")) %>%
      dyAxis("y", label=symbol, independentTicks=TRUE) %>%
      dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
      # dySeries("strategy", axis="y2", col=c("blue", "red"))
      # dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      # dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=symbol, axis="y", col="blue") %>%
      dySeries(name="strategy", axis="y2", col="red")
  })  # end output plot

  # output$hist <- hist(datav())  # end output hist
  
})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
