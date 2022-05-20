##############################
# This is a shiny app for simulating a contrarian strategy 
# using the number of consecutive close_low or close_high.
# The contrarian strategy uses function HighFreq::roll_count().
# The function HighFreq::roll_count() calculates the the number of 
# consecutive TRUE elements in a Boolean vector, and resets
# the count to zero after every FALSE element.
# 
# Just press the "Run App" button on upper right of this panel.
##############################


## Setup code runs once when the shiny app is started

# Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

############## deprecated ##############
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# You must first compile the following Rcpp file as follows:
# Rcpp::sourceCpp(file="/Users/jerzy/Develop/R/Rcpp/lm_arma.cpp")
# Source the strategy functions
# source("/Users/jerzy/Develop/R/scripts/backtest_functions.R")
# Calculate indicator_s matrix of OHLC technical indicators
# source(file="/Users/jerzy/Develop/R/scripts/load_technical_indicators.R")
# volat[which.max(volat)] <- 0
# volat[which.max(volat)] <- 0
############## end deprecated ##############


# Load daily OHLC data
# com_bo <- HighFreq::SPY
# symbolv <- c("IVW", "VTI", "IWF", "IWD", "IWB", "VYM", "DBC", "IEF", "VEU", "SVXY", "VXX")
# com_bo <- mget(symbolv, envir=rutils::etfenv)
# com_bo <- rutils::do_call(cbind, com_bo)
# com_bo <- zoo::na.locf(com_bo, na.rm=FALSE)
# com_bo <- zoo::na.locf(com_bo, fromLast=TRUE)

# Load ES1 futures data
# load(file="/Users/jerzy/Develop/data/ES1.RData")
# symbol_asset <- "SPY"

# Load combined futures data
# com_bo <- HighFreq::SPY
load(file="/Users/jerzy/Develop/data/combined.RData")
symbolv <- unique(rutils::get_name(colnames(com_bo)))
# UX1: close_high_thresh=2, close_low_thresh=2, lagg=1
# UX1: close_high_thresh=3, close_low_thresh=3, lagg=1

# captiont <- "Contrarian Strategy Using OHLC Technical Indicators"


## End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel("Contrarian Strategy Using OHLC Technical Indicators"),
  
  # Create row with for symbols
  fluidRow(
    # Input the symbol for signal
    column(width=2, selectInput("symbolvignal", label="symbol for signal: ",
                                choices=symbolv, selected=symbolv[1])),
    # Input the symbol for trading
    column(width=2, selectInput("symbol_asset", label="symbol for trading (asset): ",
                                choices=symbolv, selected=symbolv[1])),
    # Input the trade lag
    column(width=2, numericInput("lagg", label="trade lag:",
                                 min=1, max=33, value=3, step=1)),
  ),  # end fluidRow
  
  # Create row for thresholds
  fluidRow(
    # Input the count threshold levels
    column(width=2, numericInput("close_high_thresh", label="close high threshold:",
                                 min=0, max=10, value=2, step=1)),
    column(width=2, numericInput("close_low_thresh", label="close low threshold:",
                                 min=0, max=10, value=2, step=1))
    # Input the long look-back interval
    # column(width=4, sliderInput("look_long", label="look-back long:",
    #                             min=2, max=30, value=11, step=1))
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(dygraphs::dygraphOutput("dygraph"), width=12)
)  # end fluidPage interface


## Define the server code
# The function shinyServer() accepts a function 
# with the arguments "input" and "output".
servfun <- shiny::shinyServer(function(input, output) {

  ## Recalculate the model with new parameters
  # The function shiny::reactive() accepts a block of expressions
  # which calculate the model, and returns the model output.
  datav <- shiny::reactive({
    
    ## Extract model parameters from the argument "input"
    # beta_ret <- input$beta_ret
    # enter <- input$enter
    # exit <- input$exit
    # beta_vol <- input$beta_vol
    # betaskew <- input$betaskew
    # betamoment <- input$betamoment
    # beta_ophi <- input$beta_ophi
    # beta_clhi <- input$beta_clhi
    # look_short <- input$look_short
    # look_long <- input$look_long
    symbolvignal <- input$symbolvignal
    symbol_asset <- input$symbol_asset
    lagg <- input$lagg
    close_high_thresh <- input$close_high_thresh
    close_low_thresh <- input$close_low_thresh
    # weights <- c(beta_ret, enter, beta_vol, betaskew)
    # weights <- c(beta_ret, beta_vol, betaskew, betamoment, beta_ophi, beta_clhi)
    
    ## Set up data for signal
    ohlc <- com_bo[, paste(symbolvignal, c("Open", "High", "Low", "Close"), sep=".")]
    ohlc <- na.omit(ohlc)
    indeks <- zoo::index(ohlc)
    closep <- Cl(ohlc)
    close_num <- as.numeric(closep)
    # returns <- rutils::diffit(closep)
    openp <- Op(ohlc)
    highp <- Hi(ohlc)
    highp <- as.numeric(highp)
    lowp <- Lo(ohlc)
    low_num <- as.numeric(lowp)
    # Set TRUE if close is at the high
    close_high <- (close_num == highp)
    # Count number of consecutive closes is at the high
    close_high_count <- HighFreq::roll_count(close_high)
    # Set TRUE if close is at the low
    close_low <- (close_num == low_num)
    # Count number of consecutive closes is at the low
    close_low_count <- HighFreq::roll_count(close_low)
    openp <- as.numeric(openp)
    open_high <- (openp == highp)
    open_high_count <- HighFreq::roll_count(open_high)
    open_low <- (openp == low_num)
    open_low_count <- HighFreq::roll_count(open_low)
    
    
    # Set up data for trading
    closep <- com_bo[indeks, paste(symbol_asset, "Close", sep=".")]
    closep <- zoo::na.locf(closep, fromLast=TRUE)
    returns <- rutils::diffit(closep)

    ## Simulate strategy
    
    posit <- rep(NA_integer_, NROW(ohlc))
    posit[1] <- 0
    # posit[close_high] <- (-1)
    # posit[close_low] <- 1
    # posit[open_low & close_high] <- (-1)
    # posit[open_high & close_low] <- 1
    posit[close_high_count > close_high_thresh] <- (-1)
    posit[close_low_count > close_low_thresh] <- 1
    posit <- zoo::na.locf(posit)
    posit <- rutils::lagit(posit, lagg=lagg)
    # Calculate number of trades
    # turn_over <- abs(rutils::diffit(posit)) / 2
    # sum(turn_over)/NROW(posit)
    # NROW(posit)/sum(turn_over)
    
    # Calculate pnls
    pnls <- cumsum(posit*returns)
    pnls <- cbind(closep, pnls)#[xts::endpoints(pnls, on="days")]
    # pnls <- xts::to.daily(cbind(closep, pnls))
    # colnames(pnls) <- c("asset", "strategy")
    colnames(pnls) <- c(symbol_asset, "strategy")
    pnls
    # calculate max_min signals
    # max_min <- roll_maxmin(close_num, look_short)
    # close_high_count <- (close_num == max_min[, 1])
    # close_low_count <- (close_num == max_min[, 2])
    
    # calculate signal
    # score <- closep
    # trending signal
    # signal_trend <- calc_signal(ohlc=ohlc, closep=close_num,
    #                             predictor=predictor,
    #                             look_short=look_short, look_long=look_long, high_freq=FALSE)
    # signal_trend <- calc_ma(ohlc=ohlc, closep=close_num,
    #                         predictor=predictor,
    #                         look_back=look_long, high_freq=FALSE)
    
    # mean reverting signal
    # signal_revert <- ohlc[, 1]  # dummy signal
    # signal_revert <- calc_signal(ohlc=ohlc, closep=close_num,
    #                             predictor=predictor,
    #                             look_short=look_short)
    # signal_revert <- HighFreq::roll_zscores(response=close_num, 
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
    # posit <- rutils::lagit(posit, lagg=lagg)

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
    # pnls <- signal_revert
    # pnls <- cumsum(posit*returns)
    # colnames(pnls) <- "strategy"
    
    # sim_revert(signal_revert, returns_trade, close_high, close_high_count, close_low, close_low_count, enter, exit, lagg)
    
    # sim_trend(signal_trend, returns_trade, enter, exit, close_high_trade, close_low_trade, lagg)
    
    # sim_trend(signal_trend, returns, close_high, close_low, enter, exit, lagg)
    # sim_revert_trending(signal_revert, signal_trend, returns, enter, exit, close_high_trade, close_low_trade, lagg)
    # posit <- xts(posit, index(ohlc))
    # colnames(posit) <- "strategy"
    # pnls
  })  # end reactive code
  
  # return the dygraph plot to output argument
  output$dygraph <- dygraphs::renderDygraph({
    colnamev <- colnames(datav())
    captiont <- paste("Contrarian Strategy for", colnamev[1], "Using OHLC Technical Indicators")
    
    # plot daily closing prices
    # dygraphs::dygraph(cbind(closep, datav())[endpoints(closep, on="days")], main=captiont) %>%
      # plot daily closing ES1 prices
      # dygraphs::dygraph(datav(), main=captiont) %>%
      dygraphs::dygraph(datav(), main=captiont) %>%
      # plot a few days with all the minute bars
      # dygraphs::dygraph(cbind(closep, datav())["2018-02-01/2018-02-07"], main=captiont) %>%
      # plot a few days with all the ES1 minute bars
      # dygraphs::dygraph(cbind(Cl(ohlc_trade), datav())["2018-02-01/2018-02-07"], main=captiont) %>%
      # dyAxis("y", label="asset", independentTicks=TRUE) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")
  })  # end output plot

  # output$hist <- hist(datav())  # end output hist
  
})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
