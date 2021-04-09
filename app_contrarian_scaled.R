##############################
# This is a shiny app for simulating a contrarian strategy
# using returns scaled by the price range.
#
# Just press the "Run App" button on upper right of this panel.
##############################


## To-do
# Adapt lagg confirmation signal code from app_ewma.R using HighFreq::roll_vec() or HighFreq::roll_count()


## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

# Comments:
# The data_type = "tick_data" goes together with model_type = "sharpe_ticks"


# Select the data: "spybars", "otherbars", "etf", "tick_data", or "sp500"
data_type <- "otherbars"
switch(data_type,
       "spybars" = {
         cap_tion <- "Strategy for 1-minute SPY Bars"
         model_type <- "sharpe_ohlc"
         # Load 1-minute SPY bars
         sym_bol <- "SPY"
         sym_bols <- sym_bol
         oh_lc <- HighFreq::SPY
         # Data setup
         date_s <- index(oh_lc)
         n_rows <- NROW(oh_lc)
         end_points <- xts::endpoints(oh_lc, on="hours")
         price_s <- log(Cl(oh_lc))
         re_turns <- rutils::diff_it(price_s)
         std_dev <- sd(re_turns[re_turns<0])
         # price_s <- price_s[end_points]
       },
       "otherbars" = {
         cap_tion <- "Strategy for 1-minute LODE Bars"
         sym_bol <- "LODE"
         sym_bols <- sym_bol
         model_type <- "zscore"
         oh_lc <- data.table::fread(file="C:/Develop/predictive/data/lode_oneminutebars.csv", sep=",")
         n_rows <- NROW(oh_lc)
         clos_e <- log(oh_lc$close)
         date_s <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out=n_rows)
         clos_e <- xts::xts(clos_e, date_s)
         re_turns <- rutils::diff_it(clos_e)
         std_dev <- sd(re_turns[re_turns<0])
         end_points <- xts::endpoints(re_turns, on="hours")
         # Coerce oh_lc into a matrix
         oh_lc <- oh_lc[, c(4, 6, 7, 5)]
         colnames(oh_lc) <- c("Open", "High", "Low", "Close")
         # data.table::setDF(oh_lc)
         oh_lc <- as.matrix(oh_lc)
       },
       "tick_data" = {
         cap_tion <- "Strategy for AAPL Tick Data"
         model_type <- "sharpe_ticks"
         # Load AAPL tick data
         sym_bol <- "AAPL"
         sym_bols <- sym_bol
         raw_ticks <- data.table::fread(file="C:/Develop/predictive/data/aapl_20201102.csv", sep=",")
         raw_ticks <- raw_ticks[, .(timestamp=V8, seconds=V3, price=V1, volume=V2)]
         # Bind additional pieces of data together
         foo <- data.table::fread(file="C:/Develop/predictive/data/aapl_20201030.csv", sep="\t")
         foo <- foo[, c(1:3, 8)]
         colnames(foo) <- c("price", "volume", "seconds", "timestamp")
         foo <- foo[, .(timestamp, seconds, price, volume)]
         bar <- (last(raw_ticks)$price - first(foo)$price)
         foo[, price := (price + bar)]
         raw_ticks <- rbind(raw_ticks, foo)
         big_ticks <- raw_ticks[volume >= 400]
         price_s <- log(big_ticks$price)
         re_turns <- HighFreq::diff_vec(price_s)
         n_rows <- NROW(re_turns)
         std_dev <- sd(re_turns[re_turns<0])
         # n_rows <- NROW(re_turns)
         # dim(re_turns) <- c(n_rows, 1)
         # Make dates unique:
         # date_s <- as.POSIXct(big_ticks$seconds, origin="1970-01-01")
         # date_s <- xts::make.index.unique(date_s)
         end_points <- 100*(1:(n_rows %/% 100))
         date_s <- seq.POSIXt(from=as.POSIXct("2020-01-01", origin="1970-01-01"), by="sec", length.out=n_rows)
         re_turns <- xts::xts(re_turns, date_s)
       },
       "futures" = {
         # Load the 5-second ES futures bar data collected from IB.
         cap_tion <- "Strategy for Futures"
         model_type <- "sharpe_ohlc"
         data_dir <- "C:/Develop/data/ib_data/"
         # sym_bol <- "ES"  # S&P500 Emini futures
         sym_bol <- "QM"  # oil
         sym_bols <- sym_bol
         load(paste0(data_dir, sym_bol, "_ohlc.RData"))
         # Data setup
         date_s <- index(oh_lc)
         n_rows <- NROW(oh_lc)
         end_points <- xts::endpoints(oh_lc, on="hours")
         price_s <- log(Cl(oh_lc))
         re_turns <- rutils::diff_it(price_s)
         std_dev <- sd(re_turns[re_turns<0])
         # price_s <- price_s[end_points]
         # re_turns <- c(0, re_turns)
         cap_tion <- paste("Contrarian Strategy for", sym_bol, "Bars")
       },
       "etf" = {
         cap_tion <- "Strategy for ETFs"
         model_type <- "sharpe_ohlc"
         # cap_tion <- paste("Contrarian Strategy for", sym_bol, "Using the Hampel Filter Over Prices")
         sym_bols <- rutils::etf_env$sym_bols
         sym_bol <- "VTI"
         data_env <- rutils::etf_env
         oh_lc <- get(sym_bol, data_env)
         # Data setup
         date_s <- index(oh_lc)
         n_rows <- NROW(oh_lc)
         end_points <- xts::endpoints(oh_lc, on="days")
         price_s <- log(Cl(oh_lc))
         re_turns <- rutils::diff_it(price_s)
         std_dev <- sd(re_turns[re_turns<0])
         # price_s <- price_s[end_points]
       },
       "sp500" = {
         cap_tion <- "Strategy for S&P500 Stocks"
         model_type <- "sharpe_ohlc"
         # Load S&P500 stocks
         # load("C:/Develop/data/returns_100.RData")
         load("C:/Develop/lecture_slides/data/sp500.RData")
         # Select the columns with non-zero returns
         data_env <- sp500_env
         sym_bols <- ls(data_env)
         sym_bol <- "AAPL"
         oh_lc <- get(sym_bol, data_env)
         # Data setup
         date_s <- index(oh_lc)
         n_rows <- NROW(oh_lc)
         end_points <- xts::endpoints(oh_lc, on="days")
         price_s <- log(Cl(oh_lc))
         re_turns <- rutils::diff_it(price_s)
         std_dev <- sd(re_turns[re_turns<0])
         # price_s <- price_s[end_points]
       }
)  # end switch


# cap_tion <- paste("Contrarian Strategy for", sym_bol, "Using the Returns Scaled by the Price Range")

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(cap_tion),

  fluidRow(
    # The Shiny App is re-calculated when the actionButton is clicked and the re_calculate variable is updated
    column(width=12,
           h4("Click the button 'Recalculate the Model' to re-calculate the Shiny App."),
           actionButton("re_calculate", "Recalculate the Model"))
  ),  # end fluidRow

  # Create single row with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=3, selectInput("sym_bol", label="Symbol",
                                choices=sym_bols, selected=sym_bol)),
    # Input choice of model
    column(width=3, selectInput("model_type", label="Model type",
                                choices=c("rets_by_range", "sharpe_ohlc", "rescaled_ohlc", "sharpe_ticks", "volatility", "zscore", "sharpe"), selected=model_type)),
    # Input end points interval
    # column(width=3, selectInput("inter_val", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Look-back", min=3, max=30, value=5, step=1)),
    # Input look-back lag interval
    column(width=3, sliderInput("lagg", label="lagg", min=1, max=5, value=1, step=1)),
    
    # Input confirmation signal Boolean
    column(width=3, selectInput("con_firm", label="Confirm the signal", choices=c("True", "False"), selected="False")),
    
    # Input threshold interval
    column(width=3, sliderInput("thresh_old", label="Threshold", min=0.2, max=2.0, value=0.8, step=0.1)),
    # Input minimum trade volume for filtering ticks
    column(width=3, sliderInput("vol_ume", label="Big tick volume", min=50, max=1000, value=400, step=50)),
    # Input the weight decay parameter
    # column(width=3, sliderInput("lamb_da", label="Weight decay:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input model weights type
    # column(width=3, selectInput("typ_e", label="Portfolio weights type",
    #                             choices=c("max_sharpe", "min_var", "min_varpca", "rank"), selected="rank")),
    # Input number of eigenvalues for regularized matrix inverse
    # column(width=3, sliderInput("max_eigen", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
    # Input the shrinkage intensity
    # column(width=3, sliderInput("al_pha", label="Shrinkage intensity",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input the percentile
    # column(width=3, sliderInput("percen_tile", label="percentile:", min=0.01, max=0.45, value=0.1, step=0.01)),
    # Input the strategy coefficient: fac_tor=1 for momentum, and fac_tor=-1 for contrarian
    # column(width=3, selectInput("fac_tor", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    column(width=3, numericInput("bid_offer", label="Bid-offer:", value=0.0000, step=0.0001)),
    # If fac_tor=1 then trending, If fac_tor=(-1) then contrarian
    # column(width=3, numericInput("fac_tor", "Trend coefficient:", value=1)),
    column(width=3, selectInput("fac_tor", label="Trend coefficient",
                                choices=c(1, -1), selected=(-1)))
  ),  # end fluidRow

  # Create output plot panel
  mainPanel(dygraphOutput("dy_graph"), height=8, width=12)

)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {

  # Re-calculate the data and rerun the strategy
  da_ta <- reactive({
    # Get model parameters from input argument
    sym_bol <- isolate(input$sym_bol)
    model_type <- isolate(input$model_type)
    look_back <- isolate(input$look_back)
    lagg <- isolate(input$lagg)
    con_firm <- isolate(input$con_firm)
    # max_eigen <- isolate(input$max_eigen)
    thresh_old <- isolate(input$thresh_old)
    vol_ume <- isolate(input$vol_ume)
    # look_lag <- isolate(input$look_lag
    # lamb_da <- isolate(input$lamb_da)
    # typ_e <- isolate(input$typ_e)
    # al_pha <- isolate(input$al_pha)
    # percen_tile <- isolate(input$percen_tile)
    # fac_tor <- as.numeric(isolate(input$fac_tor))
    bid_offer <- isolate(input$bid_offer)
    fac_tor <- as.numeric(isolate(input$fac_tor))
    # Strategy is re-calculated when the re_calculate variable is updated
    input$re_calculate

    
    # Select the data: "spybars", "etf" or "sp500"
    switch(data_type,
           "etf" = {
             # cap_tion <- "Rolling Portfolio Optimization Strategy for ETF Portfolio"
             # cap_tion <- paste("Contrarian Strategy for", sym_bol, "Using the Hampel Filter Over Prices")
             oh_lc <- get(sym_bol, data_env)
             # Data setup
             date_s <- index(oh_lc)
             n_rows <- NROW(oh_lc)
             end_points <- xts::endpoints(oh_lc, on="days")
             price_s <- log(Cl(oh_lc))
             re_turns <- rutils::diff_it(price_s)
             std_dev <- sd(re_turns[re_turns<0])
             # price_s <- price_s[end_points]
           },
           "sp500" = {
             # cap_tion <- "Rolling Portfolio Optimization Strategy for Sub-Portfolio of S&P500 Stocks"
             oh_lc <- get(sym_bol, data_env)
             # Data setup
             date_s <- index(oh_lc)
             n_rows <- NROW(oh_lc)
             end_points <- xts::endpoints(oh_lc, on="days")
             price_s <- log(Cl(oh_lc))
             re_turns <- rutils::diff_it(price_s)
             std_dev <- sd(re_turns[re_turns<0])
             # price_s <- price_s[end_points]
           },
           "tick_data" = {
             big_ticks <- raw_ticks[volume >= vol_ume]
             price_s <- log(big_ticks$price)
             re_turns <- HighFreq::diff_vec(price_s)
             std_dev <- sd(re_turns[re_turns<0])
             n_rows <- NROW(re_turns)
             # dim(re_turns) <- c(n_rows, 1)
             end_points <- 100*(1:(n_rows %/% 100))
             date_s <- seq.POSIXt(from=as.POSIXct("2020-01-01", origin="1970-01-01"), by="sec", length.out=n_rows)
             re_turns <- xts::xts(re_turns, date_s)
           }
    )  # end switch
    
    
    # Rerun the strategy
    switch(model_type,
           "rets_by_range" = {
             ## Scale the returns using the price range
             rang_e <- log(drop(coredata(Hi(oh_lc)/Lo(oh_lc))))
             # Average with the price range from previous bar
             rang_e <- (rang_e + c(0, rang_e[-NROW(rang_e)]))/2
             # re_scaled <- re_turns
             re_scaled <- ifelse(rang_e>0, re_turns/rang_e, 0)
             ## Scale the returns using the volume
             # vol_ume <- drop(coredata(Vo(oh_lc)))
             # Average with the price range from previous bar
             # vol_ume <- (vol_ume + c(0, vol_ume[-NROW(vol_ume)]))/2
             # re_scaled <- ifelse(vol_ume>0, re_turns/sqrt(vol_ume), 0)
             # re_scaled <- re_scaled/sd(re_scaled)
             ## Backtest strategy for flipping if two consecutive positive and negative returns
             position_s <- rep(NA_integer_, n_rows)
             position_s[1] <- 0
             # Flip position if the scaled returns exceed thresh_old
             position_s[re_scaled > thresh_old] <- 1
             position_s[re_scaled < (-thresh_old)] <- (-1)
             # LOCF
             position_s <- zoo::na.locf(position_s, na.rm=FALSE)
             position_s <- rutils::lag_it(position_s, lagg=lagg)
           },
           "sharpe_ohlc" = {  # For OHLC data
             # Scale the cumulative re_turns by the trailing volatility
             ran_ge <- HighFreq::roll_sum(re_turns, look_back=look_back)
             var_rolling <- sqrt(HighFreq::roll_var_ohlc(oh_lc, look_back=look_back, scal_e=FALSE))
             look_back <- sqrt(look_back)
             re_scaled <- ifelse((var_rolling==0) | (ran_ge==0),
                                 0.0,
                                 ran_ge/var_rolling/look_back)
             # Colnames(re_scaled) <- paste0(rutils::get_name(colnames(oh_lc)[1]), ".Hurst")
             re_scaled <- rutils::na_locf(re_scaled)
             # quantile_s are calculated over first decile to avoid snooping
             quantile_s <- quantile(re_scaled[1:(n_rows %/% 10), ], c(1-thresh_old, thresh_old))
             # quantile_s <- quantile(re_scaled, c(1-thresh_old, thresh_old))
             position_s <- rep(NA_integer_, n_rows)
             position_s[1] <- 0

             if (con_firm == "False") {
               position_s <- ifelse(re_scaled > quantile_s[2], 1, position_s)
               position_s <- ifelse(re_scaled < quantile_s[1], -1, position_s)
             } else if (con_firm == "True") {
               # Flip only if two consecutive signals in same direction
               lagg_ed <- rutils::lag_it(re_scaled, 1)
               position_s <- ifelse((re_scaled > quantile_s[2]) & (lagg_ed > quantile_s[2]), 1, position_s)
               position_s <- ifelse((re_scaled < quantile_s[1]) & (lagg_ed < quantile_s[1]), -1, position_s)
             }  # end if
             
             position_s <- zoo::na.locf(position_s)
             position_s <- rutils::lag_it(position_s, lagg)
             
           },
           "rescaled_ohlc" = {  # For OHLC data
             # Scale the cumulative re_turns by the trailing volatility
             ma_x <- RcppRoll::roll_max(Hi(oh_lc), n=look_back, align="right")
             # mi_n <- -RcppRoll::roll_max(-Lo(oh_lc), n=look_back, align="right")
             # me_an <- RcppRoll::roll_mean(re_turns, n=look_back, align="right")
             medi_an <- RcppRoll::roll_median(re_turns, n=look_back, align="right")
             re_scaled <- (ma_x - medi_an)/medi_an
             re_scaled <- c(rep(1, look_back-1), re_scaled)
             # ran_ge <- re_turns
             # var_rolling <- sqrt(HighFreq::roll_var_ohlc(oh_lc, look_back=look_back, scal_e=FALSE))
             # look_back <- sqrt(look_back)
             # re_scaled <- ifelse((var_rolling==0) | (ran_ge==0),
             #                     0.0,
             #                     ran_ge/var_rolling*look_back)
             # Colnames(re_scaled) <- paste0(rutils::get_name(colnames(oh_lc)[1]), ".Hurst")
             re_scaled <- rutils::na_locf(re_scaled)
             # quantile_s are calculated over first decile to avoid snooping
             quantile_s <- quantile(re_scaled[1:(n_rows %/% 10), ], c(1-thresh_old, thresh_old))
             # quantile_s <- quantile(re_scaled, c(1-thresh_old, thresh_old))
             position_s <- rep(NA_integer_, n_rows)
             position_s[1] <- 0
             
             if (con_firm == "False") {
               position_s <- ifelse(re_scaled > quantile_s[2], 1, position_s)
               position_s <- ifelse(re_scaled < quantile_s[1], -1, position_s)
             } else if (con_firm == "True") {
               # Flip only if two consecutive signals in same direction
               lagg_ed <- rutils::lag_it(re_scaled, 1)
               position_s <- ifelse((re_scaled > quantile_s[2]) & (lagg_ed > quantile_s[2]), 1, position_s)
               position_s <- ifelse((re_scaled < quantile_s[1]) & (lagg_ed < quantile_s[1]), -1, position_s)
             }  # end if
             
             position_s <- zoo::na.locf(position_s)
             position_s <- rutils::lag_it(position_s, lagg)
           },
           "sharpe_ticks" = {
             # Scale the cumulative re_turns by the trailing volatility
             ran_ge <- HighFreq::roll_sum(re_turns, look_back=look_back)
             var_rolling <- sqrt(HighFreq::roll_var(re_turns, look_back=look_back))
             look_back <- sqrt(look_back)
             re_scaled <- ifelse((var_rolling==0) | (ran_ge==0),
                                 0.0,
                                 ran_ge/var_rolling/look_back)
             re_scaled <- rutils::na_locf(re_scaled)
             # quantile_s are calculated over first decile to avoid snooping
             quantile_s <- quantile(re_scaled[1:(n_rows %/% 10), ], c(1-thresh_old, thresh_old))
             position_s <- rep(NA_integer_, n_rows)
             position_s[1] <- 0
             
             if (con_firm == "False") {
               position_s <- ifelse(re_scaled > quantile_s[2], 1, position_s)
               position_s <- ifelse(re_scaled < quantile_s[1], -1, position_s)
             } else if (con_firm == "True") {
               # Flip only if two consecutive signals in same direction
               lagg_ed <- rutils::lag_it(re_scaled, 1)
               position_s <- ifelse((re_scaled > quantile_s[2]) & (lagg_ed > quantile_s[2]), 1, position_s)
               position_s <- ifelse((re_scaled < quantile_s[1]) & (lagg_ed < quantile_s[1]), -1, position_s)
             }  # end if

             position_s <- zoo::na.locf(position_s)
             position_s <- rutils::lag_it(position_s, lagg)
           },
           "volatility" = {  # For OHLC data
             # Calculate trailing volatilities
             var_rolling <- sqrt(HighFreq::roll_var_ohlc(oh_lc, look_back=look_back, scal_e=FALSE))
             # quantile_s <- quantile(var_rolling, c(1-thresh_old, thresh_old))
             quantile_s <- quantile(var_rolling[1:(n_rows %/% 10), ], c(1-thresh_old, thresh_old))
             position_s <- rep(NA_integer_, n_rows)
             position_s[1] <- 0
             position_s <- ifelse(var_rolling > quantile_s[2], 1, position_s)
             position_s <- ifelse(var_rolling < quantile_s[1], -1, position_s)
             position_s <- zoo::na.locf(position_s)
             position_s <- rutils::lag_it(position_s, lagg)
           },
           "zscore" = {
             # Calculate trailing z-scores
             # de_sign <- matrix(1:n_rows, nc=1)
             de_sign <- matrix(1:n_rows, nc=1)
             z_scores <- HighFreq::roll_zscores(res_ponse=re_turns, de_sign=de_sign, look_back=look_back)
             # colnames(z_scores) <- "zscore"
             z_scores[1:look_back] <- 0
             z_scores[is.infinite(z_scores)] <- 0
             z_scores[is.na(z_scores)] <- 0
             quantile_s <- quantile(z_scores[1:(n_rows %/% 10), ], c(1-thresh_old, thresh_old))
             # quantile_s <- quantile(z_scores, c(1-thresh_old, thresh_old))
             position_s <- rep(NA_integer_, n_rows)
             position_s[1] <- 0
             
             if (con_firm == "False") {
               position_s <- ifelse(z_scores > quantile_s[2], 1, position_s)
               position_s <- ifelse(z_scores < quantile_s[1], -1, position_s)
             } else if (con_firm == "True") {
               # Flip only if two consecutive signals in same direction
               lagg_ed <- rutils::lag_it(z_scores, 1)
               position_s <- ifelse((z_scores > quantile_s[2]) & (lagg_ed > quantile_s[2]), 1, position_s)
               position_s <- ifelse((z_scores < quantile_s[1]) & (lagg_ed < quantile_s[1]), -1, position_s)
             }  # end if
             
             position_s <- zoo::na.locf(position_s)
             position_s <- rutils::lag_it(position_s, lagg)
           }
    )  # end switch

    # Calculate position turnover
    turn_over <- abs(rutils::diff_it(position_s)) / 2
    # Calculate number of trades
    # sum(turn_over)/NROW(position_s)
    # Calculate strategy pnl_s
    pnl_s <- (fac_tor*position_s*re_turns)

    # Calculate transaction costs
    cost_s <- bid_offer*turn_over
    pnl_s <- (pnl_s - cost_s)

    pnl_s <- std_dev*pnl_s/sd(pnl_s[pnl_s<0])
    pnl_s <- cbind(pnl_s, re_turns)
    # sharp_e <- sqrt(252)*sapply(pnl_s, function(x) mean(x)/sd(x))
    sharp_e <- sqrt(252)*sapply(pnl_s, function(x) mean(x)/sd(x[x<0]))
    sharp_e <- round(sharp_e, 3)
    pnl_s <- cumsum(pnl_s)

    ## Coerce pnl_s to xts
    # pnl_s <- xts(pnl_s, date_s)
    colnames(pnl_s) <- paste0(c("Strategy SR=", "Index SR="), sharp_e)
    pnl_s[c(1, end_points), ]
  })  # end reactive code

  # Return to the output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph({
    col_names <- colnames(da_ta())
    dygraphs::dygraph(da_ta(), main=cap_tion) %>%
      dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
      dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
      dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue") %>%
      dyLegend(width=500)
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
