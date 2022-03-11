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
         symbol <- "SPY"
         symbolv <- symbol
         ohlc <- HighFreq::SPY
         # Data setup
         dates <- index(ohlc)
        .n_rows <- NROW(ohlc)
         endpoints <- xts::endpoints(ohlc, on="hours")
         prices <- log(Cl(ohlc))
         returns <- rutils::diffit(prices)
         stdev <- sd(returns[returns<0])
         # prices <- prices[endpoints]
       },
       "otherbars" = {
         cap_tion <- "Strategy for 1-minute LODE Bars"
         symbol <- "LODE"
         symbolv <- symbol
         model_type <- "zscore"
         ohlc <- data.table::fread(file="C:/Develop/predictive/data/lode_oneminutebars.csv", sep=",")
        .n_rows <- NROW(ohlc)
         closep <- log(ohlc$close)
         dates <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out.n_rows)
         closep <- xts::xts(closep, dates)
         returns <- rutils::diffit(closep)
         stdev <- sd(returns[returns<0])
         endpoints <- xts::endpoints(returns, on="hours")
         # Coerce ohlc into a matrix
         ohlc <- ohlc[, c(4, 6, 7, 5)]
         colnames(ohlc) <- c("Open", "High", "Low", "Close")
         # data.table::setDF(ohlc)
         ohlc <- as.matrix(ohlc)
       },
       "tick_data" = {
         cap_tion <- "Strategy for AAPL Tick Data"
         model_type <- "sharpe_ticks"
         # Load AAPL tick data
         symbol <- "AAPL"
         symbolv <- symbol
         raw_ticks <- data.table::fread(file="C:/Develop/predictive/data/aapl20201102.csv", sep=",")
         raw_ticks <- raw_ticks[, .(timestamp=V8, seconds=V3, price=V1, volume=V2)]
         # Bind additional pieces of data together
         foo <- data.table::fread(file="C:/Develop/predictive/data/aapl20201030.csv", sep="\t")
         foo <- foo[, c(1:3, 8)]
         colnames(foo) <- c("price", "volume", "seconds", "timestamp")
         foo <- foo[, .(timestamp, seconds, price, volume)]
         bar <- (last(raw_ticks)$price - first(foo)$price)
         foo[, price := (price + bar)]
         raw_ticks <- rbind(raw_ticks, foo)
         big_ticks <- raw_ticks[volume >= 400]
         prices <- log(big_ticks$price)
         returns <- HighFreq::diff_vec(prices)
        .n_rows <- NROW(returns)
         stdev <- sd(returns[returns<0])
         #.n_rows <- NROW(returns)
         # dim(returns) <- c.n_rows, 1)
         # Make dates unique:
         # dates <- as.POSIXct(big_ticks$seconds, origin="1970-01-01")
         # dates <- xts::make.index.unique(dates)
         endpoints <- 100*(1:.n_rows %/% 100))
         dates <- seq.POSIXt(from=as.POSIXct("2020-01-01", origin="1970-01-01"), by="sec", length.out.n_rows)
         returns <- xts::xts(returns, dates)
       },
       "futures" = {
         # Load the 5-second ES futures bar data collected from IB.
         cap_tion <- "Strategy for Futures"
         model_type <- "sharpe_ohlc"
         data_dir <- "C:/Develop/data/ib_data/"
         # symbol <- "ES"  # S&P500 Emini futures
         symbol <- "QM"  # oil
         symbolv <- symbol
         load(paste0(data_dir, symbol, "_ohlc.RData"))
         # Data setup
         dates <- index(ohlc)
        .n_rows <- NROW(ohlc)
         endpoints <- xts::endpoints(ohlc, on="hours")
         prices <- log(Cl(ohlc))
         returns <- rutils::diffit(prices)
         stdev <- sd(returns[returns<0])
         # prices <- prices[endpoints]
         # returns <- c(0, returns)
         cap_tion <- paste("Contrarian Strategy for", symbol, "Bars")
       },
       "etf" = {
         cap_tion <- "Strategy for ETFs"
         model_type <- "sharpe_ohlc"
         # cap_tion <- paste("Contrarian Strategy for", symbol, "Using the Hampel Filter Over Prices")
         symbolv <- rutils::etfenv$symbolv
         symbol <- "VTI"
         data_env <- rutils::etfenv
         ohlc <- get(symbol, data_env)
         # Data setup
         dates <- index(ohlc)
        .n_rows <- NROW(ohlc)
         endpoints <- xts::endpoints(ohlc, on="days")
         prices <- log(Cl(ohlc))
         returns <- rutils::diffit(prices)
         stdev <- sd(returns[returns<0])
         # prices <- prices[endpoints]
       },
       "sp500" = {
         cap_tion <- "Strategy for S&P500 Stocks"
         model_type <- "sharpe_ohlc"
         # Load S&P500 stocks
         # load("C:/Develop/data/returns100.RData")
         load("C:/Develop/lecture_slides/data/sp500.RData")
         # Select the columns with non-zero returns
         data_env <- sp500env
         symbolv <- ls(data_env)
         symbol <- "AAPL"
         ohlc <- get(symbol, data_env)
         # Data setup
         dates <- index(ohlc)
        .n_rows <- NROW(ohlc)
         endpoints <- xts::endpoints(ohlc, on="days")
         prices <- log(Cl(ohlc))
         returns <- rutils::diffit(prices)
         stdev <- sd(returns[returns<0])
         # prices <- prices[endpoints]
       }
)  # end switch


# cap_tion <- paste("Contrarian Strategy for", symbol, "Using the Returns Scaled by the Price Range")

## End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel(cap_tion),

  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the re_calculate variable is updated
    column(width=12,
           h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
           actionButton("re_calculate", "Recalculate the Model"))
  ),  # end fluidRow

  # Create single row with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol",
                                choices=symbolv, selected=symbol)),
    # Input choice of model
    column(width=2, selectInput("model_type", label="Model type",
                                choices=c("rets_by_range", "sharpe_ohlc", "rescaled_ohlc", "sharpe_ticks", "volatility", "zscore", "sharpe"), selected=model_type)),
    # Input end points interval
    # column(width=2, selectInput("interval", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Look-back", min=3, max=30, value=5, step=1)),
    # Input look-back lag interval
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=1, step=1)),
    
    # Input confirmation signal Boolean
    column(width=2, selectInput("confirm", label="Confirm the signal", choices=c("True", "False"), selected="False")),
    
    # Input threshold interval
    column(width=2, sliderInput("threshold", label="Threshold", min=0.2, max=2.0, value=0.8, step=0.1)),
    # Input minimum trade volume for filtering ticks
    column(width=2, sliderInput("volumes", label="Big tick volume", min=50, max=1000, value=400, step=50)),
    # Input the weight decay parameter
    # column(width=2, sliderInput("lambdav", label="Weight decay:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input model weights type
    # column(width=2, selectInput("typev", label="Portfolio weights type",
    #                             choices=c("max_sharpe", "min_var", "min_varpca", "rank"), selected="rank")),
    # Input number of eigenvalues for regularized matrix inverse
    # column(width=2, sliderInput("max_eigen", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
    # Input the shrinkage intensity
    # column(width=2, sliderInput("alpha", label="Shrinkage intensity",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input the percentile
    # column(width=2, sliderInput("percen_tile", label="percentile:", min=0.01, max=0.45, value=0.1, step=0.01)),
    # Input the strategy coefficient: coeff=1 for momentum, and coeff=-1 for contrarian
    # column(width=2, selectInput("coeff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    column(width=2, numericInput("bid_offer", label="Bid-offer:", value=0.0000, step=0.0001)),
    # If coeff=1 then trending, If coeff=(-1) then contrarian
    # column(width=2, numericInput("coeff", "Trend coefficient:", value=1)),
    column(width=2, selectInput("coeff", label="Trend coefficient",
                                choices=c(1, -1), selected=(-1)))
  ),  # end fluidRow

  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), height=8, width=12)

)  # end fluidPage interface


## Define the server code
servfunc <- function(input, output) {

  # Recalculate the data and rerun the strategy
  datav <- reactive({
    # Get model parameters from input argument
    symbol <- isolate(input$symbol)
    model_type <- isolate(input$model_type)
    look_back <- isolate(input$look_back)
    lagg <- isolate(input$lagg)
    confirm <- isolate(input$confirm)
    # max_eigen <- isolate(input$max_eigen)
    threshold <- isolate(input$threshold)
    volumes <- isolate(input$volumes)
    # look_lag <- isolate(input$look_lag
    # lambdav <- isolate(input$lambdav)
    # typev <- isolate(input$typev)
    # alpha <- isolate(input$alpha)
    # percen_tile <- isolate(input$percen_tile)
    # coeff <- as.numeric(isolate(input$coeff))
    bid_offer <- isolate(input$bid_offer)
    coeff <- as.numeric(isolate(input$coeff))
    # Strategy is recalculated when the re_calculate variable is updated
    input$re_calculate

    
    # Select the data: "spybars", "etf" or "sp500"
    switch(data_type,
           "etf" = {
             # cap_tion <- "Rolling Portfolio Optimization Strategy for ETF Portfolio"
             # cap_tion <- paste("Contrarian Strategy for", symbol, "Using the Hampel Filter Over Prices")
             ohlc <- get(symbol, data_env)
             # Data setup
             dates <- index(ohlc)
            .n_rows <- NROW(ohlc)
             endpoints <- xts::endpoints(ohlc, on="days")
             prices <- log(Cl(ohlc))
             returns <- rutils::diffit(prices)
             stdev <- sd(returns[returns<0])
             # prices <- prices[endpoints]
           },
           "sp500" = {
             # cap_tion <- "Rolling Portfolio Optimization Strategy for Sub-Portfolio of S&P500 Stocks"
             ohlc <- get(symbol, data_env)
             # Data setup
             dates <- index(ohlc)
            .n_rows <- NROW(ohlc)
             endpoints <- xts::endpoints(ohlc, on="days")
             prices <- log(Cl(ohlc))
             returns <- rutils::diffit(prices)
             stdev <- sd(returns[returns<0])
             # prices <- prices[endpoints]
           },
           "tick_data" = {
             big_ticks <- raw_ticks[volume >= volumes]
             prices <- log(big_ticks$price)
             returns <- HighFreq::diff_vec(prices)
             stdev <- sd(returns[returns<0])
            .n_rows <- NROW(returns)
             # dim(returns) <- c.n_rows, 1)
             endpoints <- 100*(1:.n_rows %/% 100))
             dates <- seq.POSIXt(from=as.POSIXct("2020-01-01", origin="1970-01-01"), by="sec", length.out.n_rows)
             returns <- xts::xts(returns, dates)
           }
    )  # end switch
    
    
    # Rerun the strategy
    switch(model_type,
           "rets_by_range" = {
             ## Scale the returns using the price range
             rangev <- log(drop(coredata(Hi(ohlc)/Lo(ohlc))))
             # Average with the price range from previous bar
             rangev <- (rangev + c(0, rangev[-NROW(rangev)]))/2
             # re_scaled <- returns
             re_scaled <- ifelse(rangev>0, returns/rangev, 0)
             ## Scale the returns using the volume
             # volumes <- drop(coredata(Vo(ohlc)))
             # Average with the price range from previous bar
             # volumes <- (volumes + c(0, volumes[-NROW(volumes)]))/2
             # re_scaled <- ifelse(volumes>0, returns/sqrt(volumes), 0)
             # re_scaled <- re_scaled/sd(re_scaled)
             ## Backtest strategy for flipping if two consecutive positive and negative returns
             position_s <- rep(NA_integer_,.n_rows)
             position_s[1] <- 0
             # Flip position if the scaled returns exceed threshold
             position_s[re_scaled > threshold] <- 1
             position_s[re_scaled < (-threshold)] <- (-1)
             # LOCF
             position_s <- zoo::na.locf(position_s, na.rm=FALSE)
             position_s <- rutils::lagit(position_s, lagg=lagg)
           },
           "sharpe_ohlc" = {  # For OHLC data
             # Scale the cumulative returns by the trailing volatility
             rangev <- HighFreq::roll_sum(returns, look_back=look_back)
             var_rolling <- sqrt(HighFreq::roll_var_ohlc(ohlc, look_back=look_back, scalit=FALSE))
             look_back <- sqrt(look_back)
             re_scaled <- ifelse((var_rolling==0) | (rangev==0),
                                 0.0,
                                 rangev/var_rolling/look_back)
             # Colnames(re_scaled) <- paste0(rutils::get_name(colnames(ohlc)[1]), ".Hurst")
             re_scaled <- rutils::na_locf(re_scaled)
             # quantiles are calculated over first decile to avoid snooping
             quantiles <- quantile(re_scaled[1:.n_rows %/% 10), ], c(1-threshold, threshold))
             # quantiles <- quantile(re_scaled, c(1-threshold, threshold))
             position_s <- rep(NA_integer_,.n_rows)
             position_s[1] <- 0

             if (confirm == "False") {
               position_s <- ifelse(re_scaled > quantiles[2], 1, position_s)
               position_s <- ifelse(re_scaled < quantiles[1], -1, position_s)
             } else if (confirm == "True") {
               # Flip only if two consecutive signals in same direction
               lagg_ed <- rutils::lagit(re_scaled, 1)
               position_s <- ifelse((re_scaled > quantiles[2]) & (lagg_ed > quantiles[2]), 1, position_s)
               position_s <- ifelse((re_scaled < quantiles[1]) & (lagg_ed < quantiles[1]), -1, position_s)
             }  # end if
             
             position_s <- zoo::na.locf(position_s)
             position_s <- rutils::lagit(position_s, lagg)
             
           },
           "rescaled_ohlc" = {  # For OHLC data
             # Scale the cumulative returns by the trailing volatility
             ma_x <- RcppRoll::roll_max(Hi(ohlc), n=look_back, align="right")
             # mi_n <- -RcppRoll::roll_max(-Lo(ohlc), n=look_back, align="right")
             # me_an <- RcppRoll::roll_mean(returns, n=look_back, align="right")
             medi_an <- RcppRoll::roll_median(returns, n=look_back, align="right")
             re_scaled <- (ma_x - medi_an)/medi_an
             re_scaled <- c(rep(1, look_back-1), re_scaled)
             # rangev <- returns
             # var_rolling <- sqrt(HighFreq::roll_var_ohlc(ohlc, look_back=look_back, scalit=FALSE))
             # look_back <- sqrt(look_back)
             # re_scaled <- ifelse((var_rolling==0) | (rangev==0),
             #                     0.0,
             #                     rangev/var_rolling*look_back)
             # Colnames(re_scaled) <- paste0(rutils::get_name(colnames(ohlc)[1]), ".Hurst")
             re_scaled <- rutils::na_locf(re_scaled)
             # quantiles are calculated over first decile to avoid snooping
             quantiles <- quantile(re_scaled[1:.n_rows %/% 10), ], c(1-threshold, threshold))
             # quantiles <- quantile(re_scaled, c(1-threshold, threshold))
             position_s <- rep(NA_integer_,.n_rows)
             position_s[1] <- 0
             
             if (confirm == "False") {
               position_s <- ifelse(re_scaled > quantiles[2], 1, position_s)
               position_s <- ifelse(re_scaled < quantiles[1], -1, position_s)
             } else if (confirm == "True") {
               # Flip only if two consecutive signals in same direction
               lagg_ed <- rutils::lagit(re_scaled, 1)
               position_s <- ifelse((re_scaled > quantiles[2]) & (lagg_ed > quantiles[2]), 1, position_s)
               position_s <- ifelse((re_scaled < quantiles[1]) & (lagg_ed < quantiles[1]), -1, position_s)
             }  # end if
             
             position_s <- zoo::na.locf(position_s)
             position_s <- rutils::lagit(position_s, lagg)
           },
           "sharpe_ticks" = {
             # Scale the cumulative returns by the trailing volatility
             rangev <- HighFreq::roll_sum(returns, look_back=look_back)
             var_rolling <- sqrt(HighFreq::roll_var(returns, look_back=look_back))
             look_back <- sqrt(look_back)
             re_scaled <- ifelse((var_rolling==0) | (rangev==0),
                                 0.0,
                                 rangev/var_rolling/look_back)
             re_scaled <- rutils::na_locf(re_scaled)
             # quantiles are calculated over first decile to avoid snooping
             quantiles <- quantile(re_scaled[1:.n_rows %/% 10), ], c(1-threshold, threshold))
             position_s <- rep(NA_integer_,.n_rows)
             position_s[1] <- 0
             
             if (confirm == "False") {
               position_s <- ifelse(re_scaled > quantiles[2], 1, position_s)
               position_s <- ifelse(re_scaled < quantiles[1], -1, position_s)
             } else if (confirm == "True") {
               # Flip only if two consecutive signals in same direction
               lagg_ed <- rutils::lagit(re_scaled, 1)
               position_s <- ifelse((re_scaled > quantiles[2]) & (lagg_ed > quantiles[2]), 1, position_s)
               position_s <- ifelse((re_scaled < quantiles[1]) & (lagg_ed < quantiles[1]), -1, position_s)
             }  # end if

             position_s <- zoo::na.locf(position_s)
             position_s <- rutils::lagit(position_s, lagg)
           },
           "volatility" = {  # For OHLC data
             # Calculate trailing volatilities
             var_rolling <- sqrt(HighFreq::roll_var_ohlc(ohlc, look_back=look_back, scalit=FALSE))
             # quantiles <- quantile(var_rolling, c(1-threshold, threshold))
             quantiles <- quantile(var_rolling[1:.n_rows %/% 10), ], c(1-threshold, threshold))
             position_s <- rep(NA_integer_,.n_rows)
             position_s[1] <- 0
             position_s <- ifelse(var_rolling > quantiles[2], 1, position_s)
             position_s <- ifelse(var_rolling < quantiles[1], -1, position_s)
             position_s <- zoo::na.locf(position_s)
             position_s <- rutils::lagit(position_s, lagg)
           },
           "zscore" = {
             # Calculate trailing z-scores
             # design <- matrix(1.n_rows, nc=1)
             design <- matrix(1.n_rows, nc=1)
             z_scores <- HighFreq::roll_zscores(response=returns, design=design, look_back=look_back)
             # colnames(z_scores) <- "zscore"
             z_scores[1:look_back] <- 0
             z_scores[is.infinite(z_scores)] <- 0
             z_scores[is.na(z_scores)] <- 0
             quantiles <- quantile(z_scores[1:.n_rows %/% 10), ], c(1-threshold, threshold))
             # quantiles <- quantile(z_scores, c(1-threshold, threshold))
             position_s <- rep(NA_integer_,.n_rows)
             position_s[1] <- 0
             
             if (confirm == "False") {
               position_s <- ifelse(z_scores > quantiles[2], 1, position_s)
               position_s <- ifelse(z_scores < quantiles[1], -1, position_s)
             } else if (confirm == "True") {
               # Flip only if two consecutive signals in same direction
               lagg_ed <- rutils::lagit(z_scores, 1)
               position_s <- ifelse((z_scores > quantiles[2]) & (lagg_ed > quantiles[2]), 1, position_s)
               position_s <- ifelse((z_scores < quantiles[1]) & (lagg_ed < quantiles[1]), -1, position_s)
             }  # end if
             
             position_s <- zoo::na.locf(position_s)
             position_s <- rutils::lagit(position_s, lagg)
           }
    )  # end switch

    # Calculate position turnover
    turn_over <- abs(rutils::diffit(position_s)) / 2
    # Calculate number of trades
    # sum(turn_over)/NROW(position_s)
    # Calculate strategy pnls
    pnls <- (coeff*position_s*returns)

    # Calculate transaction costs
    costs <- bid_offer*turn_over
    pnls <- (pnls - costs)

    pnls <- stdev*pnls/sd(pnls[pnls<0])
    pnls <- cbind(pnls, returns)
    # sharp_e <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x))
    sharp_e <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    sharp_e <- round(sharp_e, 3)
    pnls <- cumsum(pnls)

    ## Coerce pnls to xts
    # pnls <- xts(pnls, dates)
    colnames(pnls) <- paste0(c("Strategy SR=", "Index SR="), sharp_e)
    pnls[c(1, endpoints), ]
  })  # end reactive code

  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    colnamev <- colnames(datav())
    dygraphs::dygraph(datav(), main=cap_tion) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue") %>%
      dyLegend(width=500)
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfunc)
