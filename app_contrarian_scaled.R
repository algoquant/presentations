##############################
# This is a shiny app for simulating a contrarian strategy
# using returns scaled by the price range.
#
# Just press the "Run App" button on upper right of this panel.
##############################


## To-do
# Adapt lagg confirmation signal code from app_ewma.R using HighFreq::roll_sum() or HighFreq::roll_count()


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
         captiont <- "Strategy for 1-minute SPY Bars"
         model_type <- "sharpe_ohlc"
         # Load 1-minute SPY bars
         symbol <- "SPY"
         symbolv <- symbol
         ohlc <- HighFreq::SPY
         # Data setup
         dates <- index(ohlc)
         nrows <- NROW(ohlc)
         endp <- xts::endpoints(ohlc, on="hours")
         pricev <- log(Cl(ohlc))
         retv <- rutils::diffit(pricev)
         stdev <- sd(retv[returns<0])
         # pricev <- prices[endp]
       },
       "otherbars" = {
         captiont <- "Strategy for 1-minute LODE Bars"
         symbol <- "LODE"
         symbolv <- symbol
         model_type <- "zscore"
         ohlc <- data.table::fread(file="/Volumes/external/Develop/Predictive/data/lode_oneminutebars.csv", sep=",")
         nrows <- NROW(ohlc)
         closep <- log(ohlc$close)
         dates <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out=nrows)
         closep <- xts::xts(closep, dates)
         retv <- rutils::diffit(closep)
         stdev <- sd(retv[returns<0])
         endp <- xts::endpoints(retv, on="hours")
         # Coerce ohlc into a matrix
         ohlc <- ohlc[, c(4, 6, 7, 5)]
         colnames(ohlc) <- c("Open", "High", "Low", "Close")
         # data.table::setDF(ohlc)
         ohlc <- as.matrix(ohlc)
       },
       "tick_data" = {
         captiont <- "Strategy for AAPL Tick Data"
         model_type <- "sharpe_ticks"
         # Load AAPL tick data
         symbol <- "AAPL"
         symbolv <- symbol
         raw_ticks <- data.table::fread(file="/Volumes/external/Develop/Predictive/data/aapl20201102.csv", sep=",")
         raw_ticks <- raw_ticks[, .(timestamp=V8, seconds=V3, price=V1, volume=V2)]
         # Bind additional pieces of data together
         foo <- data.table::fread(file="/Volumes/external/Develop/Predictive/data/aapl20201030.csv", sep="\t")
         foo <- foo[, c(1:3, 8)]
         colnames(foo) <- c("price", "volume", "seconds", "timestamp")
         foo <- foo[, .(timestamp, seconds, price, volume)]
         bar <- (last(raw_ticks)$price - first(foo)$price)
         foo[, price := (price + bar)]
         raw_ticks <- rbind(raw_ticks, foo)
         big_ticks <- raw_ticks[volume >= 400]
         pricev <- log(big_ticks$price)
         retv <- HighFreq::diff_vec(pricev)
         nrows <- NROW(retv)
         stdev <- sd(retv[returns<0])
         # nrows <- NROW(retv)
         # dim(retv) <- c(nrows, 1)
         # Make dates unique:
         # dates <- as.POSIXct(big_ticks$seconds, origin="1970-01-01")
         # dates <- xts::make.index.unique(dates)
         endp <- 100*(1:(nrows %/% 100))
         dates <- seq.POSIXt(from=as.POSIXct("2020-01-01", origin="1970-01-01"), by="sec", length.out=nrows)
         retv <- xts::xts(retv, dates)
       },
       "futures" = {
         # Load the 5-second ES futures bar data collected from IB.
         captiont <- "Strategy for Futures"
         model_type <- "sharpe_ohlc"
         data_dir <- "/Users/jerzy/Develop/data/ib_data/"
         # symbol <- "ES"  # S&P500 Emini futures
         symbol <- "QM"  # oil
         symbolv <- symbol
         load(paste0(data_dir, symbol, "_ohlc.RData"))
         # Data setup
         dates <- index(ohlc)
         nrows <- NROW(ohlc)
         endp <- xts::endpoints(ohlc, on="hours")
         pricev <- log(Cl(ohlc))
         retv <- rutils::diffit(pricev)
         stdev <- sd(retv[returns<0])
         # pricev <- prices[endp]
         # retv <- c(0, retv)
         captiont <- paste("Contrarian Strategy for", symbol, "Bars")
       },
       "etf" = {
         captiont <- "Strategy for ETFs"
         model_type <- "sharpe_ohlc"
         # captiont <- paste("Contrarian Strategy for", symbol, "Using the Hampel Filter Over Prices")
         symbolv <- rutils::etfenv$symbolv
         symbol <- "VTI"
         data_env <- rutils::etfenv
         ohlc <- get(symbol, data_env)
         # Data setup
         dates <- index(ohlc)
         nrows <- NROW(ohlc)
         endp <- xts::endpoints(ohlc, on="days")
         pricev <- log(Cl(ohlc))
         retv <- rutils::diffit(pricev)
         stdev <- sd(retv[returns<0])
         # pricev <- prices[endp]
       },
       "sp500" = {
         captiont <- "Strategy for S&P500 Stocks"
         model_type <- "sharpe_ohlc"
         # Load S&P500 stocks
         # load("/Users/jerzy/Develop/data/returns100.RData")
         load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
         # Select the columns with non-zero returns
         data_env <- sp500env
         symbolv <- ls(data_env)
         symbol <- "AAPL"
         ohlc <- get(symbol, data_env)
         # Data setup
         dates <- index(ohlc)
         nrows <- NROW(ohlc)
         endp <- xts::endpoints(ohlc, on="days")
         pricev <- log(Cl(ohlc))
         retv <- rutils::diffit(pricev)
         stdev <- sd(retv[returns<0])
         # pricev <- prices[endp]
       }
)  # end switch


# captiont <- paste("Contrarian Strategy for", symbol, "Using the Returns Scaled by the Price Range")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
    column(width=12,
           h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
           actionButton("recalcb", "Recalculate the Model"))
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
    # column(width=2, sliderInput("lambda", label="Weight decay:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input model weights type
    # column(width=2, selectInput("typev", label="Portfolio weights type",
    #                             choices=c("max_sharpe", "min_var", "min_varpca", "rank"), selected="rank")),
    # Input number of eigenvalues for regularized matrix inverse
    # column(width=2, sliderInput("dimax", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
    # Input the shrinkage intensity
    # column(width=2, sliderInput("alpha", label="Shrinkage intensity",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input the percentile
    # column(width=2, sliderInput("quant", label="percentile:", min=0.01, max=0.45, value=0.1, step=0.01)),
    # Input the strategy coefficient: coeff=1 for momentum, and coeff=-1 for contrarian
    # column(width=2, selectInput("coeff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-ask spread
    column(width=2, numericInput("bidask", label="Bid-ask:", value=0.0000, step=0.0001)),
    # If coeff=1 then trending, If coeff=(-1) then contrarian
    # column(width=2, numericInput("coeff", "Trend coefficient:", value=1)),
    column(width=2, selectInput("coeff", label="Trend coefficient",
                                choices=c(1, -1), selected=(-1)))
  ),  # end fluidRow

  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), height=8, width=12)

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Recalculate the data and rerun the strategy
  datav <- shiny::reactive({
    # Get model parameters from input argument
    symbol <- isolate(input$symbol)
    model_type <- isolate(input$model_type)
    look_back <- isolate(input$look_back)
    lagg <- isolate(input$lagg)
    confirm <- isolate(input$confirm)
    # dimax <- isolate(input$dimax)
    threshold <- isolate(input$threshold)
    volumes <- isolate(input$volumes)
    # look_lag <- isolate(input$look_lag
    # lambda <- isolate(input$lambda)
    # typev <- isolate(input$typev)
    # alpha <- isolate(input$alpha)
    # quant <- isolate(input$quant)
    # coeff <- as.numeric(isolate(input$coeff))
    bidask <- isolate(input$bidask)
    coeff <- as.numeric(isolate(input$coeff))
    # Strategy is recalculated when the recalcb variable is updated
    input$recalcb

    
    # Select the data: "spybars", "etf" or "sp500"
    switch(data_type,
           "etf" = {
             # captiont <- "Rolling Portfolio Optimization Strategy for ETF Portfolio"
             # captiont <- paste("Contrarian Strategy for", symbol, "Using the Hampel Filter Over Prices")
             ohlc <- get(symbol, data_env)
             # Data setup
             dates <- index(ohlc)
             nrows <- NROW(ohlc)
             endp <- xts::endpoints(ohlc, on="days")
             pricev <- log(Cl(ohlc))
             retv <- rutils::diffit(pricev)
             stdev <- sd(retv[returns<0])
             # pricev <- prices[endp]
           },
           "sp500" = {
             # captiont <- "Rolling Portfolio Optimization Strategy for Sub-Portfolio of S&P500 Stocks"
             ohlc <- get(symbol, data_env)
             # Data setup
             dates <- index(ohlc)
             nrows <- NROW(ohlc)
             endp <- xts::endpoints(ohlc, on="days")
             pricev <- log(Cl(ohlc))
             retv <- rutils::diffit(pricev)
             stdev <- sd(retv[returns<0])
             # pricev <- prices[endp]
           },
           "tick_data" = {
             big_ticks <- raw_ticks[volume >= volumes]
             pricev <- log(big_ticks$price)
             retv <- HighFreq::diff_vec(pricev)
             stdev <- sd(retv[returns<0])
             nrows <- NROW(retv)
             # dim(retv) <- c(nrows, 1)
             endp <- 100*(1:(nrows %/% 100))
             dates <- seq.POSIXt(from=as.POSIXct("2020-01-01", origin="1970-01-01"), by="sec", length.out=nrows)
             retv <- xts::xts(retv, dates)
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
             posv <- rep(NA_integer_, nrows)
             posv[1] <- 0
             # Flip position if the scaled returns exceed threshold
             posv[re_scaled > threshold] <- 1
             posv[re_scaled < (-threshold)] <- (-1)
             # LOCF
             posv <- zoo::na.locf(posv, na.rm=FALSE)
             posv <- rutils::lagit(posv, lagg=lagg)
           },
           "sharpe_ohlc" = {  # For OHLC data
             # Scale the cumulative returns by the trailing volatility
             rangev <- HighFreq::roll_sum(retv, look_back=look_back)
             var_rolling <- sqrt(HighFreq::roll_var_ohlc(ohlc, look_back=look_back, scale=FALSE))
             look_back <- sqrt(look_back)
             re_scaled <- ifelse((var_rolling==0) | (rangev==0),
                                 0.0,
                                 rangev/var_rolling/look_back)
             # Colnames(re_scaled) <- paste0(rutils::get_name(colnames(ohlc)[1]), ".Hurst")
             re_scaled <- rutils::na_locf(re_scaled)
             # quantiles are calculated over first decile to avoid snooping
             quantiles <- quantile(re_scaled[1:(nrows %/% 10), ], c(1-threshold, threshold))
             # quantiles <- quantile(re_scaled, c(1-threshold, threshold))
             posv <- rep(NA_integer_, nrows)
             posv[1] <- 0

             if (confirm == "False") {
               posv <- ifelse(re_scaled > quantiles[2], 1, posv)
               posv <- ifelse(re_scaled < quantiles[1], -1, posv)
             } else if (confirm == "True") {
               # Flip only if two consecutive signals in same direction
               lagg_ed <- rutils::lagit(re_scaled, 1)
               posv <- ifelse((re_scaled > quantiles[2]) & (lagg_ed > quantiles[2]), 1, posv)
               posv <- ifelse((re_scaled < quantiles[1]) & (lagg_ed < quantiles[1]), -1, posv)
             }  # end if
             
             posv <- zoo::na.locf(posv)
             posv <- rutils::lagit(posv, lagg)
             
           },
           "rescaled_ohlc" = {  # For OHLC data
             # Scale the cumulative returns by the trailing volatility
             maxv <- RcppRoll::rolregmodax(Hi(ohlc), n=look_back, align="right")
             # minv <- -RcppRoll::rolregmodax(-Lo(ohlc), n=look_back, align="right")
             # me_an <- RcppRoll::rolregmodean(retv, n=look_back, align="right")
             medianv <- RcppRoll::roll_median(retv, n=look_back, align="right")
             re_scaled <- (maxv - medianv)/medianv
             re_scaled <- c(rep(1, look_back-1), re_scaled)
             # rangev <- returns
             # var_rolling <- sqrt(HighFreq::roll_var_ohlc(ohlc, look_back=look_back, scale=FALSE))
             # look_back <- sqrt(look_back)
             # re_scaled <- ifelse((var_rolling==0) | (rangev==0),
             #                     0.0,
             #                     rangev/var_rolling*look_back)
             # Colnames(re_scaled) <- paste0(rutils::get_name(colnames(ohlc)[1]), ".Hurst")
             re_scaled <- rutils::na_locf(re_scaled)
             # quantiles are calculated over first decile to avoid snooping
             quantiles <- quantile(re_scaled[1:(nrows %/% 10), ], c(1-threshold, threshold))
             # quantiles <- quantile(re_scaled, c(1-threshold, threshold))
             posv <- rep(NA_integer_, nrows)
             posv[1] <- 0
             
             if (confirm == "False") {
               posv <- ifelse(re_scaled > quantiles[2], 1, posv)
               posv <- ifelse(re_scaled < quantiles[1], -1, posv)
             } else if (confirm == "True") {
               # Flip only if two consecutive signals in same direction
               lagg_ed <- rutils::lagit(re_scaled, 1)
               posv <- ifelse((re_scaled > quantiles[2]) & (lagg_ed > quantiles[2]), 1, posv)
               posv <- ifelse((re_scaled < quantiles[1]) & (lagg_ed < quantiles[1]), -1, posv)
             }  # end if
             
             posv <- zoo::na.locf(posv)
             posv <- rutils::lagit(posv, lagg)
           },
           "sharpe_ticks" = {
             # Scale the cumulative returns by the trailing volatility
             rangev <- HighFreq::roll_sum(retv, look_back=look_back)
             var_rolling <- sqrt(HighFreq::roll_var(retv, look_back=look_back))
             look_back <- sqrt(look_back)
             re_scaled <- ifelse((var_rolling==0) | (rangev==0),
                                 0.0,
                                 rangev/var_rolling/look_back)
             re_scaled <- rutils::na_locf(re_scaled)
             # quantiles are calculated over first decile to avoid snooping
             quantiles <- quantile(re_scaled[1:(nrows %/% 10), ], c(1-threshold, threshold))
             posv <- rep(NA_integer_, nrows)
             posv[1] <- 0
             
             if (confirm == "False") {
               posv <- ifelse(re_scaled > quantiles[2], 1, posv)
               posv <- ifelse(re_scaled < quantiles[1], -1, posv)
             } else if (confirm == "True") {
               # Flip only if two consecutive signals in same direction
               lagg_ed <- rutils::lagit(re_scaled, 1)
               posv <- ifelse((re_scaled > quantiles[2]) & (lagg_ed > quantiles[2]), 1, posv)
               posv <- ifelse((re_scaled < quantiles[1]) & (lagg_ed < quantiles[1]), -1, posv)
             }  # end if

             posv <- zoo::na.locf(posv)
             posv <- rutils::lagit(posv, lagg)
           },
           "volatility" = {  # For OHLC data
             # Calculate trailing volatilities
             var_rolling <- sqrt(HighFreq::roll_var_ohlc(ohlc, look_back=look_back, scale=FALSE))
             # quantiles <- quantile(var_rolling, c(1-threshold, threshold))
             quantiles <- quantile(var_rolling[1:(nrows %/% 10), ], c(1-threshold, threshold))
             posv <- rep(NA_integer_, nrows)
             posv[1] <- 0
             posv <- ifelse(var_rolling > quantiles[2], 1, posv)
             posv <- ifelse(var_rolling < quantiles[1], -1, posv)
             posv <- zoo::na.locf(posv)
             posv <- rutils::lagit(posv, lagg)
           },
           "zscore" = {
             # Calculate trailing z-scores
             # predv <- matrix(1:nrows, nc=1)
             predv <- matrix(1:nrows, nc=1)
             zscores <- HighFreq::roll_reg(respv=retv, predictor=predv, look_back=look_back)
             zscores <- zscores[, NCOL(zscores), drop=FALSE]
             # colnames(zscores) <- "zscore"
             zscores[1:look_back] <- 0
             zscores[is.infinite(zscores)] <- 0
             zscores[is.na(zscores)] <- 0
             quantiles <- quantile(zscores[1:(nrows %/% 10), ], c(1-threshold, threshold))
             # quantiles <- quantile(zscores, c(1-threshold, threshold))
             posv <- rep(NA_integer_, nrows)
             posv[1] <- 0
             
             if (confirm == "False") {
               posv <- ifelse(zscores > quantiles[2], 1, posv)
               posv <- ifelse(zscores < quantiles[1], -1, posv)
             } else if (confirm == "True") {
               # Flip only if two consecutive signals in same direction
               lagg_ed <- rutils::lagit(zscores, 1)
               posv <- ifelse((zscores > quantiles[2]) & (lagg_ed > quantiles[2]), 1, posv)
               posv <- ifelse((zscores < quantiles[1]) & (lagg_ed < quantiles[1]), -1, posv)
             }  # end if
             
             posv <- zoo::na.locf(posv)
             posv <- rutils::lagit(posv, lagg)
           }
    )  # end switch

    # Calculate position turnover
    turn_over <- abs(rutils::diffit(posv)) / 2
    # Calculate number of trades
    # sum(turn_over)/NROW(posv)
    # Calculate strategy pnls
    pnls <- (coeff*posv*retv)

    # Calculate transaction costs
    costs <- bidask*turn_over
    pnls <- (pnls - costs)

    pnls <- stdev*pnls/sd(pnls[pnls<0])
    pnls <- cbind(pnls, retv)
    # sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x))
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 3)
    pnls <- cumsum(pnls)

    ## Coerce pnls to xts
    # pnls <- xts(pnls, dates)
    colnames(pnls) <- paste0(c("Strategy SR=", "Index SR="), sharper)
    pnls[c(1, endp), ]
  })  # end reactive code

  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    colnamev <- colnames(datav())
    dygraphs::dygraph(datav(), main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="blue") %>%
      dyLegend(width=500)
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
