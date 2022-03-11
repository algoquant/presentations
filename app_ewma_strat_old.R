##############################
# This is a shiny app for simulating a EWMA moving 
# average crossover strategy, with dygraphs plot.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

# symbol <- "CAN"
# ohlc <- data.table::fread(file="C:/Develop/predictive/data/outfile_can.csv", sep=",")

# volumes <- ohlc$volume
# plot(volumes, t="l", ylim=c(0, 1e2))
# sum(volumes[returns>0])

# openp <- log(ohlc$open_p)
# highp <- log(ohlc$high)
# lowp <- log(ohlc$low)
# closep <- log(ohlc$close_p)

symbol <- "LODE"
ohlc <- data.table::fread(file="C:/Develop/predictive/data/lode_oneminutebars.csv", sep=",")
nrows <- NROW(ohlc)
closep <- log(ohlc$close)


# symbol <- "SPY"
# ohlc <- HighFreq::SPY
# closep <- log(ohlc$SPY.Close)


returns <- rutils::diffit(closep)
stdev <- sd(returns[returns<0])
cumsumv <- cumsum(returns)
# rangev <- (highp - lowp)
# The re_scaled returns are skewed towards negative returns 
# because volumes is larger for positive returns. 
# re_scaled <- mean(volumes)*ifelse(volumes > 0, returns/volumes, 0)
# re_scaled <- ifelse(rangev > 0, returns/log(rangev), 0)
# re_scaled <- mean(volumes)*returns/volumes^0.2
# re_scaled <- (re_scaled - mean(re_scaled))
# re_scaled <- cumsum(re_scaled)
# plot(re_scaled, t="l")



dates <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out.n_rows)
# closep <- xts::xts(closep, dates)
# dygraphs::dygraph(closep)


# colnamev <- c("Prices", "Rescaled")
# datav <- cbind(closep, re_scaled)
# colnames(datav) <- colnamev
# dygraphs::dygraph(datav, main="Prices Rescaled by Volume") %>%
#   dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
#   dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
#   dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
#   dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue") %>%
#   dyLegend(width=500)
  


# plot(closep, t="l")


cap_tion <- paste("Contrarian Strategy for", symbol, "Using Two EWMAs")
# cap_tion <- paste("Contrarian Strategy for", symbol, "Using the Returns Scaled by the Price Range")

## End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel(cap_tion),

  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the re_calculate variable is updated
    column(width=12,
           h4("Click the button 'Recalculate the Model' to recalculate the Shiny App."),
           actionButton("re_calculate", "Recalculate the Model"))
  ),  # end fluidRow

  # Create single row with two slider inputs
  fluidRow(
    # Input stock symbol
    # column(width=2, selectInput("symbol", label="Symbol",
    #                             choices=symbolv, selected=symbol)),
    # Input EWMA decays
    column(width=2, sliderInput("fast_lambda", label="fast_lambda:", min=0.01, max=0.9, value=0.7, step=0.01)),
    column(width=2, sliderInput("slow_lambda", label="slow_lambda:", min=0.01, max=0.9, value=0.25, step=0.01)),
    # Input end points interval
    # column(width=2, selectInput("interval", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Look-back", min=11, max=100, value=100, step=1)),
    # Input look-back lag interval
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1)),
    
    # Input confirmation signal Boolean
    # column(width=2, selectInput("confirm", label="Confirm the signal", choices=c("True", "False"), selected="False")),
    
    # Input threshold interval
    # column(width=2, sliderInput("threshold", label="Threshold", min=0.2, max=2.0, value=0.8, step=0.1)),
    # Input minimum trade volume for filtering ticks
    # column(width=2, sliderInput("volumes", label="Big tick volume", min=50, max=1000, value=400, step=50)),
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
                                choices=c(-1, 1), selected=(1)))
  ),  # end fluidRow

  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), height=8, width=12)

)  # end fluidPage interface


## Define the server code
servfunc <- function(input, output) {

  # recalculate the data and rerun the strategy
  datav <- reactive({
    # Get model parameters from input argument
    fast_lambda <- isolate(input$fast_lambda)
    slow_lambda <- isolate(input$slow_lambda)
    # symbol <- isolate(input$symbol)
    # model_type <- isolate(input$model_type)
    look_back <- isolate(input$look_back)
    lagg <- isolate(input$lagg)
    # confirm <- isolate(input$confirm)
    # max_eigen <- isolate(input$max_eigen)
    # threshold <- isolate(input$threshold)
    # volumes <- isolate(input$volumes)
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

    # Calculate EWMA weights
    fast_weights <- exp(-fast_lambda*1:look_back)
    fast_weights <- fast_weights/sum(fast_weights)
    slow_weights <- exp(-slow_lambda*1:look_back)
    slow_weights <- slow_weights/sum(slow_weights)
    
    # Calculate EWMA prices by filtering with the weights
    fast_ewma <- .Call(stats:::C_cfilter, cumsumv, filter=fast_weights, sides=1, circular=FALSE)
    fast_ewma[1:(look_back-1)] <- fast_ewma[look_back]
    slow_ewma <- .Call(stats:::C_cfilter, cumsumv, filter=slow_weights, sides=1, circular=FALSE)
    slow_ewma[1:(look_back-1)] <- slow_ewma[look_back]
    # Determine dates when the EWMAs have crossed
    indic <- sign(fast_ewma - slow_ewma)
    
    # Older code
    # trade_dates <- (rutils::diffit(indic) != 0)
    # trade_dates <- which(trade_dates)
    # trade_dates <- trade_dates[trade_dates <.n_rows]
    # position_s <- rep(NA_integer_,.n_rows)
    # position_s[1] <- 0
    # Flip position if the scaled returns exceed threshold
    # position_s[re_scaled > threshold] <- 1
    # position_s[re_scaled < (-threshold)] <- (-1)
    # LOCF
    # position_s <- zoo::na.locf(position_s, na.rm=FALSE)
    # position_s <- rutils::lagit(position_s, lagg=lagg)
    # Calculate positions, either: -1, 0, or 1
    # position_s <- rep(NA_integer_, NROW(closep))
    # position_s[1] <- 0
    # position_s[trade_dates] <- indic[trade_dates]
    # position_s[trade_dates] <- rutils::lagit(indic)[trade_dates]
    # position_s <- na.locf(position_s)
    # position_s <- rutils::lagit(position_s)
    
    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the indic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # position_s <- ifelse(indic == indic_lag, indic, position_s)
    indic_sum <- HighFreq::roll_vec(tseries=matrix(indic), look_back=lagg)
    indic_sum[1:lagg] <- 0
    position_s <- rep(NA_integer_,.n_rows)
    position_s[1] <- 0
    position_s <- ifelse(indic_sum == lagg, 1, position_s)
    position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
    position_s <- zoo::na.locf(position_s, na.rm=FALSE)
    # position_s[1:lagg] <- 0
    # Lag the positions to trade in next period
    position_s <- rutils::lagit(position_s, lagg=1)
    
    # Calculate strategy pnls
    pnls <- (coeff*position_s*returns)

    # Calculate position turnover
    turn_over <- abs(rutils::diffit(position_s))/2
    n_trades <- sum(2*turn_over)# /.n_rows
    # Calculate number of trades
    # sum(turn_over).n_rows
    # Calculate transaction costs
    costs <- bid_offer*turn_over
    pnls <- (pnls - costs)

    pnls <- stdev*pnls/sd(pnls[pnls<0])
    pnls <- cbind(pnls, returns)
    # Coerce pnls to xts
    pnls <- xts::xts(pnls, dates)
    
    # sharp_e <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x))
    sharp_e <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    sharp_e <- round(sharp_e, 3)
    # pnls <- apply(pnls, MARGIN=2, cumsum)
    pnls <- cumsum(pnls)
    pnls <- cbind(pnls, fast_ewma, slow_ewma)
    colnames(pnls) <- c(paste0(c("Strategy SR=", "Index SR="), sharp_e), "fast", "slow")
    pnls
  })  # end reactive code

  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    colnamev <- colnames(datav())
    dygraphs::dygraph(datav(), main=cap_tion) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue") %>%
      dySeries(name=colnamev[3], axis="y2", label=colnamev[3], strokeWidth=1, col="orange") %>%
      dySeries(name=colnamev[4], axis="y2", label=colnamev[4], strokeWidth=1, col="lightpurple") %>%
      dyLegend(width=500)
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfunc)
