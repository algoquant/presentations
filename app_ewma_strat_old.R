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
# ohlc <- data.table::fread(file="/Volumes/external/Develop/Predictive/data/outfile_can.csv", sep=",")

# volumes <- ohlc$volume
# plot(volumes, t="l", ylim=c(0, 1e2))
# sum(volumes[returns>0])

# openp <- log(ohlc$open_p)
# highp <- log(ohlc$high)
# lowp <- log(ohlc$low)
# closep <- log(ohlc$close_p)

symbol <- "LODE"
ohlc <- data.table::fread(file="/Volumes/external/Develop/Predictive/data/lode_oneminutebars.csv", sep=",")
nrows <- NROW(ohlc)
closep <- log(ohlc$close)


# symbol <- "SPY"
# ohlc <- HighFreq::SPY
# closep <- log(ohlc$SPY.Close)


retv <- rutils::diffit(closep)
stdev <- sd(retv[returns<0])
cumsumv <- cumsum(retv)
# rangev <- (highp - lowp)
# The re_scaled returns are skewed towards negative returns 
# because volumes is larger for positive returns. 
# re_scaled <- mean(volumes)*ifelse(volumes > 0, returns/volumes, 0)
# re_scaled <- ifelse(rangev > 0, returns/log(rangev), 0)
# re_scaled <- mean(volumes)*returns/volumes^0.2
# re_scaled <- (re_scaled - mean(re_scaled))
# re_scaled <- cumsum(re_scaled)
# plot(re_scaled, t="l")



dates <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out=nrows)
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


captiont <- paste("Contrarian Strategy for", symbol, "Using Two EWMAs")
# captiont <- paste("Contrarian Strategy for", symbol, "Using the Returns Scaled by the Price Range")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
    column(width=12,
           h4("Click the button 'Recalculate the Model' to recalculate the Shiny App."),
           actionButton("recalcb", "Recalculate the Model"))
  ),  # end fluidRow

  # Create single row with two slider inputs
  fluidRow(
    # Input stock symbol
    # column(width=2, selectInput("symbol", label="Symbol",
    #                             choices=symbolv, selected=symbol)),
    # Input EWMA decays
    column(width=2, sliderInput("lambdaf", label="lambdaf:", min=0.01, max=0.9, value=0.7, step=0.01)),
    column(width=2, sliderInput("lambdas", label="lambdas:", min=0.01, max=0.9, value=0.25, step=0.01)),
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
servfun <- function(input, output) {

  # recalculate the data and rerun the strategy
  datav <- shiny::reactive({
    # Get model parameters from input argument
    lambdaf <- isolate(input$lambdaf)
    lambdas <- isolate(input$lambdas)
    # symbol <- isolate(input$symbol)
    # model_type <- isolate(input$model_type)
    look_back <- isolate(input$look_back)
    lagg <- isolate(input$lagg)
    # confirm <- isolate(input$confirm)
    # dimax <- isolate(input$dimax)
    # threshold <- isolate(input$threshold)
    # volumes <- isolate(input$volumes)
    # look_lag <- isolate(input$look_lag
    # lambda <- isolate(input$lambda)
    # typev <- isolate(input$typev)
    # alpha <- isolate(input$alpha)
    # quant <- isolate(input$quant)
    # coeff <- as.numeric(isolate(input$coeff))
    bid_offer <- isolate(input$bid_offer)
    coeff <- as.numeric(isolate(input$coeff))
    # Strategy is recalculated when the recalcb variable is updated
    input$recalcb

    # Calculate EWMA weights
    weightf <- exp(-lambdaf*1:look_back)
    weightf <- weightf/sum(weightf)
    weightss <- exp(-lambdas*1:look_back)
    weightss <- weightss/sum(weightss)
    
    # Calculate EWMA prices by filtering with the weights
    ewmaf <- .Call(stats:::C_cfilter, cumsumv, filter=weightf, sides=1, circular=FALSE)
    ewmaf[1:(look_back-1)] <- ewmaf[look_back]
    ewmas <- .Call(stats:::C_cfilter, cumsumv, filter=weightss, sides=1, circular=FALSE)
    ewmas[1:(look_back-1)] <- ewmas[look_back]
    # Determine dates when the EWMAs have crossed
    indic <- sign(ewmaf - ewmas)
    
    # Older code
    # trade_dates <- (rutils::diffit(indic) != 0)
    # trade_dates <- which(trade_dates)
    # trade_dates <- trade_dates[trade_dates < nrows]
    # posv <- rep(NA_integer_, nrows)
    # posv[1] <- 0
    # Flip position if the scaled returns exceed threshold
    # posv[re_scaled > threshold] <- 1
    # posv[re_scaled < (-threshold)] <- (-1)
    # LOCF
    # posv <- zoo::na.locf(posv, na.rm=FALSE)
    # posv <- rutils::lagit(posv, lagg=lagg)
    # Calculate positions, either: -1, 0, or 1
    # posv <- rep(NA_integer_, NROW(closep))
    # posv[1] <- 0
    # posv[trade_dates] <- indic[trade_dates]
    # posv[trade_dates] <- rutils::lagit(indic)[trade_dates]
    # posv <- na.locf(posv)
    # posv <- rutils::lagit(posv)
    
    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the indic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # posv <- ifelse(indic == indic_lag, indic, posv)
    indics <- HighFreq::roll_sum(tseries=matrix(indic), look_back=lagg)
    indics[1:lagg] <- 0
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    posv <- ifelse(indics == lagg, 1, posv)
    posv <- ifelse(indics == (-lagg), -1, posv)
    posv <- zoo::na.locf(posv, na.rm=FALSE)
    # posv[1:lagg] <- 0
    # Lag the positions to trade in next period
    posv <- rutils::lagit(posv, lagg=1)
    
    # Calculate strategy pnls
    pnls <- (coeff*posv*retv)

    # Calculate position turnover
    turn_over <- abs(rutils::diffit(posv))/2
    ntrades <- sum(2*turn_over)# / nrows
    # Calculate number of trades
    # sum(turn_over) nrows
    # Calculate transaction costs
    costs <- bid_offer*turn_over
    pnls <- (pnls - costs)

    pnls <- stdev*pnls/sd(pnls[pnls<0])
    pnls <- cbind(pnls, retv)
    # Coerce pnls to xts
    pnls <- xts::xts(pnls, dates)
    
    # sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x))
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 3)
    # pnls <- apply(pnls, MARGIN=2, cumsum)
    pnls <- cumsum(pnls)
    pnls <- cbind(pnls, ewmaf, ewmas)
    colnames(pnls) <- c(paste0(c("Strategy SR=", "Index SR="), sharper), "fast", "slow")
    pnls
  })  # end reactive code

  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    colnamev <- colnames(datav())
    dygraphs::dygraph(datav(), main=captiont) %>%
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
shiny::shinyApp(ui=uifun, server=servfun)
