##############################
# This is a shiny app for simulating a contrarian strategy using 
# the Hampel filter over prices scaled by the trading volumes.
# The model flips the position only if the indicator persists over 
# several consecutive periods equal to lagg.
# It uses reactive code to avoid unnecessary calculations.
# This is the best performing univariate strategy.
# 
# Just press the "Run App" button on upper right of this panel.
##############################


## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Model and data setup

# symbolv <- names(data_env)
symbolv <- c("SPY", "VXX", "LODE", "GME")
symbol <- "VXX"

captiont <- paste("Contrarian Strategy Using the Hampel Filter Over Prices")
# captiont <- paste("Contrarian Strategy for", symbol, "Using the Hampel Filter Over Prices")

## End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel(captiont),
  
  # fluidRow(
  # The Shiny App is recalculated when the actionButton is clicked and the add_annotations variable is updated
  #   column(width=12,
  #          h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
  #          actionButton("add_annotations", "Recalculate the Model"))
  # ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input end points interval
    # column(width=2, selectInput("interval", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol",
                                choices=symbolv, selected=symbol)),
    # Input short look-back interval
    column(width=2, sliderInput("short_back", label="Short lookback", min=3, max=40, value=15, step=1)),
    # Input long look-back interval
    # column(width=2, sliderInput("long_back", label="Long lookback", min=10, max=200, value=100, step=2)),
    # Input lag trade parameter
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=1, step=1)),
    # Input threshold level
    column(width=2, sliderInput("threshold", label="threshold", min=0.5, max=3.0, value=2.0, step=0.1)),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False"))
    # Input the weight decay parameter
    # column(width=2, sliderInput("lambda", label="Weight decay:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input model weights type
    # column(width=2, selectInput("typev", label="Portfolio weights type",
    #                             choices=c("max_sharpe", "min_var", "min_varpca", "rank"), selected="rank")),
    # Input number of eigenvalues for regularized matrix inverse
    # column(width=2, sliderInput("eigen_max", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
    # Input the shrinkage intensity
    # column(width=2, sliderInput("alpha", label="Shrinkage intensity",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input the percentile
    # column(width=2, sliderInput("quant", label="percentile:", min=0.01, max=0.45, value=0.1, step=0.01)),
    # Input the strategy coefficient: coeff=1 for momentum, and coeff=-1 for contrarian
    # column(width=2, selectInput("coeff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    # column(width=2, numericInput("bid_offer", label="bid-offer:", value=0.001, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # Recalculate the data and rerun the model
  # datav <- shiny::reactive({
  # Get model parameters from input argument
  # eigen_max <- isolate(input$eigen_max)
  # look_lag <- isolate(input$look_lag
  # lambda <- isolate(input$lambda)
  # typev <- isolate(input$typev)
  # alpha <- isolate(input$alpha)
  # quant <- isolate(input$quant)
  # coeff <- as.numeric(isolate(input$coeff))
  # bid_offer <- isolate(input$bid_offer)
  # Model is recalculated when the add_annotations variable is updated
  # input$add_annotations
  
  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Load data
  ohlc <- shiny::reactive({
    cat("Loading data\n")
    symbol <- input$symbol
    
    # Select the data: "SPY", "VXX", "LODE" or "GME"
    switch(symbol,
           "SPY" = {
             ## SPY ETF 1-minute bars
             # ohlc <- HighFreq::SPY["2012"]["T09:31:00/T15:59:00"]
             load(file="/Users/jerzy/Develop/data/polygon/spy_minutes.RData")
             # nrows <- NROW(ohlc)
             # log(Cl(ohlc))
             ohlc["T09:00:00/T16:30:00"]
           },
           "VXX" = {
             ## SPY ETF 1-minute bars
             # ohlc <- HighFreq::SPY["2012"]["T09:31:00/T15:59:00"]
             load(file="/Users/jerzy/Develop/data/polygon/vxx_minutes.RData")
             # nrows <- NROW(ohlc)
             # log(Cl(ohlc))
             ohlc["T09:00:00/T16:30:00"]
           },
           "LODE" = {
             ## LODE 1-minute bars
             ohlc <- data.table::fread(file="/Volumes/external/Develop/Predictive/data/lode_oneminutebars.csv", sep=",")
             ohlc <- ohlc[, c(4, 6, 7, 5, 2)]
             colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume")
             nrows <- NROW(ohlc)
             dates <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out=nrows)
             xts::xts(as.matrix(ohlc), dates)
           },
           "GME" = {
             ## GME 1-minute bars
             ohlc <- data.table::fread(file="/Volumes/external/Develop/Predictive/data/gme_oneminutebars.csv", sep=",")
             ohlc <- ohlc[, c(4, 6, 7, 5, 2)]
             colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume")
             nrows <- NROW(ohlc)
             dates <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out=nrows)
             xts::xts(as.matrix(ohlc), dates)
           }
    )  # end switch
    
  })  # end reactive
  
  # Calculate log close prices
  # closep <- shiny::reactive({
  #   cat("Calculating log close prices\n")
  #   log(Cl(ohlc()))
  # })  # end reactive
  
  # Calculate log returns
  returns <- shiny::reactive({
    cat("Calculating log returns\n")
    rutils::diffit(log(Cl(ohlc())))
  })  # end reactive
  
  # Calculate zscores if there are new short_back and long_back values
  zscores <- shiny::reactive({
    cat("Calculating zscores\n")
    short_back <- input$short_back
    # long_back <- input$long_back
    
    # Calculate the rolling volume
    volumes <- Vo(ohlc())
    # Scale the volume by the rolling volume
    volumes <- short_back*volumes/HighFreq::roll_sum(volumes, look_back=short_back)
    # returns <- rutils::diffit(closep())
    # Calculate the cumulative returns scaled by the rolling volume
    volumes <- rutils::lagit(volumes, pad_zeros=FALSE)
    returns <- ifelse(volumes > 0, returns()/volumes, 0)
    # returns[is.na(returns) | is.infinite(returns)] <- 0
    cum_scaled <- cumsum(returns)

    # Calculate the rolling median of the cumulative returns
    mi_n <- roll::roll_min(cum_scaled, width=short_back)
    ma_x <- roll::roll_max(cum_scaled, width=short_back)
    mi_n[1:short_back, ] <- 0
    ma_x[1:short_back, ] <- 1
    mi_n <- rutils::lagit(mi_n, pad_zeros=FALSE)
    ma_x <- rutils::lagit(ma_x, pad_zeros=FALSE)
    # Don't divide zscores by the madv because it's redundant since zscores is divided by the mad_zscores.
    # Old code:
    # madv <- TTR::runMAD(returns, n=short_back)
    # madv[1:short_back, ] <- 1
    # zscores <- ifelse(madv != 0, (closep-mi_n)/madv, 0)
    # Calculate the zscores as the rolling cumulative returns
    zscores <- ifelse(ma_x > mi_n, (2*cum_scaled - mi_n - ma_x)/(ma_x - mi_n), 0)
    # zscores[is.na(zscores) | is.infinite(zscores)] <- 0
    # Standardize the zscores
    # Old code:
    # zscores[1:short_back, ] <- 0
    # med_zscores <- TTR::runMedian(zscores, n=long_back)
    # med_zscores[1:(long_back), ] <- 0
    # mad_zscores <- TTR::runMAD(zscores, n=long_back)
    # mad_zscores[1:(long_back), ] <- 0
    # ifelse(mad_zscores != 0, (zscores - med_zscores)/mad_zscores, 0)
    # Standardize the zscores - HighFreq::roll_scale() is fastest
    # zscores <- HighFreq::roll_scale(zscores, look_back=long_back, use_median=TRUE)
    # zscores[is.na(zscores) | is.infinite(zscores)] <- 0
    zscores
  })  # end reactive
  
  # Plot histogram of zscores
  # range(zscores)
  # zscores <- zscores[zscores > quantile(zscores, 0.05)]
  # zscores <- zscores[zscores < quantile(zscores, 0.95)]
  # x11(width=6, height=5)
  # hist(zscores, xlim=c(quantile(zscores, 0.05), quantile(zscores, 0.95)), breaks=50, main=paste("Z-scores for", "short_back =", short_back))
  
  # Calculate posit and pnls if there's new threshold value
  pnls <- shiny::reactive({
    cat("Calculating posit and pnls\n")
    threshold <- input$threshold
    lagg <- input$lagg
    # returns <- rutils::diffit(closep())
    nrows <- NROW(returns())
    # Determine if the zscores have exceeded the threshold
    indic <- rep(0, nrows)
    # indic[1] <- 0
    indic <- ifelse(zscores() > threshold, -1, indic)
    indic <- ifelse(zscores() < (-threshold), 1, indic)
    # Calculate number of consecutive indicators in same direction.
    # This is predictored to avoid trading on microstructure noise.
    # indic <- ifelse(indic == indic_lag, indic, indic)
    indic_sum <- HighFreq::roll_vec(tseries=matrix(indic), look_back=lagg)
    indic_sum[1:lagg] <- 0
    
    # Calculate posit and pnls from indic_sum.
    # posit <- rep(NA_integer_, nrows)
    # posit[1] <- 0
    # threshold <- 3*mad(zscores)
    # Flip position only if the indic_sum is at least equal to lagg.
    # Otherwise keep previous position.
    posit <- rep(NA_integer_, nrows)
    posit[1] <- 0
    posit <- ifelse(indic_sum >= lagg, 1, posit)
    posit <- ifelse(indic_sum <= (-lagg), -1, posit)
    # posit <- ifelse(zscores > threshold, -1, posit)
    # posit <- ifelse(zscores < (-threshold), 1, posit)
    posit <- zoo::na.locf(posit, na.rm=FALSE)
    posit <- rutils::lagit(posit, lagg=1)
    
    # Number of trades
    values$ntrades <- sum(abs(rutils::diffit(posit)))# / nrows
    
    pnls <- cbind(posit*returns(), returns())
    
    # Sharpe
    sharper <- sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sqrt(252)*sharper, 3)

    # pnls <- cumsum(posit*returns())
    # cum_scaled <- cumsum(returns())
    pnls <- cumsum(pnls)
    cum_scaled <- pnls[, 2]
    colnames(pnls) <- c("Strategy", "Index")
    
    # Add buy/sell indicators
    indic <- rutils::diffit(posit)
    indic_buy <- (indic > 0)
    indic_sell <- (indic < 0)
    
    pnls <- cbind(pnls, cum_scaled[indic_buy], cum_scaled[indic_sell])
    colnames(pnls)[3:4] <- c("Buy", "Sell")
    pnls
    # list(caption=captiont, pnls=pnls)
  })  # end reactive
  
  
  # Plot dygraph of pnls if the add_annotations variable is updated
  dyplot <- shiny::reactive({
    cat("Plotting pnls\n")
    add_annotations <- input$add_annotations
    # captiont <- pnls()$caption
    pnls <- pnls()
    colnamev <- colnames(pnls)
    # cat(paste("colnamev\n", colnamev, "\n"))
    # cat(paste("pnls\n", tail(pnls), "\n"))
    
    captiont <- paste(paste("Number of trades =", values$ntrades), ",",
                      paste(paste(colnamev[1:2], "Sharpe =", values$sharper), collapse=", "))

    if (add_annotations == "True") {
      # Create a dygraph object with annotations (no plot is created)
      dygraphs::dygraph(pnls, main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
        # Add second y-axis
        dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[3], axis="y2", label=colnamev[3], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=colnamev[4], axis="y2", label=colnamev[4], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == "False") {
      # Create a dygraph object without annotations (no plot is created)
      dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        # Add second y-axis
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
        dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue")
    }  # end if
    
  })  # end reactive
  
  # Render (plot) the dygraph object and return it to the output argument
  output$dyplot <- dygraphs::renderDygraph(dyplot())
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
