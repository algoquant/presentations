##############################
# This version has some weird bug. 
# It calculates pnls twice, with the first time, 
# values$nrows is NULL (line 234).

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
symbolv <- c("SPY", "LODE", "GME")
symbol <- "SPY"

captiont <- paste("Contrarian Strategy for", symbol, "Using the Hampel Filter Over Prices")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
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
    column(width=2, sliderInput("short_back", label="Short lookback", min=3, max=30, value=8, step=1)),
    # Input long look-back interval
    column(width=2, sliderInput("long_back", label="Long lookback", min=10, max=100, value=20, step=1)),
    # Input lag trade parameter
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1)),
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
    # column(width=2, sliderInput("dimax", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
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
  # dimax <- isolate(input$dimax)
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
    
    # Select the data: "SPY", "LODE" or "GME"
    switch(symbol,
           "SPY" = {
             ## SPY ETF 1-minute bars
             ohlc <- HighFreq::SPY["2011"]["T09:31:00/T15:59:00"]
             values$closep <- log(quantmod::Cl(ohlc))
             values$returns <- rutils::diffit(values$closep)
             values$volumes <- Vo(ohlc)
             values$nrows <- NROW(ohlc)
             # nrows <- NROW(ohlc)
             # log(Cl(ohlc))
             cat("Loaded SPY", values$nrows, "\n")
             ohlc
           },
           "LODE" = {
             ## LODE 1-minute bars
             ohlc <- data.table::fread(file="/Volumes/external/Develop/Predictive/data/lode_oneminutebars.csv", sep=",")
             ohlc <- ohlc[, c(4, 6, 7, 5, 2)]
             colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume")
             nrows <- NROW(ohlc)
             dates <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out=nrows)
             ohlc <- xts::xts(as.matrix(ohlc), dates)
             values$nrows <- nrows
             values$closep <- log(quantmod::Cl(ohlc))
             values$returns <- rutils::diffit(values$closep)
             values$volumes <- Vo(ohlc)
             ohlc
           },
           "GME" = {
             ## GME 1-minute bars
             ohlc <- data.table::fread(file="/Volumes/external/Develop/Predictive/data/gme_oneminutebars.csv", sep=",")
             ohlc <- ohlc[, c(4, 6, 7, 5, 2)]
             colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume")
             nrows <- NROW(ohlc)
             dates <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out=nrows)
             ohlc <- xts::xts(as.matrix(ohlc), dates)
             values$nrows <- nrows
             values$closep <- log(quantmod::Cl(ohlc))
             values$returns <- rutils::diffit(values$closep)
             values$volumes <- Vo(ohlc)
             ohlc
           }
    )  # end switch
    
  })  # end reactive
  
  # Calculate log close prices
  # closep <- shiny::reactive({
  #   cat("Calculating log close prices\n")
  #   log(Cl(ohlc()))
  # })  # end reactive
  
  # Calculate log returns
  # retv <- shiny::reactive({
  #   cat("Calculating log returns\n")
  #   rutils::diffit(closep())
  # })  # end reactive
  
  # Calculate zscores if there are new short_back and long_back values
  zscores <- shiny::reactive({
    cat("Calculating zscores\n")
    short_back <- input$short_back
    long_back <- input$long_back
    
    # Calculate the rolling volume
    # volumes <- Vo(ohlc())
    # Scale the volume by the rolling volume
    volumes <- short_back*values$volumes/HighFreq::roll_sum(values$volumes, look_back=short_back)
    # retv <- rutils::diffit(closep())
    # Calculate the cumulative returns scaled by the rolling volume
    cumsumv <- cumsum(values$returns/volumes)
    cumsumv[is.na(cumsumv)] <- 0
    cumsumv[is.infinite(cumsumv)] <- 0
    cat("Calculated cumsumv\n")
    
    # Calculate the rolling median of the cumulative returns
    medianv <- roll::roll_median(cumsumv, width=short_back)
    medianv[1:short_back, ] <- 1
    cat("Calculated medianv\n")
    # Don't divide zscores by the madv because it's redundant since zscores is divided by the mad_zscores.
    # Old code:
    # madv <- TTR::runMAD(retv, n=short_back)
    # madv[1:short_back, ] <- 1
    # zscores <- ifelse(madv != 0, (closep-medianv)/madv, 0)
    # Calculate the zscores as the rolling cumulative returns
    zscores <- (cumsumv - medianv)
    # Standardize the zscores
    # Old code:
    # zscores[1:short_back, ] <- 0
    # med_zscores <- TTR::runMedian(zscores, n=long_back)
    # med_zscores[1:(long_back), ] <- 0
    # mad_zscores <- TTR::runMAD(zscores, n=long_back)
    # mad_zscores[1:(long_back), ] <- 0
    # ifelse(mad_zscores != 0, (zscores - med_zscores)/mad_zscores, 0)
    # Standardize the zscores - HighFreq::roll_scale() is fastest
    zscores <- HighFreq::roll_scale(zscores, look_back=long_back, use_median=TRUE)
    zscores[is.na(zscores)] <- 0
    zscores[is.infinite(zscores)] <- 0
    cat("Calculated zscores\n")
    zscores
  })  # end reactive
  
  # Plot histogram of zscores
  # range(zscores)
  # zscores <- zscores[zscores > quantile(zscores, 0.05)]
  # zscores <- zscores[zscores < quantile(zscores, 0.95)]
  # x11(width=6, height=5)
  # hist(zscores, xlim=c(quantile(zscores, 0.05), quantile(zscores, 0.95)), breaks=50, main=paste("Z-scores for", "short_back =", short_back))
  
  # Calculate posv and pnls if there's new threshold value
  pnls <- shiny::reactive({
    cat("Calculating posv and pnls\n")
    # short_back <- input$short_back
    # long_back <- input$long_back
    threshold <- input$threshold
    lagg <- input$lagg
    # retv <- rutils::diffit(closep())
    # nrows <- NROW(values$closep)
    # Determine if the zscores have exceeded the threshold
    # indic <- rep(0, values$nrows)
    cat("1 nrows =", values$nrows, "\n")
    cat("is.null(values$nrows) =", is.null(values$nrows), "\n")
    # nrows <- values$nrows
    indic <- rep(0, NROW(ohlc()))
    
    # indic[1] <- 0
    indic <- ifelse(zscores() > threshold, -1, indic)
    indic <- ifelse(zscores() < (-threshold), 1, indic)
    cat("Calculated zscores\n")
    # Calculate number of consecutive indicators in same direction.
    # This is designed to avoid trading on microstructure noise.
    # indic <- ifelse(indic == indic_lag, indic, indic)
    indics <- HighFreq::roll_sum(tseries=matrix(indic), look_back=lagg)
    indics[1:lagg] <- 0
    cat("Calculated indics\n")
    
    # Calculate posv and pnls from indics.
    # posv <- rep(NA_integer_, nrows)
    # posv[1] <- 0
    # threshold <- 3*mad(zscores)
    # Flip position only if the indics is at least equal to lagg.
    # Otherwise keep previous position.
    posv <- rep(NA_integer_, values$nrows)
    cat("2 nrows =", values$nrows, "\n")
    posv[1] <- 0
    posv <- ifelse(indics >= lagg, 1, posv)
    posv <- ifelse(indics <= (-lagg), -1, posv)
    # posv <- ifelse(zscores > threshold, -1, posv)
    # posv <- ifelse(zscores < (-threshold), 1, posv)
    posv <- zoo::na.locf(posv, na.rm=FALSE)
    posv <- rutils::lagit(posv, lagg=1)
    cat("Calculated positions\n")
    
    pnls <- cumsum(posv*values$returns)
    cumsumv <- cumsum(values$returns)
    pnls <- cbind(pnls, cumsumv)
    colnames(pnls) <- c("Strategy", "Index")
    cat("Calculated pnls\n")
    
    # Add buy/sell indicators
    indic <- rutils::diffit(posv)
    longi <- (indic > 0)
    shorti <- (indic < 0)
    
    pnls <- cbind(pnls, cumsumv[longi], cumsumv[shorti])
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
    
    # Number of trades
    # ntrades <- sum(abs(rutils::diffit(posv)))# / nrows
    # captiont <- paste("Number of trades =", ntrades)
    captiont <- paste("Number of trades")
    
    if (add_annotations == "True") {
      # Create a dygraph object with annotations (no plot is created)
      dygraphs::dygraph(pnls, main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="red") %>%
        # Add second y-axis
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[3], axis="y2", drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=colnamev[4], axis="y2", drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == "False") {
      # Create a dygraph object without annotations (no plot is created)
      dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        # Add second y-axis
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="red") %>%
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="blue")
    }  # end if
    
  })  # end reactive
  
  # Render (plot) the dygraph object and return it to the output argument
  output$dyplot <- dygraphs::renderDygraph(dyplot())
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
