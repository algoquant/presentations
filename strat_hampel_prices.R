##############################
# This is a shiny app for simulating a contrarian strategy 
# using the Hampel filter over prices and volumes.
# It flips the position only if the indicator persists over several 
# consecutive periods equal to lagg.
# This is the best performing version.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Model and data setup

## VTI ETF daily bars
# symbol <- "VTI"
# closep <- log(rutils::etfenv$VTI$VTI.Close)
# volumes <- rutils::etfenv$VTI$VTI.Volume

## SPY ETF 1-minute bars - works really well !!!
symbol <- "SPY"
load(file="/Users/jerzy/Develop/data/spy_ticks_20220617.RData")
# closep <- spyticks$price
# volumes <- spyticks$size

# ohlc <- HighFreq::SPY["2011"]["T09:31:00/T15:59:00"]
# closep <- log(ohlc$SPY.Close)
# volumes <- ohlc$SPY.Volume

maxsize <- quantile(spyticks$size, 0.99)
maxsizem <- median(spyticks$size)

# nrows <- NROW(closep)

## Load 1-minute bars
# captiont <- "Strategy for 1-minute LODE Bars"
# symbol <- "LODE"
# ohlc <- data.table::fread(file="/Volumes/external/Develop/Predictive/data/lode_oneminutebars.csv", sep=",")
# nrows <- NROW(ohlc)
# dates <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out=nrows)
# closep <- log(ohlc$close)
# closep <- xts::xts(closep, dates)
# rets <- rutils::diffit(closep)


## Load QM futures 5-second bars
# symbol <- "ES"  # S&P500 Emini futures
# symbol <- "QM"  # oil
# load(file=paste0("/Users/jerzy/Develop/data/ib_data/", symbol, "_ohlc.RData"))
# closep <- log(Cl(ohlc))
# Or random prices
# closep <- xts(cumsum(rnorm(nrows)), index(ohlc))

## Load combined futures data
# com_bo <- HighFreq::SPY
# load(file="/Users/jerzy/Develop/data/combined.RData")
# symbol <- "UX1"
# symbolv <- unique(rutils::get_name(colnames(com_bo)))
# closep <- log(na.omit(com_bo[, "UX1.Close"]))
# TU1: lookb=14, threshold=2.0, lagg=1
# TU1: lookb=30, threshold=9.2, lagg=1


## Load VX futures daily bars
# symbol <- "VX"
# load(file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")
# closep <- log(Cl(vix_env$chain_ed))

# rets <- rutils::diffit(closep)

captiont <- paste("Hampel Strategy for", symbol, "Over Prices")

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
    # Input end points interval
    # column(width=2, selectInput("interval", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input minimum trade size
    column(width=2, sliderInput("minsize", label="Minimum trade size:",
                                min=1, max=maxsizem, value=(maxsizem %/% 2), step=1)),
    # Input maximum trade size
    column(width=2, sliderInput("maxsize", label="Maximum trade size:",
                                min=100, max=maxsize, value=(maxsize %/% 2), step=1)),
    # Input look-back interval
    column(width=2, sliderInput("lookb", label="Lookback", min=3, max=100, value=60, step=1)),
    # Input lag trade parameter
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1)),
    # Input threshold interval
    column(width=2, sliderInput("threshold", label="threshold", min=0.1, max=1.0, value=0.1, step=0.1))
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
    # column(width=2, numericInput("bidask", label="bid-ask:", value=0.001, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="550px")
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # Recalculate the prices
  dataticks <- shiny::reactive({
    cat("Calculating prices \n")
    # Get model parameters from input argument
    minsize <- input$minsize
    maxsize <- input$maxsize
    
    # Remove prices with small size
    dataticks <- spyticks[(spyticks$size >= minsize), ]
    # Remove prices with small size
    dataticks <- dataticks[(dataticks$size <= maxsize), ]
    
    dataticks
    
  })  # end reactive code
  
  
  # Recalculate the price z-scores
  zscores <- shiny::reactive({
    cat("Calculating z-scores \n")
    # Get model parameters from input argument
    lookb <- input$lookb
    # lookb <- isolate(input$lookb)
    # lagg <- isolate(input$lagg)
    # coeff <- as.numeric(isolate(input$coeff))
    # bidask <- isolate(input$bidask)
    # Model is recalculated when the recalcb variable is updated
    # input$recalcb
    closep <- dataticks()$price

    # Calculate the zscores
    medianv <- roll::roll_median(closep, width=lookb)
    # medianv <- TTR::runMedian(closep, n=lookb)
    medianv[1:lookb, ] <- 1
    # madv <- TTR::runMAD(rets, n=lookb)
    # madv[1:lookb, ] <- 1
    # zscores <- ifelse(madv != 0, (closep-medianv)/madv, 0)
    # Don't divide zscores by the madv because it's redundant since zscores is divided by the madv.
    zscores <- (closep-medianv)
    # zscores[1:lookb, ] <- 0
    madv <- HighFreq::roll_var(matrix(zscores), lookb=10*lookb, method="nonparametric")
    # madv <- TTR::runMAD(zscores, n=10*lookb)
    madv[1:(10*lookb), ] <- 0
    zscores <- ifelse(madv != 0, zscores/madv, 0)

    # Plot histogram of zscores
    # range(zscores)
    # zscores <- zscores[zscores > quantile(zscores, 0.05)]
    # zscores <- zscores[zscores < quantile(zscores, 0.95)]
    # x11(width=6, height=5)
    # hist(zscores, breaks=500, xlim=c(quantile(zscores, 0.01), quantile(zscores, 0.99)), main=paste("Z-scores for", "lookb =", lookb))

    zscores
    
  })  # end reactive code
  
  
  # Recalculate the volume z-scores
  # zscorev <- shiny::reactive({
  #   cat("Calculating volume z-scores \n")
  #   # Get model parameters from input argument
  #   lookb <- input$lookb
  #   # coeff <- as.numeric(isolate(input$coeff))
  #   # bidask <- isolate(input$bidask)
  #   # Model is recalculated when the recalcb variable is updated
  #   # input$recalcb
  #   volumes <- dataticks()$size
  #   
  #   # Calculate the zscores
  #   medianv <- roll::roll_median(volumes, width=lookb)
  #   # medianv <- TTR::runMedian(volumes, n=lookb)
  #   medianv[1:lookb, ] <- 1
  #   # madv <- TTR::runMAD(rets, n=lookb)
  #   # madv[1:lookb, ] <- 1
  #   # zscores <- ifelse(madv != 0, (volumes-medianv)/madv, 0)
  #   # Don't divide zscores by the madv because it's redundant since zscores is divided by the madv.
  #   zscores <- (volumes-medianv)
  #   # zscores[1:lookb, ] <- 0
  #   madv <- sqrt(HighFreq::roll_var(matrix(zscores), lookb=10*lookb, method="nonparametric"))
  #   # madv <- TTR::runMAD(zscores, n=10*lookb)
  #   madv[1:(10*lookb), ] <- 0
  #   zscores <- ifelse(madv != 0, zscores/madv, 0)
  #   
  #   # Plot histogram of zscores
  #   # range(zscores)
  #   # zscores <- zscores[zscores > quantile(zscores, 0.05)]
  #   # zscores <- zscores[zscores < quantile(zscores, 0.95)]
  #   # x11(width=6, height=5)
  #   # hist(zscores, breaks=5000, xlim=c(quantile(zscores, 0.01), quantile(zscores, 0.95)), main=paste("Z-scores for", "lookb =", lookb))
  #   
  #   zscores
  #   
  # })  # end reactive code
  
  
  # Rerun the model and calculate the pnls
  datav <- shiny::reactive({
    cat("Calculating pnls \n")
    # Get model parameters from input argument
    threshold <- input$threshold
    lagg <- input$lagg
    # look_lag <- isolate(input$look_lag
    # lambda <- isolate(input$lambda)
    # typev <- isolate(input$typev)
    # alpha <- isolate(input$alpha)
    # quant <- isolate(input$quant)
    # coeff <- as.numeric(isolate(input$coeff))
    # bidask <- isolate(input$bidask)
    # Model is recalculated when the recalcb variable is updated
    # input$recalcb
    
    zscores <- zscores()
    # zscorev <- zscorev()
    closep <- dataticks()$price
    nrows <- NROW(closep)
    rets <- rutils::diffit(closep)
    
    # lookb <- 11
    # half_window <- lookb %/% 2
    
    # Determine if the zscores have exceeded the threshold
    indic <- rep(0, nrows)
    # indic[1] <- 0
    # indic <- ifelse((zscores > threshold) & (zscorev > threshold), -1, indic)
    # indic <- ifelse((zscores < (-threshold)) & (zscorev > threshold), 1, indic)
    indic <- ifelse((zscores > threshold), -1, indic)
    indic <- ifelse((zscores < (-threshold)), 1, indic)
    # Calculate number of consecutive indicators in same direction.
    # This is designed to avoid trading on microstructure noise.
    # indic <- ifelse(indic == indic_lag, indic, indic)
    indics <- HighFreq::roll_sum(tseries=matrix(indic), lookb=lagg)
    indics[1:lagg] <- 0
    
    # Calculate posv and pnls from indics.
    # posv <- rep(NA_integer_, nrows)
    # posv[1] <- 0
    # threshold <- 3*mad(zscores)
    # Flip position only if the indics is at least equal to lagg.
    # Otherwise keep previous position.
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    posv <- ifelse(indics >= lagg, 1, posv)
    posv <- ifelse(indics <= (-lagg), -1, posv)
    # posv <- ifelse(zscores > threshold, -1, posv)
    # posv <- ifelse(zscores < (-threshold), 1, posv)
    posv <- zoo::na.locf(posv, na.rm=FALSE)
    posv <- rutils::lagit(posv, lagg=1)
    
    # Number of trades
    ntrades <- sum(abs(rutils::diffit(posv)))# / nrows
    captiont <- paste("Number of trades =", ntrades)
    
    pnls <- cumsum(posv*rets)
    pnls <- cbind(pnls, cumsum(rets))
    colnames(pnls) <- c("Strategy", "Index")
    # pnls[rutils::calc_endpoints(pnls, interval="minutes")]
    # pnls[rutils::calc_endpoints(pnls, interval="hours")]
    list(caption=captiont, pnls=pnls)
  })  # end reactive code
  
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    captiont <- datav()$caption
    pnls <- datav()$pnls
    colnamev <- colnames(pnls)
    dygraphs::dygraph(pnls, main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="blue")
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
