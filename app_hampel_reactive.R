##############################
# This is a shiny app for simulating a contrarian strategy 
# using the Hampel filter over returns or prices.
# It flips the position only if the indicator persists over several 
# consecutive periods equal to lagg.
# It uses reactive code to avoid unnecessary calculations.
# This is the best performing strategy.
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
symbol <- "LODE"

captiont <- paste("Contrarian Strategy Using the Hampel Filter")

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
  
  # Create single row with inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol",
                                choices=symbolv, selected=symbol)),
    # Input data type Boolean
    column(width=2, selectInput("data_type", label="Select data for Hampel", choices=c("Returns", "Prices"), selected="Returns")),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False"))
  ),  # end fluidRow
  
  # Create single row with inputs
  fluidRow(
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Look-back", min=3, max=50, value=10, step=1)),
    # Input long look-back interval
    # column(width=2, sliderInput("long_back", label="Long lookback", min=10, max=100, value=20, step=1)),
    # Input threshold interval
    column(width=2, sliderInput("threshold", label="threshold", min=0.5, max=3.0, value=1.2, step=0.1)),
    # Input lag trade parameter
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1))
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
  
  
  # look_back <- 11
  # half_window <- look_back %/% 2

  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Load data
  closep <- shiny::reactive({
    cat("Loading data\n")
    symbol <- input$symbol
    
    # Select the data: "SPY", "LODE" or "GME"
    switch(symbol,
           "SPY" = {
             ## SPY ETF 1-minute bars
             ohlc <- HighFreq::SPY["2011"]["T09:31:00/T15:59:00"]
             # nrows <- NROW(ohlc)
             log(Cl(ohlc))
           },
           "LODE" = {
             ## LODE 1-minute bars
             ohlc <- data.table::fread(file="/Volumes/external/Develop/Predictive/data/lode_oneminutebars.csv", sep=",")
             nrows <- NROW(ohlc)
             dates <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out=nrows)
             closep <- log(ohlc$close)
             xts::xts(closep, dates)
           },
           "GME" = {
             ## GME 1-minute bars
             ohlc <- data.table::fread(file="/Volumes/external/Develop/Predictive/data/gme_oneminutebars.csv", sep=",")
             nrows <- NROW(ohlc)
             dates <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out=nrows)
             closep <- log(ohlc$close)
             xts::xts(closep, dates)
           }
    )  # end switch

  })  # end reactive
  
  # Calculate zscores if new there's look_back value
  zscores <- shiny::reactive({
    cat("Calculating zscores\n")
    look_back <- input$look_back
    data_type <- input$data_type
    
    # long_back <- input$long_back
    # closep <- closep()
    # Select the data
    if (data_type == "Returns") {
      # cat("Calculating returns\n")
      datav <- rutils::diffit(closep())
    } else if (data_type == "Prices") {
      # cat("Calculating prices\n")
      datav <- closep()
    }  # end if
    # Calculate the zscores
    # datav <- rutils::diffit(closep())
    medianv <- roll::roll_median(datav, width=look_back)
    medianv[1:look_back, ] <- 1
    # Don't divide zscores by the madv because it's redundant since zscores is divided by the mad_zscores.
    # Old code:
    # madv <- TTR::runMAD(datav, n=look_back)
    # madv[1:look_back, ] <- 1
    # zscores <- ifelse(madv != 0, (datav-medianv)/madv, 0)
    # Calculate cumulative return
    zscores <- (datav - medianv)
    # Standardize the zscores
    # Old code:
    # zscores[1:look_back, ] <- 0
    # med_zscores <- TTR::runMedian(zscores, n=long_back)
    # med_zscores[1:(long_back), ] <- 0
    # mad_zscores <- TTR::runMAD(zscores, n=long_back)
    # mad_zscores[1:(long_back), ] <- 0
    # ifelse(mad_zscores != 0, (zscores - med_zscores)/mad_zscores, 0)
    # Standardize the zscores - HighFreq::roll_scale() is fastest
    zscores <- HighFreq::roll_scale(zscores, look_back=look_back, use_median=TRUE)
    zscores[is.na(zscores)] <- 0
    zscores[is.infinite(zscores)] <- 0
    zscores
  })  # end reactive
  
  # Plot histogram of zscores
  # range(zscores)
  # zscores <- zscores[zscores > quantile(zscores, 0.05)]
  # zscores <- zscores[zscores < quantile(zscores, 0.95)]
  # x11(width=6, height=5)
  # hist(zscores, xlim=c(quantile(zscores, 0.05), quantile(zscores, 0.95)), breaks=50, main=paste("Z-scores for", "look_back =", look_back))
  
  # Calculate posv and pnls if there's new threshold value
  pnls <- shiny::reactive({
    cat("Calculating posv and pnls\n")
    threshold <- input$threshold
    lagg <- input$lagg
    retv <- rutils::diffit(closep())
    nrows <- NROW(closep())
    # Determine if the zscores have exceeded the threshold
    indic <- rep(0, nrows)
    # indic[1] <- 0
    indic <- ifelse(zscores() > threshold, -1, indic)
    indic <- ifelse(zscores() < (-threshold), 1, indic)
    # Calculate number of consecutive indicators in same direction.
    # This is designed to avoid trading on microstructure noise.
    # indic <- ifelse(indic == indic_lag, indic, indic)
    indics <- HighFreq::roll_sum(tseries=matrix(indic), look_back=lagg)
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
    
    # Calculate indicator of flipping the positions
    indic <- rutils::diffit(posv)
    # Calculate number of trades
    values$ntrades <- sum(abs(indic)>0)
    
    # Add buy/sell indicators for annotations
    longi <- (indic > 0)
    shorti <- (indic < 0)
    
    # Lag the positions to trade in next period
    posv <- rutils::lagit(posv, lagg=1)
    # Calculate strategy pnls
    pnls <- cumsum(posv*retv)
    
    # Bind together strategy pnls
    retsum <- cumsum(retv)
    pnls <- cbind(retsum, pnls)
    colnames(pnls) <- c("Index", "Strategy")
    
    pnls <- cbind(pnls, retsum[longi], retsum[shorti])
    colnames(pnls)[3:4] <- c("Buy", "Sell")
    pnls
    # list(caption=captiont, pnls=pnls)
  })  # end reactive
  

  # Calculate posv if there's new threshold value
  dyplot <- shiny::reactive({
    
    cat("Plotting pnls\n")
    # captiont <- pnls()$caption
    pnls <- pnls()
    colnamev <- colnames(pnls)
    # cat(paste("colnamev\n", colnamev, "\n"))
    # cat(paste("pnls\n", tail(pnls), "\n"))
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(rutils::diffit(pnls[, 1:2]), function(x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 3)

    # captiont <- paste("Contrarian Strategy for", input$symbol, "Using the Hampel Filter Over Prices")
    captiont <- paste("Strategy for", input$symbol, "Over ", input$data_type, "/ \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
                      "Number of trades=", values$ntrades)
    
    
    # Plot with annotations
    add_annotations <- input$add_annotations
    
    if (add_annotations == "True") {
      dygraphs::dygraph(pnls, main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red") %>%
        dySeries(name=colnamev[3], axis="y", label=colnamev[3], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=colnamev[4], axis="y", label=colnamev[4], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == "False") {
        dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
          dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
          dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
          dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
          dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red")
    }  # end if
        
  })  # end reactive

  # Render the dyplot object
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph(dyplot())
    
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
