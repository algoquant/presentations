##############################
# This is a shiny app for simulating high frequency trading
# strategies, with a dygraphs plot.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Set up TAQ data

# These files are on the external hard drive
# filen <- "aapl_tick_trades2020_0720_biglots"
# filen <- "aapl_tick_trades2020_0316_biglots"
# filen <- "aapl_tick_trades2020_02102020_0510_biglots"
# filen <- "vxx_tick_trades2020_0316_biglots"
# filen <- "vxx_tick_trades2020_02202020_0310_biglots"
# filen <- "vxx_tick_trades2020_02102020_0510_biglots"
# filen <- "xlk_tick_trades2020_0316_biglots"
# filen <- "xlk_tick_trades2020_biglots"
# filen <- "goog_tick_trades2020_02102020_0510_biglots"
# filen <- "goog_tick_trades2020_02102020_0510_biglots"

# taq <- data.table::fread(file=paste0("/Users/jerzy/Develop/data/", filen, ".csv"), stringsAsFactors=FALSE)
# Create date-time index
# indeks <- paste(taq$DATE, taq$TIME_M)
# indeks <- strptime(indeks, "%Y%m%d %H:%M:%OS")
# indeks <- as.POSIXct(indeks)
# load(file=paste0("/Users/jerzy/Develop/data/TwoSig/", filen, ".RData"))
load(file="/Users/jerzy/Develop/lecture_slides/data/xlk_tick_trades_20200316.RData")

# symboln <- taq$SYM_ROOT[1]
symboln <- rutils::get_name(colnames(taq)[1])

# taq <- taq[3e4: nrows-3e4), c("SIZE", "PRICE")]
# taq <- taq[taq$SIZE > 200]
# closep <- matrix(taq$PRICE, nc=1)
# volumev <- matrix(taq$SIZE, nc=1)
# retv <- rutils::diffit(closep)


## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(paste(symboln, "High Frequency Strategy")),
  
  # create single row with four slider inputs
  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
    # column(width=12, 
    #        h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
    #        actionButton("recalcb", "Recalculate the Model")),
    # Input stock symbol
    # column(width=2, selectInput("symboln", label="Symbol",
    #                             choices=symboln, selected=symboln)),
    # Input model type
    column(width=2, selectInput("modelt", label="model",
                                choices=c("VWAP", "Hampel", "ZScore"), selected="ZScore")),
    # Input look-back interval
    column(width=2, sliderInput("lookb", label="Lookback interval",
                                min=1, max=300, value=100, step=1)),
    # Input lag trade parameter
    column(width=2, sliderInput("lagg", label="Confirmation signals", min=1, max=5, value=2, step=1)),
    # Input lots parameter to remove odd lots - larger lotsize removes more ticks
    column(width=2, sliderInput("lotsize", label="Lots size", min=100, max=1e3, value=200, step=50)),
    # Input threshold parameter to control position flipping
    column(width=2, sliderInput("threshv", label="Threshold", min=0.0, max=4.0, value=0.0, step=0.1)),
    # Input trending or reverting (contrarian) strategy
    column(width=2, selectInput("coeff", label="Trend (1) Revert (-1)",
                                choices=c(1, -1), selected=(-1)))
  ),  # end fluidRow
  
  # create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  # mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface


## Define the server code
servfun <- shiny::shinyServer(function(input, output) {

  # Recalculate the data and rerun the model
  pnls <- shiny::reactive({
    # Get model parameters from input argument
    modelt <- input$modelt
    lookb <- input$lookb
    lagg <- input$lagg
    threshv <- input$threshv
    lotsize <- input$lotsize
    coeff <- as.numeric(input$coeff)
    # Model is recalculated when the recalcb variable is updated
    # input$recalcb
    
    # Prepare data
    # taq <- taq[taq$SIZE > lotsize]
    volumv <- quantmod::Vo(taq)
    taq <- taq[volumv > lotsize, ]
    closep <- quantmod::Cl(taq)
    # closep <- (max(closep) + 10 - closep)
    volumev <- quantmod::Vo(taq)
    retv <- rutils::diffit(closep)
    nrows <- NROW(taq)


    if (modelt == "VWAP") {
      # VWAP model
      vwap <- HighFreq::roll_sum(closep, lookb=lookb, weightv=volumv)
      vwap[is.na(vwap)] <- 0
      # Calculate VWAP indicator
      longi <- ((closep - vwap) > threshv)
      shorti <- ((closep - vwap) < (-threshv))
      # End VWAP model
    } else if (modelt == "Hampel") {
      # Hampel model
      medianv <- TTR::runMedian(retv, n=lookb)
      medianv[1:lookb] <- 1
      madv <- TTR::runMAD(retv, n=lookb)
      madv[1:lookb] <- 1
      zscores <- ifelse(madv!=0, (retv-medianv)/madv, 0)
      zscores[1:lookb] <- 0
      mad_zscores <- TTR::runMAD(zscores, n=lookb)
      mad_zscores[1:lookb] <- 0
      longi <- (zscores > threshv*mad_zscores)
      shorti <- (zscores < (-threshv*mad_zscores))
      # End Hampel model
    } else if (modelt == "ZScore") {
      # Z-Score regression model
      predm <- matrix(1:nrows, nc=1)
      zscores <- HighFreq::roll_zscores(respv=closep, predm=predm, lookb=lookb)
      colnames(zscores) <- "zscores"
      zscores[1:lookb] <- 0
      zscores[is.infinite(zscores)] <- 0
      longi <- (zscores > threshv)
      shorti <- (zscores < (-threshv))
      # End z-score regression model
    }  # end if
    
    ## Calculate the positions
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    indic <- HighFreq::roll_count(longi)
    posv <- ifelse(indic >= lagg, 1, posv)
    indic <- HighFreq::roll_count(shorti)
    posv <- ifelse(indic >= lagg, -1, posv)
    # Lag the positions to trade in next period
    posv <- zoo::na.locf(posv, na.rm=FALSE)
    posv <- rutils::lagit(posv, lagg=1)
    pnls <- cumsum(coeff*retv*posv)
    pnls <- cbind(closep, pnls)
    # indeks <- seq.POSIXt(Sys.time()-NROW(pnls)+1, Sys.time(), by=1)
    # pnls <- xts::xts(coredata(pnls), indeks)
    colnames(pnls) <- c(symboln, "strategy")
    # pnls[rutils::calc_endpoints(xtes=pnls, interval="hours")]
    pnls
  })  # end reactive code
  
  # return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    # plot(coredata(pnls()), t="l")
    pnls <- pnls()
    colnamev <- colnames(pnls)
    dygraphs::dygraph(pnls, main=paste(colnamev[1], "Strategy")) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
      # dySeries(name=colnamev[3], axis="y", strokeWidth=2, col="green") %>%
      dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
