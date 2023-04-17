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

# file_name <- "aapl_tick_trades2020_0720_biglots"
# file_name <- "aapl_tick_trades2020_0316_biglots"
# file_name <- "aapl_tick_trades2020_02102020_0510_biglots"
# file_name <- "vxx_tick_trades2020_0316_biglots"
# file_name <- "vxx_tick_trades2020_02202020_0310_biglots"
# file_name <- "vxx_tick_trades2020_02102020_0510_biglots"
# file_name <- "xlk_tick_trades2020_0316_biglots"
# file_name <- "xlk_tick_trades2020_biglots"
# file_name <- "goog_tick_trades2020_02102020_0510_biglots"
file_name <- "goog_tick_trades2020_02102020_0510_biglots"

# taq <- data.table::fread(file=paste0("/Users/jerzy/Develop/data/", file_name, ".csv"), stringsAsFactors=FALSE)
# Create date-time index
# indeks <- paste(taq$DATE, taq$TIME_M)
# indeks <- strptime(indeks, "%Y%m%d %H:%M:%OS")
# indeks <- as.POSIXct(indeks)
load(file=paste0("/Users/jerzy/Develop/data/TwoSig/", file_name, ".RData"))

# symbol <- taq$SYM_ROOT[1]
symbol <- rutils::get_name(colnames(taq)[1])

# taq <- taq[3e4: nrows-3e4), c("SIZE", "PRICE")]

# taq <- taq[taq$SIZE > 200]
# closep <- matrix(taq$PRICE, nc=1)
# volumes <- matrix(taq$SIZE, nc=1)
# retv <- rutils::diffit(closep)


## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(paste(symbol, "High Frequency Strategy")),
  
  # create single row with four slider inputs
  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
    column(width=12, 
           h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
           actionButton("recalcb", "Recalculate the Model")),
    # Input stock symbol
    # column(width=2, selectInput("symbol", label="Symbol",
    #                             choices=symbolv, selected=symbol)),
    # Input model type
    column(width=2, selectInput("model_type", label="model",
                                choices=c("VWAP", "Hampel", "ZScore"), selected="ZScore")),
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Lookback interval",
                                min=1, max=300, value=100, step=1)),
    # Input lag trade parameter
    column(width=2, sliderInput("lagg", label="Confirmation signals", min=1, max=5, value=2, step=1)),
    # Input trim parameter to remove price jumps - smaller tri_m removes more price jumps
    # column(width=2, sliderInput("tri_m", label="Trim", min=0.1, max=10, value=10.0, step=0.1)),
    # Input lots parameter to remove odd lots - larger lot_s removes more ticks
    column(width=2, sliderInput("lot_s", label="Lots number", min=100, max=1e3, value=200, step=50)),
    # Input threshold parameter to control position flipping
    column(width=2, sliderInput("threshold", label="Threshold", min=0.0, max=4.0, value=0.0, step=0.1)),
    # Input trending or reverting (contrarian) strategy
    column(width=2, selectInput("coeff", label="Trend (1) Revert (-1)",
                                choices=c(1, -1), selected=(-1)))
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface


## Define the server code
servfun <- shiny::shinyServer(function(input, output) {

  # Recalculate the data and rerun the model
  datav <- shiny::reactive({
    # Get model parameters from input argument
    model_type <- isolate(input$model_type)
    look_back <- isolate(input$look_back)
    lagg <- isolate(input$lagg)
    threshold <- isolate(input$threshold)
    # tri_m <- input$tri_m
    lot_s <- isolate(input$lot_s)
    coeff <- as.numeric(isolate(input$coeff))
    # Model is recalculated when the recalcb variable is updated
    input$recalcb
    
    # Prepare data
    # taq <- taq[taq$SIZE > lot_s]
    taq <- taq[quantmod::Vo(taq) > lot_s, ]
    closep <- quantmod::Cl(taq)
    # closep <- (max(closep) + 10 - closep)
    volumes <- quantmod::Vo(taq)
    retv <- rutils::diffit(closep)
    nrows <- NROW(taq)


    if (model_type == "VWAP") {
      # VWAP model
      vwapv <- HighFreq::roll_sum(tseries=closep*volumes, look_back=look_back)
      volume_rolling <- HighFreq::roll_sum(tseries=volumes, look_back=look_back)
      vwapv <- vwapv/volume_rolling
      vwapv[is.na(vwapv)] <- 0
      # Calculate VWAP indicator
      indic_long <- ((closep - vwapv) > threshold)
      indic_short <- ((closep - vwapv) < (-threshold))
      # End VWAP model
    } else if (model_type == "Hampel") {
      # Hampel model
      medianv <- TTR::runMedian(retv, n=look_back)
      medianv[1:look_back] <- 1
      madv <- TTR::runMAD(retv, n=look_back)
      madv[1:look_back] <- 1
      zscores <- ifelse(madv!=0, (retv-medianv)/madv, 0)
      zscores[1:look_back] <- 0
      mad_zscores <- TTR::runMAD(zscores, n=look_back)
      mad_zscores[1:look_back] <- 0
      indic_long <- (zscores > threshold*mad_zscores)
      indic_short <- (zscores < (-threshold*mad_zscores))
      # End Hampel model
    } else if (model_type == "ZScore") {
      # Z-Score regression model
      predv <- matrix(1:nrows, nc=1)
      score <- HighFreq::roll_zscores(respv=closep, predictor=predv, look_back=look_back)
      colnames(score) <- "score"
      score[1:look_back] <- 0
      score[is.infinite(score)] <- 0
      indic_long <- (score > threshold)
      indic_short <- (score < (-threshold))
      # End Z-Score regression model
    }  # end if
    
    ## Calculate the positions
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    indic <- HighFreq::roll_count(indic_long)
    posv <- ifelse(indic >= lagg, 1, posv)
    indic <- HighFreq::roll_count(indic_short)
    posv <- ifelse(indic >= lagg, -1, posv)
    # Lag the positions to trade in next period
    posv <- zoo::na.locf(posv, na.rm=FALSE)
    posv <- rutils::lagit(posv, lagg=1)
    pnls <- cumsum(coeff*returns*posv)
    pnls <- cbind(closep, pnls)
    # indeks <- seq.POSIXt(Sys.time()-NROW(pnls)+1, Sys.time(), by=1)
    # pnls <- xts::xts(coredata(pnls), indeks)
    colnames(pnls) <- c(symbol, "strategy")
    # pnls[rutils::calc_endpoints(xtes=pnls, interval="hours")]
    pnls
  })  # end reactive code
  
  # return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    # plot(coredata(datav()), t="l")
    colnamev <- colnames(datav())
    dygraphs::dygraph(datav(), main=paste(colnamev[1], "Strategy")) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
      # dySeries(name=colnamev[3], axis="y", label=colnamev[3], strokeWidth=2, col="green") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
