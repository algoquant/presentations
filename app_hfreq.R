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

# file_name <- "aapl_tick_trades_2020_07_20_biglots"
# file_name <- "aapl_tick_trades_2020_03_16_biglots"
# file_name <- "aapl_tick_trades_2020_02_10_2020_05_10_biglots"
# file_name <- "vxx_tick_trades_2020_03_16_biglots"
# file_name <- "vxx_tick_trades_2020_02_20_2020_03_10_biglots"
# file_name <- "vxx_tick_trades_2020_02_10_2020_05_10_biglots"
# file_name <- "xlk_tick_trades_2020_03_16_biglots"
# file_name <- "xlk_tick_trades_2020_biglots"
# file_name <- "goog_tick_trades_2020_02_10_2020_05_10_biglots"
file_name <- "goog_tick_trades_2020_02_10_2020_05_10_biglots"

# ta_q <- data.table::fread(file=paste0("C:/Develop/data/", file_name, ".csv"), stringsAsFactors=FALSE)
# Create date-time index
# in_dex <- paste(ta_q$DATE, ta_q$TIME_M)
# in_dex <- strptime(in_dex, "%Y%m%d %H:%M:%OS")
# in_dex <- as.POSIXct(in_dex)
load(file=paste0("C:/Develop/data/TwoSig/", file_name, ".RData"))

# sym_bol <- ta_q$SYM_ROOT[1]
sym_bol <- rutils::get_name(colnames(ta_q)[1])

# ta_q <- ta_q[3e4:(n_rows-3e4), c("SIZE", "PRICE")]

# ta_q <- ta_q[ta_q$SIZE > 200]
# clos_e <- matrix(ta_q$PRICE, nc=1)
# vol_ume <- matrix(ta_q$SIZE, nc=1)
# re_turns <- rutils::diff_it(clos_e)


## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(paste(sym_bol, "High Frequency Strategy")),
  
  # create single row with four slider inputs
  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the re_calculate variable is updated
    column(width=12, 
           h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
           actionButton("re_calculate", "Recalculate the Model")),
    # Input stock symbol
    # column(width=2, selectInput("sym_bol", label="Symbol",
    #                             choices=sym_bols, selected=sym_bol)),
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
    column(width=2, sliderInput("thresh_old", label="Threshold", min=0.0, max=4.0, value=0.0, step=0.1)),
    # Input trending or reverting (contrarian) strategy
    column(width=2, selectInput("co_eff", label="Trend (1) Revert (-1)",
                                choices=c(1, -1), selected=(-1)))
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph"), width=12)
)  # end fluidPage interface


## Define the server code
ser_ver <- shiny::shinyServer(function(input, output) {

  # Recalculate the data and rerun the model
  da_ta <- reactive({
    # Get model parameters from input argument
    model_type <- isolate(input$model_type)
    look_back <- isolate(input$look_back)
    lagg <- isolate(input$lagg)
    thresh_old <- isolate(input$thresh_old)
    # tri_m <- input$tri_m
    lot_s <- isolate(input$lot_s)
    co_eff <- as.numeric(isolate(input$co_eff))
    # Model is recalculated when the re_calculate variable is updated
    input$re_calculate
    
    # Prepare data
    # ta_q <- ta_q[ta_q$SIZE > lot_s]
    ta_q <- ta_q[quantmod::Vo(ta_q) > lot_s, ]
    clos_e <- quantmod::Cl(ta_q)
    # clos_e <- (max(clos_e) + 10 - clos_e)
    vol_ume <- quantmod::Vo(ta_q)
    re_turns <- rutils::diff_it(clos_e)
    n_rows <- NROW(ta_q)


    if (model_type == "VWAP") {
      # VWAP model
      v_wap <- HighFreq::roll_sum(se_ries=clos_e*vol_ume, look_back=look_back)
      volume_rolling <- HighFreq::roll_sum(se_ries=vol_ume, look_back=look_back)
      v_wap <- v_wap/volume_rolling
      v_wap[is.na(v_wap)] <- 0
      # Calculate VWAP indicator
      indic_long <- ((clos_e - v_wap) > thresh_old)
      indic_short <- ((clos_e - v_wap) < (-thresh_old))
      # End VWAP model
    } else if (model_type == "Hampel") {
      # Hampel model
      medi_an <- TTR::runMedian(re_turns, n=look_back)
      medi_an[1:look_back] <- 1
      ma_d <- TTR::runMAD(re_turns, n=look_back)
      ma_d[1:look_back] <- 1
      z_scores <- ifelse(ma_d!=0, (re_turns-medi_an)/ma_d, 0)
      z_scores[1:look_back] <- 0
      mad_zscores <- TTR::runMAD(z_scores, n=look_back)
      mad_zscores[1:look_back] <- 0
      indic_long <- (z_scores > thresh_old*mad_zscores)
      indic_short <- (z_scores < (-thresh_old*mad_zscores))
      # End Hampel model
    } else if (model_type == "ZScore") {
      # Z-Score regression model
      de_sign <- matrix(1:n_rows, nc=1)
      sig_nal <- HighFreq::roll_zscores(res_ponse=clos_e, de_sign=de_sign, look_back=look_back)
      colnames(sig_nal) <- "sig_nal"
      sig_nal[1:look_back] <- 0
      sig_nal[is.infinite(sig_nal)] <- 0
      indic_long <- (sig_nal > thresh_old)
      indic_short <- (sig_nal < (-thresh_old))
      # End Z-Score regression model
    }  # end if
    
    ## Calculate the positions
    position_s <- rep(NA_integer_, n_rows)
    position_s[1] <- 0
    indica_tor <- HighFreq::roll_count(indic_long)
    position_s <- ifelse(indica_tor >= lagg, 1, position_s)
    indica_tor <- HighFreq::roll_count(indic_short)
    position_s <- ifelse(indica_tor >= lagg, -1, position_s)
    # Lag the positions to trade in next period
    position_s <- zoo::na.locf(position_s, na.rm=FALSE)
    position_s <- rutils::lag_it(position_s, lagg=1)
    pnl_s <- cumsum(co_eff*re_turns*position_s)
    pnl_s <- cbind(clos_e, pnl_s)
    # in_dex <- seq.POSIXt(Sys.time()-NROW(pnl_s)+1, Sys.time(), by=1)
    # pnl_s <- xts::xts(coredata(pnl_s), in_dex)
    colnames(pnl_s) <- c(sym_bol, "strategy")
    # pnl_s[rutils::calc_endpoints(x_ts=pnl_s, inter_val="hours")]
    pnl_s
  })  # end reactive code
  
  # return the dygraph plot to output argument
  output$dy_graph <- dygraphs::renderDygraph({
    # plot(coredata(da_ta()), t="l")
    col_names <- colnames(da_ta())
    dygraphs::dygraph(da_ta(), main=paste(col_names[1], "Strategy")) %>%
      dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
      dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
      # dySeries(name=col_names[3], axis="y", label=col_names[3], strokeWidth=2, col="green") %>%
      dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
