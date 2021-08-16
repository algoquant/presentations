##############################
# This is a shiny app for simulating a contrarian strategy using 
# the Hampel filter over prices scaled by the trading volumes.
# The model flips the position only if the indicator persists over 
# several consectutive periods equal to lagg.
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

# sym_bols <- names(data_env)
sym_bols <- c("SPY", "VXX", "LODE", "GME")
sym_bol <- "VXX"

cap_tion <- paste("Contrarian Strategy Using the Hampel Filter Over Prices")
# cap_tion <- paste("Contrarian Strategy for", sym_bol, "Using the Hampel Filter Over Prices")

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(cap_tion),
  
  # fluidRow(
  # The Shiny App is recalculated when the actionButton is clicked and the add_annotations variable is updated
  #   column(width=12,
  #          h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
  #          actionButton("add_annotations", "Recalculate the Model"))
  # ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input end points interval
    # column(width=2, selectInput("inter_val", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input stock symbol
    column(width=2, selectInput("sym_bol", label="Symbol",
                                choices=sym_bols, selected=sym_bol)),
    # Input short look-back interval
    column(width=2, sliderInput("short_back", label="Short lookback", min=3, max=40, value=15, step=1)),
    # Input long look-back interval
    # column(width=2, sliderInput("long_back", label="Long lookback", min=10, max=200, value=100, step=2)),
    # Input lag trade parameter
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=1, step=1)),
    # Input threshold level
    column(width=2, sliderInput("thresh_old", label="threshold", min=0.5, max=3.0, value=2.0, step=0.1)),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False"))
    # Input the weight decay parameter
    # column(width=2, sliderInput("lamb_da", label="Weight decay:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input model weights type
    # column(width=2, selectInput("typ_e", label="Portfolio weights type",
    #                             choices=c("max_sharpe", "min_var", "min_varpca", "rank"), selected="rank")),
    # Input number of eigenvalues for regularized matrix inverse
    # column(width=2, sliderInput("max_eigen", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
    # Input the shrinkage intensity
    # column(width=2, sliderInput("al_pha", label="Shrinkage intensity",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input the percentile
    # column(width=2, sliderInput("percen_tile", label="percentile:", min=0.01, max=0.45, value=0.1, step=0.01)),
    # Input the strategy coefficient: co_eff=1 for momentum, and co_eff=-1 for contrarian
    # column(width=2, selectInput("co_eff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    # column(width=2, numericInput("bid_offer", label="bid-offer:", value=0.001, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph"), width=12)
  
)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {
  
  # Recalculate the data and rerun the model
  # da_ta <- reactive({
  # Get model parameters from input argument
  # max_eigen <- isolate(input$max_eigen)
  # look_lag <- isolate(input$look_lag
  # lamb_da <- isolate(input$lamb_da)
  # typ_e <- isolate(input$typ_e)
  # al_pha <- isolate(input$al_pha)
  # percen_tile <- isolate(input$percen_tile)
  # co_eff <- as.numeric(isolate(input$co_eff))
  # bid_offer <- isolate(input$bid_offer)
  # Model is recalculated when the add_annotations variable is updated
  # input$add_annotations
  
  # Create an empty list of reactive values.
  value_s <- reactiveValues()
  
  # Load data
  oh_lc <- reactive({
    cat("Loading data\n")
    sym_bol <- input$sym_bol
    
    # Select the data: "SPY", "VXX", "LODE" or "GME"
    switch(sym_bol,
           "SPY" = {
             ## SPY ETF 1-minute bars
             # oh_lc <- HighFreq::SPY["2012"]["T09:31:00/T15:59:00"]
             load(file="C:/Develop/data/polygon/spy_minutes.RData")
             # n_rows <- NROW(oh_lc)
             # log(Cl(oh_lc))
             oh_lc["T09:00:00/T16:30:00"]
           },
           "VXX" = {
             ## SPY ETF 1-minute bars
             # oh_lc <- HighFreq::SPY["2012"]["T09:31:00/T15:59:00"]
             load(file="C:/Develop/data/polygon/vxx_minutes.RData")
             # n_rows <- NROW(oh_lc)
             # log(Cl(oh_lc))
             oh_lc["T09:00:00/T16:30:00"]
           },
           "LODE" = {
             ## LODE 1-minute bars
             oh_lc <- data.table::fread(file="C:/Develop/predictive/data/lode_oneminutebars.csv", sep=",")
             oh_lc <- oh_lc[, c(4, 6, 7, 5, 2)]
             colnames(oh_lc) <- c("Open", "High", "Low", "Close", "Volume")
             n_rows <- NROW(oh_lc)
             date_s <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out=n_rows)
             xts::xts(as.matrix(oh_lc), date_s)
           },
           "GME" = {
             ## GME 1-minute bars
             oh_lc <- data.table::fread(file="C:/Develop/predictive/data/gme_oneminutebars.csv", sep=",")
             oh_lc <- oh_lc[, c(4, 6, 7, 5, 2)]
             colnames(oh_lc) <- c("Open", "High", "Low", "Close", "Volume")
             n_rows <- NROW(oh_lc)
             date_s <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out=n_rows)
             xts::xts(as.matrix(oh_lc), date_s)
           }
    )  # end switch
    
  })  # end reactive
  
  # Calculate log close prices
  # clos_e <- reactive({
  #   cat("Calculating log close prices\n")
  #   log(Cl(oh_lc()))
  # })  # end reactive
  
  # Calculate log returns
  re_turns <- reactive({
    cat("Calculating log returns\n")
    rutils::diff_it(log(Cl(oh_lc())))
  })  # end reactive
  
  # Calculate z_scores if there are new short_back and long_back values
  z_scores <- reactive({
    cat("Calculating z_scores\n")
    short_back <- input$short_back
    # long_back <- input$long_back
    
    # Calculate the rolling volume
    vol_ume <- Vo(oh_lc())
    # Scale the volume by the rolling volume
    vol_ume <- short_back*vol_ume/HighFreq::roll_sum(vol_ume, look_back=short_back)
    # re_turns <- rutils::diff_it(clos_e())
    # Calculate the cumulative returns scaled by the rolling volume
    vol_ume <- rutils::lag_it(vol_ume, pad_zeros=FALSE)
    re_turns <- ifelse(vol_ume > 0, re_turns()/vol_ume, 0)
    # re_turns[is.na(re_turns) | is.infinite(re_turns)] <- 0
    cum_scaled <- cumsum(re_turns)

    # Calculate the rolling median of the cumulative returns
    mi_n <- roll::roll_min(cum_scaled, width=short_back)
    ma_x <- roll::roll_max(cum_scaled, width=short_back)
    mi_n[1:short_back, ] <- 0
    ma_x[1:short_back, ] <- 1
    mi_n <- rutils::lag_it(mi_n, pad_zeros=FALSE)
    ma_x <- rutils::lag_it(ma_x, pad_zeros=FALSE)
    # Don't divide z_scores by the ma_d because it's redundant since z_scores is divided by the mad_zscores.
    # Old code:
    # ma_d <- TTR::runMAD(re_turns, n=short_back)
    # ma_d[1:short_back, ] <- 1
    # z_scores <- ifelse(ma_d != 0, (clos_e-mi_n)/ma_d, 0)
    # Calculate the z_scores as the rolling cumulative returns
    z_scores <- ifelse(ma_x > mi_n, (2*cum_scaled - mi_n - ma_x)/(ma_x - mi_n), 0)
    # z_scores[is.na(z_scores) | is.infinite(z_scores)] <- 0
    # Standardize the z_scores
    # Old code:
    # z_scores[1:short_back, ] <- 0
    # med_zscores <- TTR::runMedian(z_scores, n=long_back)
    # med_zscores[1:(long_back), ] <- 0
    # mad_zscores <- TTR::runMAD(z_scores, n=long_back)
    # mad_zscores[1:(long_back), ] <- 0
    # ifelse(mad_zscores != 0, (z_scores - med_zscores)/mad_zscores, 0)
    # Standardize the z_scores - HighFreq::roll_scale() is fastest
    # z_scores <- HighFreq::roll_scale(z_scores, look_back=long_back, use_median=TRUE)
    # z_scores[is.na(z_scores) | is.infinite(z_scores)] <- 0
    z_scores
  })  # end reactive
  
  # Plot histogram of z_scores
  # range(z_scores)
  # z_scores <- z_scores[z_scores > quantile(z_scores, 0.05)]
  # z_scores <- z_scores[z_scores < quantile(z_scores, 0.95)]
  # x11(width=6, height=5)
  # hist(z_scores, xlim=c(quantile(z_scores, 0.05), quantile(z_scores, 0.95)), breaks=50, main=paste("Z-scores for", "short_back =", short_back))
  
  # Calculate position_s and pnl_s if there's new thresh_old value
  pnl_s <- reactive({
    cat("Calculating position_s and pnl_s\n")
    thresh_old <- input$thresh_old
    lagg <- input$lagg
    # re_turns <- rutils::diff_it(clos_e())
    n_rows <- NROW(re_turns())
    # Determine if the z_scores have exceeded the thresh_old
    in_dic <- rep(0, n_rows)
    # in_dic[1] <- 0
    in_dic <- ifelse(z_scores() > thresh_old, -1, in_dic)
    in_dic <- ifelse(z_scores() < (-thresh_old), 1, in_dic)
    # Calculate number of consecutive indicators in same direction.
    # This is designed to avoid trading on microstructure noise.
    # in_dic <- ifelse(in_dic == indic_lag, in_dic, in_dic)
    indic_sum <- HighFreq::roll_vec(tseries=matrix(in_dic), look_back=lagg)
    indic_sum[1:lagg] <- 0
    
    # Calculate position_s and pnls from indic_sum.
    # position_s <- rep(NA_integer_, n_rows)
    # position_s[1] <- 0
    # thresh_old <- 3*mad(z_scores)
    # Flip position only if the indic_sum is at least equal to lagg.
    # Otherwise keep previous position.
    position_s <- rep(NA_integer_, n_rows)
    position_s[1] <- 0
    position_s <- ifelse(indic_sum >= lagg, 1, position_s)
    position_s <- ifelse(indic_sum <= (-lagg), -1, position_s)
    # position_s <- ifelse(z_scores > thresh_old, -1, position_s)
    # position_s <- ifelse(z_scores < (-thresh_old), 1, position_s)
    position_s <- zoo::na.locf(position_s, na.rm=FALSE)
    position_s <- rutils::lag_it(position_s, lagg=1)
    
    # Number of trades
    value_s$n_trades <- sum(abs(rutils::diff_it(position_s)))# / n_rows
    
    pnl_s <- cbind(position_s*re_turns(), re_turns())
    
    # Sharpe
    sharp_e <- sapply(pnl_s, function(x) mean(x)/sd(x[x<0]))
    value_s$sharp_e <- round(sqrt(252)*sharp_e, 3)

    # pnl_s <- cumsum(position_s*re_turns())
    # cum_scaled <- cumsum(re_turns())
    pnl_s <- cumsum(pnl_s)
    cum_scaled <- pnl_s[, 2]
    colnames(pnl_s) <- c("Strategy", "Index")
    
    # Add buy/sell indicators
    in_dic <- rutils::diff_it(position_s)
    indic_buy <- (in_dic > 0)
    indic_sell <- (in_dic < 0)
    
    pnl_s <- cbind(pnl_s, cum_scaled[indic_buy], cum_scaled[indic_sell])
    colnames(pnl_s)[3:4] <- c("Buy", "Sell")
    pnl_s
    # list(caption=cap_tion, pnls=pnl_s)
  })  # end reactive
  
  
  # Plot dygraph of pnl_s if the add_annotations variable is updated
  dy_graph <- reactive({
    cat("Plotting pnl_s\n")
    add_annotations <- input$add_annotations
    # cap_tion <- pnl_s()$caption
    pnl_s <- pnl_s()
    col_names <- colnames(pnl_s)
    # cat(paste("col_names\n", col_names, "\n"))
    # cat(paste("pnl_s\n", tail(pnl_s), "\n"))
    
    cap_tion <- paste(paste("Number of trades =", value_s$n_trades), ",",
                      paste(paste(col_names[1:2], "Sharpe =", value_s$sharp_e), collapse=", "))

    if (add_annotations == "True") {
      # Create a dygraph object with annotations (no plot is created)
      dygraphs::dygraph(pnl_s, main=cap_tion) %>%
        dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
        dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
        # Add second y-axis
        dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue") %>%
        dySeries(name=col_names[3], axis="y2", label=col_names[3], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=col_names[4], axis="y2", label=col_names[4], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == "False") {
      # Create a dygraph object without annotations (no plot is created)
      dygraphs::dygraph(pnl_s[, 1:2], main=cap_tion) %>%
        dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
        # Add second y-axis
        dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
        dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
        dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue")
    }  # end if
    
  })  # end reactive
  
  # Render (plot) the dygraph object and return it to the output argument
  output$dy_graph <- dygraphs::renderDygraph(dy_graph())
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
