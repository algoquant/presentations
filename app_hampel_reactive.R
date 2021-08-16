##############################
# This is a shiny app for simulating a contrarian strategy 
# using the Hampel filter over returns or prices.
# It flips the position only if the indicator persists over several 
# consectutive periods equal to lagg.
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

# sym_bols <- names(data_env)
sym_bols <- c("SPY", "LODE", "GME")
sym_bol <- "LODE"

cap_tion <- paste("Contrarian Strategy Using the Hampel Filter")

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
  
  # Create single row with inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("sym_bol", label="Symbol",
                                choices=sym_bols, selected=sym_bol)),
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
    column(width=2, sliderInput("thresh_old", label="threshold", min=0.5, max=3.0, value=1.2, step=0.1)),
    # Input lag trade parameter
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1))
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
  
  
  # look_back <- 11
  # half_window <- look_back %/% 2

  # Create an empty list of reactive values.
  value_s <- reactiveValues()
  
  # Load data
  clos_e <- reactive({
    cat("Loading data\n")
    sym_bol <- input$sym_bol
    
    # Select the data: "SPY", "LODE" or "GME"
    switch(sym_bol,
           "SPY" = {
             ## SPY ETF 1-minute bars
             oh_lc <- HighFreq::SPY["2011"]["T09:31:00/T15:59:00"]
             # n_rows <- NROW(oh_lc)
             log(Cl(oh_lc))
           },
           "LODE" = {
             ## LODE 1-minute bars
             oh_lc <- data.table::fread(file="C:/Develop/predictive/data/lode_oneminutebars.csv", sep=",")
             n_rows <- NROW(oh_lc)
             date_s <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out=n_rows)
             clos_e <- log(oh_lc$close)
             xts::xts(clos_e, date_s)
           },
           "GME" = {
             ## GME 1-minute bars
             oh_lc <- data.table::fread(file="C:/Develop/predictive/data/gme_oneminutebars.csv", sep=",")
             n_rows <- NROW(oh_lc)
             date_s <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out=n_rows)
             clos_e <- log(oh_lc$close)
             xts::xts(clos_e, date_s)
           }
    )  # end switch

  })  # end reactive
  
  # Calculate z_scores if new there's look_back value
  z_scores <- reactive({
    cat("Calculating z_scores\n")
    look_back <- input$look_back
    data_type <- input$data_type
    
    # long_back <- input$long_back
    # clos_e <- clos_e()
    # Select the data
    if (data_type == "Returns") {
      # cat("Calculating returns\n")
      da_ta <- rutils::diff_it(clos_e())
    } else if (data_type == "Prices") {
      # cat("Calculating prices\n")
      da_ta <- clos_e()
    }  # end if
    # Calculate the z_scores
    # da_ta <- rutils::diff_it(clos_e())
    medi_an <- roll::roll_median(da_ta, width=look_back)
    medi_an[1:look_back, ] <- 1
    # Don't divide z_scores by the ma_d because it's redundant since z_scores is divided by the mad_zscores.
    # Old code:
    # ma_d <- TTR::runMAD(da_ta, n=look_back)
    # ma_d[1:look_back, ] <- 1
    # z_scores <- ifelse(ma_d != 0, (da_ta-medi_an)/ma_d, 0)
    # Calculate cumulative return
    z_scores <- (da_ta - medi_an)
    # Standardize the z_scores
    # Old code:
    # z_scores[1:look_back, ] <- 0
    # med_zscores <- TTR::runMedian(z_scores, n=long_back)
    # med_zscores[1:(long_back), ] <- 0
    # mad_zscores <- TTR::runMAD(z_scores, n=long_back)
    # mad_zscores[1:(long_back), ] <- 0
    # ifelse(mad_zscores != 0, (z_scores - med_zscores)/mad_zscores, 0)
    # Standardize the z_scores - HighFreq::roll_scale() is fastest
    z_scores <- HighFreq::roll_scale(z_scores, look_back=look_back, use_median=TRUE)
    z_scores[is.na(z_scores)] <- 0
    z_scores[is.infinite(z_scores)] <- 0
    z_scores
  })  # end reactive
  
  # Plot histogram of z_scores
  # range(z_scores)
  # z_scores <- z_scores[z_scores > quantile(z_scores, 0.05)]
  # z_scores <- z_scores[z_scores < quantile(z_scores, 0.95)]
  # x11(width=6, height=5)
  # hist(z_scores, xlim=c(quantile(z_scores, 0.05), quantile(z_scores, 0.95)), breaks=50, main=paste("Z-scores for", "look_back =", look_back))
  
  # Calculate position_s and pnl_s if there's new thresh_old value
  pnl_s <- reactive({
    cat("Calculating position_s and pnl_s\n")
    thresh_old <- input$thresh_old
    lagg <- input$lagg
    re_turns <- rutils::diff_it(clos_e())
    n_rows <- NROW(clos_e())
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
    
    # Calculate indicator of flipping the positions
    in_dic <- rutils::diff_it(position_s)
    # Calculate number of trades
    value_s$n_trades <- sum(abs(in_dic)>0)
    
    # Add buy/sell indicators for annotations
    indic_buy <- (in_dic > 0)
    indic_sell <- (in_dic < 0)
    
    # Lag the positions to trade in next period
    position_s <- rutils::lag_it(position_s, lagg=1)
    # Calculate strategy pnl_s
    pnl_s <- cumsum(position_s*re_turns)
    
    # Bind together strategy pnl_s
    cum_rets <- cumsum(re_turns)
    pnl_s <- cbind(cum_rets, pnl_s)
    colnames(pnl_s) <- c("Index", "Strategy")
    
    pnl_s <- cbind(pnl_s, cum_rets[indic_buy], cum_rets[indic_sell])
    colnames(pnl_s)[3:4] <- c("Buy", "Sell")
    pnl_s
    # list(caption=cap_tion, pnls=pnl_s)
  })  # end reactive
  

  # Calculate position_s if there's new thresh_old value
  dy_graph <- reactive({
    
    cat("Plotting pnl_s\n")
    # cap_tion <- pnl_s()$caption
    pnl_s <- pnl_s()
    col_names <- colnames(pnl_s)
    # cat(paste("col_names\n", col_names, "\n"))
    # cat(paste("pnl_s\n", tail(pnl_s), "\n"))
    
    # Calculate Sharpe ratios
    sharp_e <- sqrt(252)*sapply(rutils::diff_it(pnl_s[, 1:2]), function(x) mean(x)/sd(x[x<0]))
    sharp_e <- round(sharp_e, 3)

    # cap_tion <- paste("Contrarian Strategy for", input$sym_bol, "Using the Hampel Filter Over Prices")
    cap_tion <- paste("Strategy for", input$sym_bol, "Over ", input$data_type, "/ \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharp_e, collapse=" / "), "/ \n",
                      "Number of trades=", value_s$n_trades)
    
    
    # Plot with annotations
    add_annotations <- input$add_annotations
    
    if (add_annotations == "True") {
      dygraphs::dygraph(pnl_s, main=cap_tion) %>%
        dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
        dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="blue") %>%
        dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="red") %>%
        dySeries(name=col_names[3], axis="y", label=col_names[3], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=col_names[4], axis="y", label=col_names[4], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == "False") {
        dygraphs::dygraph(pnl_s[, 1:2], main=cap_tion) %>%
          dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
          dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
          dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="blue") %>%
          dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="red")
    }  # end if
        
  })  # end reactive

  # Render the dy_graph object
  # Return to the output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph(dy_graph())
    
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
