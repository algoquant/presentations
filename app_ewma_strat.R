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

# sym_bol <- "CAN"
# oh_lc <- data.table::fread(file="C:/Develop/predictive/data/outfile_can.csv", sep=",")

# vol_ume <- oh_lc$volume
# plot(vol_ume, t="l", ylim=c(0, 1e2))
# sum(vol_ume[re_turns>0])

# op_en <- log(oh_lc$open_p)
# hi_gh <- log(oh_lc$high)
# lo_w <- log(oh_lc$low)
# clos_e <- log(oh_lc$close_p)

sym_bol <- "LODE"
oh_lc <- data.table::fread(file="C:/Develop/predictive/data/lode_oneminutebars.csv", sep=",")
n_rows <- NROW(oh_lc)
clos_e <- log(oh_lc$close)


# sym_bol <- "SPY"
# oh_lc <- HighFreq::SPY
# clos_e <- log(oh_lc$SPY.Close)


re_turns <- rutils::diff_it(clos_e)
std_dev <- sd(re_turns[re_turns<0])
cum_sum <- cumsum(re_turns)
# rang_e <- (hi_gh - lo_w)
# The re_scaled re_turns are skewed towards negative re_turns 
# because vol_ume is larger for positive re_turns. 
# re_scaled <- mean(vol_ume)*ifelse(vol_ume > 0, re_turns/vol_ume, 0)
# re_scaled <- ifelse(rang_e > 0, re_turns/log(rang_e), 0)
# re_scaled <- mean(vol_ume)*re_turns/vol_ume^0.2
# re_scaled <- (re_scaled - mean(re_scaled))
# re_scaled <- cumsum(re_scaled)
# plot(re_scaled, t="l")



date_s <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out=n_rows)
# clos_e <- xts::xts(clos_e, date_s)
# dygraphs::dygraph(clos_e)


# col_names <- c("Prices", "Rescaled")
# da_ta <- cbind(clos_e, re_scaled)
# colnames(da_ta) <- col_names
# dygraphs::dygraph(da_ta, main="Prices Rescaled by Volume") %>%
#   dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
#   dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
#   dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
#   dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue") %>%
#   dyLegend(width=500)
  


# plot(clos_e, t="l")


cap_tion <- paste("Contrarian Strategy for", sym_bol, "Using Two EWMAs")
# cap_tion <- paste("Contrarian Strategy for", sym_bol, "Using the Returns Scaled by the Price Range")

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(cap_tion),

  fluidRow(
    # The Shiny App is re-calculated when the actionButton is clicked and the re_calculate variable is updated
    column(width=12,
           h4("Click the button 'Recalculate the Model' to re-calculate the Shiny App."),
           actionButton("re_calculate", "Recalculate the Model"))
  ),  # end fluidRow

  # Create single row with two slider inputs
  fluidRow(
    # Input stock symbol
    # column(width=3, selectInput("sym_bol", label="Symbol",
    #                             choices=sym_bols, selected=sym_bol)),
    # Input EWMA decays
    column(width=3, sliderInput("fast_lambda", label="fast_lambda:", min=0.01, max=0.9, value=0.7, step=0.01)),
    column(width=3, sliderInput("slow_lambda", label="slow_lambda:", min=0.01, max=0.9, value=0.25, step=0.01)),
    # Input end points interval
    # column(width=3, selectInput("inter_val", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Look-back", min=11, max=100, value=100, step=1)),
    # Input look-back lag interval
    column(width=3, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1)),
    
    # Input confirmation signal Boolean
    # column(width=3, selectInput("con_firm", label="Confirm the signal", choices=c("True", "False"), selected="False")),
    
    # Input threshold interval
    # column(width=3, sliderInput("thresh_old", label="Threshold", min=0.2, max=2.0, value=0.8, step=0.1)),
    # Input minimum trade volume for filtering ticks
    # column(width=3, sliderInput("vol_ume", label="Big tick volume", min=50, max=1000, value=400, step=50)),
    # Input the weight decay parameter
    # column(width=3, sliderInput("lamb_da", label="Weight decay:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input model weights type
    # column(width=3, selectInput("typ_e", label="Portfolio weights type",
    #                             choices=c("max_sharpe", "min_var", "min_varpca", "rank"), selected="rank")),
    # Input number of eigenvalues for regularized matrix inverse
    # column(width=3, sliderInput("max_eigen", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
    # Input the shrinkage intensity
    # column(width=3, sliderInput("al_pha", label="Shrinkage intensity",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input the percentile
    # column(width=3, sliderInput("percen_tile", label="percentile:", min=0.01, max=0.45, value=0.1, step=0.01)),
    # Input the strategy coefficient: fac_tor=1 for momentum, and fac_tor=-1 for contrarian
    # column(width=3, selectInput("fac_tor", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    column(width=3, numericInput("bid_offer", label="Bid-offer:", value=0.0000, step=0.0001)),
    # If fac_tor=1 then trending, If fac_tor=(-1) then contrarian
    # column(width=3, numericInput("fac_tor", "Trend coefficient:", value=1)),
    column(width=3, selectInput("fac_tor", label="Trend coefficient",
                                choices=c(1, -1), selected=(1)))
  ),  # end fluidRow

  # Create output plot panel
  mainPanel(dygraphOutput("dy_graph"), height=8, width=12)

)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {

  # Re-calculate the data and rerun the strategy
  da_ta <- reactive({
    # Get model parameters from input argument
    fast_lambda <- isolate(input$fast_lambda)
    slow_lambda <- isolate(input$slow_lambda)
    # sym_bol <- isolate(input$sym_bol)
    # model_type <- isolate(input$model_type)
    look_back <- isolate(input$look_back)
    lagg <- isolate(input$lagg)
    # con_firm <- isolate(input$con_firm)
    # max_eigen <- isolate(input$max_eigen)
    # thresh_old <- isolate(input$thresh_old)
    # vol_ume <- isolate(input$vol_ume)
    # look_lag <- isolate(input$look_lag
    # lamb_da <- isolate(input$lamb_da)
    # typ_e <- isolate(input$typ_e)
    # al_pha <- isolate(input$al_pha)
    # percen_tile <- isolate(input$percen_tile)
    # fac_tor <- as.numeric(isolate(input$fac_tor))
    bid_offer <- isolate(input$bid_offer)
    fac_tor <- as.numeric(isolate(input$fac_tor))
    # Strategy is re-calculated when the re_calculate variable is updated
    input$re_calculate

    # calculate EWMA prices
    fast_weights <- exp(-fast_lambda*1:look_back)
    fast_weights <- fast_weights/sum(fast_weights)
    slow_weights <- exp(-slow_lambda*1:look_back)
    slow_weights <- slow_weights/sum(slow_weights)
    
    # Filter the prices using weights
    fast_ewma <- .Call(stats:::C_cfilter, cum_sum, filter=fast_weights, sides=1, circular=FALSE)
    fast_ewma[1:(look_back-1)] <- fast_ewma[look_back]
    slow_ewma <- .Call(stats:::C_cfilter, cum_sum, filter=slow_weights, sides=1, circular=FALSE)
    slow_ewma[1:(look_back-1)] <- slow_ewma[look_back]
    # Determine dates when the EWMAs have crossed
    in_dic <- sign(fast_ewma - slow_ewma)
    
    # Older code
    # trade_dates <- (rutils::diff_it(in_dic) != 0)
    # trade_dates <- which(trade_dates)
    # trade_dates <- trade_dates[trade_dates < n_rows]
    # position_s <- rep(NA_integer_, n_rows)
    # position_s[1] <- 0
    # Flip position if the scaled returns exceed thresh_old
    # position_s[re_scaled > thresh_old] <- 1
    # position_s[re_scaled < (-thresh_old)] <- (-1)
    # LOCF
    # position_s <- zoo::na.locf(position_s, na.rm=FALSE)
    # position_s <- rutils::lag_it(position_s, lagg=lagg)
    # Calculate positions, either: -1, 0, or 1
    # position_s <- rep(NA_integer_, NROW(clos_e))
    # position_s[1] <- 0
    # position_s[trade_dates] <- in_dic[trade_dates]
    # position_s[trade_dates] <- rutils::lag_it(in_dic)[trade_dates]
    # position_s <- na.locf(position_s)
    # position_s <- rutils::lag_it(position_s)
    
    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the in_dic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # position_s <- ifelse(in_dic == indic_lag, in_dic, position_s)
    indic_sum <- HighFreq::roll_vec(se_ries=in_dic, look_back=lagg)
    indic_sum[1:lagg] <- 0
    position_s <- rep(NA_integer_, n_rows)
    position_s[1] <- 0
    position_s <- ifelse(indic_sum == lagg, 1, position_s)
    position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
    position_s <- zoo::na.locf(position_s, na.rm=FALSE)
    # position_s[1:lagg] <- 0
    # Lag the positions to trade in next period
    position_s <- rutils::lag_it(position_s, lagg=1)
    
    # Calculate strategy pnl_s
    pnl_s <- (fac_tor*position_s*re_turns)

    # Calculate position turnover
    turn_over <- abs(rutils::diff_it(position_s))/2
    # Calculate number of trades
    # sum(turn_over)/NROW(position_s)
    # Calculate transaction costs
    cost_s <- bid_offer*turn_over
    pnl_s <- (pnl_s - cost_s)

    pnl_s <- std_dev*pnl_s/sd(pnl_s[pnl_s<0])
    pnl_s <- cbind(pnl_s, re_turns)
    # Coerce pnl_s to xts
    pnl_s <- xts::xts(pnl_s, date_s)
    
    # sharp_e <- sqrt(252)*sapply(pnl_s, function(x) mean(x)/sd(x))
    sharp_e <- sqrt(252)*sapply(pnl_s, function(x) mean(x)/sd(x[x<0]))
    sharp_e <- round(sharp_e, 3)
    # pnl_s <- apply(pnl_s, MARGIN=2, cumsum)
    pnl_s <- cumsum(pnl_s)
    pnl_s <- cbind(pnl_s, fast_ewma, slow_ewma)
    colnames(pnl_s) <- c(paste0(c("Strategy SR=", "Index SR="), sharp_e), "fast", "slow")
    pnl_s
  })  # end reactive code

  # Return to the output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph({
    col_names <- colnames(da_ta())
    dygraphs::dygraph(da_ta(), main=cap_tion) %>%
      dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
      dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
      dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue") %>%
      dySeries(name=col_names[3], axis="y2", label=col_names[3], strokeWidth=1, col="orange") %>%
      dySeries(name=col_names[4], axis="y2", label=col_names[4], strokeWidth=1, col="lightpurple") %>%
      dyLegend(width=500)
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
