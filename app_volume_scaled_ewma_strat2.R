##############################
# This is a shiny app for simulating an EWMA moving average 
# crossover strategy.
# It uses the returns scaled by the trading volume (volume clock).
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

# cap_tion <- paste("EWMA Strategy for", sym_bol)
cap_tion <- paste("EWMA Moving Average Strategy Using the Trading Volumes")

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(cap_tion),

  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the re_calculate variable is updated
    # column(width=2,
           # h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
           # actionButton("re_calculate", "Click to Recalculate")),
    # Input stock symbol
    column(width=3, selectInput("sym_bol", label="Symbol",
                                choices=c("SPY", rutils::etf_env$sym_bols), selected="VTI")),
    # Input EWMA decays
    column(width=2, sliderInput("fast_lambda", label="fast_lambda:", min=0.1, max=0.3, value=0.2, step=0.001)),
    column(width=2, sliderInput("slow_lambda", label="slow_lambda:", min=0.0, max=0.2, value=0.1, step=0.001)),
    # Input end points interval
    # column(width=2, selectInput("inter_val", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Look-back", min=5, max=300, value=100, step=1)),
    # Input look-back lag interval
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=8, value=2, step=1)),
    # Input exponent of volume
    column(width=2, sliderInput("expo_nent", label="exponent", min=0.05, max=2.0, value=1.0, step=0.05)),
    # Input exponent of volume
    column(width=2, sliderInput("floo_r", label="vol floor", min=0.01, max=0.25, value=0.1, step=0.01))
    
  ),  # end fluidRow

  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph", width="100%", height="550px"), height=10, width=11)

)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {

  # Create an empty list of reactive values.
  value_s <- reactiveValues()
  value_s$minute_ly <- FALSE
  
  # Load the data
  da_ta <- reactive({
    sym_bol <- input$sym_bol
    cat("Loading data for ", sym_bol, "\n")
    
    if (sym_bol == "SPY") {
      ## SPY ETF 1-minute bars
      value_s$minute_ly <- TRUE
      oh_lc <- HighFreq::SPY["2009"]
      # n_rows <- NROW(oh_lc)
      price_s <- quantmod::Cl(oh_lc)
      vol_ume <- quantmod::Vo(oh_lc)
      re_turns <- rutils::diff_it(log(price_s))
      re_turns <- re_turns/sd(re_turns)
      cbind(re_turns, vol_ume)
    } else {
      value_s$minute_ly <- FALSE
      oh_lc <- get(sym_bol, rutils::etf_env)
      # n_rows <- NROW(oh_lc)
      price_s <- quantmod::Cl(oh_lc)
      vol_ume <- quantmod::Vo(oh_lc)
      re_turns <- rutils::diff_it(log(price_s))
      re_turns <- re_turns/sd(re_turns)
      cbind(re_turns, vol_ume)
    }  # end if
    
  })  # end reactive
  

  # Scale the data
  scaled_data <- reactive({
    cat("Scaling data for ", input$sym_bol, "\n")
    # Get model parameters from input argument
    expo_nent <- input$expo_nent
    look_back <- input$look_back
    floo_r <- input$floo_r
    
    re_turns <- da_ta()[, 1]
    vol_ume <- da_ta()[, 2]
    
    # Set the floor on volume using the rolling volume
    # roll_vol <- HighFreq::roll_sum(vol_ume, look_back=look_back)/look_back
    # vol_floor <- floo_r*roll_vol
    # vol_ume <- ifelse(vol_ume < vol_floor, vol_floor, vol_ume)
    # Scale the volume by the rolling volume
    # vol_ume <- vol_ume/roll_vol
    
    # Divide returns by the volume (volume clock).
    # rets_scaled <- ifelse(vol_ume > 0, re_turns/(vol_ume^expo_nent), 0)
    # rets_scaled <- rets_scaled/sd(rets_scaled)
    rets_scaled <- vol_ume
    
    # Calculate daily returns from minutely prices
    if (value_s$minute_ly) {
      cum_sum <- cumsum(re_turns)
      cum_sum <- xts::to.daily(cum_sum)
      re_turns <- rutils::diff_it(quantmod::Cl(cum_sum))
      # cum_sum <- cumsum(re_turns)
      cum_scaled <- xts::xts(cumsum(rets_scaled), index(da_ta()))
      cum_scaled <- xts::to.daily(cum_scaled)
      rets_scaled <- rutils::diff_it(quantmod::Cl(cum_scaled))
      # cum_scaled <- cumsum(rets_scaled)
    } # end if
    
    cbind(re_turns, rets_scaled)
    
  })  # end reactive code


  # Recalculate the strategy
  model_data <- reactive({
    cat("Recalculating strategy for ", input$sym_bol, "\n")
    # Get model parameters from input argument
    fast_lambda <- input$fast_lambda
    slow_lambda <- input$slow_lambda
    look_back <- input$look_back
    lagg <- input$lagg
    # fac_tor <- input$fac_tor
    # input$re_calculate
    
    # Calculate cumulative returns
    re_turns <- scaled_data()[, 1]
    cum_sum <- cumsum(re_turns)
    rets_scaled <- scaled_data()[, 2]
    cum_scaled <- cumsum(rets_scaled)
    n_rows <- NROW(re_turns)
    
    # Calculate EWMA weights
    fast_weights <- exp(-fast_lambda*1:look_back)
    fast_weights <- fast_weights/sum(fast_weights)
    slow_weights <- exp(-slow_lambda*1:look_back)
    slow_weights <- slow_weights/sum(slow_weights)
    
    # Calculate EWMA prices by filtering with the weights
    # cum_scaled <- cumsum(rets_scaled)
    fast_ewma <- .Call(stats:::C_cfilter, rets_scaled, filter=fast_weights, sides=1, circular=FALSE)
    fast_ewma[1:(look_back-1)] <- fast_ewma[look_back]
    slow_ewma <- .Call(stats:::C_cfilter, rets_scaled, filter=slow_weights, sides=1, circular=FALSE)
    slow_ewma[1:(look_back-1)] <- slow_ewma[look_back]
    
    # Determine dates when the EWMAs have crossed
    in_dic <- sign(fast_ewma - slow_ewma)
    
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
    position_s[1:lagg] <- 0
    # Lag the positions to trade in next period
    position_s <- rutils::lag_it(position_s, lagg=1)
    
    # position_s <- sin(5*pi*(1:n_rows)/n_rows)
    
    # Calculate strategy pnl_s
    fac_tor <- (-1)
    # pnl_s <- 0.5*((fac_tor*position_s*re_turns) + re_turns)
    pnl_s <- fac_tor*position_s*re_turns
    # std_dev <- sd(re_turns[re_turns<0])
    pnl_s <- pnl_s*sd(re_turns[re_turns<0])/sd(pnl_s[pnl_s<0])
    
    # 
    da_ta <- cbind(re_turns, pnl_s, position_s)
    # da_ta <- cbind(re_turns, rets_scaled, position_s)
    
    # da_ta <- xts::xts(da_ta, seq.POSIXt(from=index(first(oh_lc)), by="hour", length.out=NROW(da_ta)))
    # col_names <- c("SPY Returns", "Scaled by Volume", "Positions")
    col_names <- c(paste(input$sym_bol, "Returns"), "Strategy", "Positions")
    colnames(da_ta) <- col_names
    da_ta
  })  # end reactive code
  

  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph({
    da_ta <- model_data()
    col_names <- colnames(da_ta)[1:2]
    
    sharp_e <- sqrt(252)*sapply(da_ta[, 1:2], function(x) mean(x)/sd(x[x<0]))
    sharp_e <- round(sharp_e, 3)

    # Calculate number of trades
    n_trades <- sum(abs(rutils::diff_it(da_ta[, "Positions"])))# / n_rows

    cap_tion <- paste("Strategy for", input$sym_bol, "Trading Volumes, \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharp_e, collapse=", "), ", \n",
                      "Number of trades=", n_trades)

    da_ta <- cumsum(da_ta[, 1:2])
    
    dygraphs::dygraph(da_ta, main=cap_tion) %>%
      dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
      dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="blue") %>%
      dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="red") %>%
      dyLegend(width=500)
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
