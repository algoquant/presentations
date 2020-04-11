##############################
# This is a shiny app for simulating a Kelly strategy.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(shiny)
library(dygraphs)
library(HighFreq)

# Model and data setup

re_turns <- na.omit(rutils::etf_env$re_turns[, "VTI"])


## Load QM futures 5-second bars
# sym_bol <- "ES"  # S&P500 Emini futures
# sym_bol <- "QM"  # oil
# load(file=paste0("C:/Develop/data/ib_data/", sym_bol, "_ohlc.RData"))
# price_s <- Cl(oh_lc)
# Or random prices
# price_s <- xts(exp(cumsum(rnorm(NROW(oh_lc)))), index(oh_lc))

## Load VX futures 5-second bars
# sym_bol <- "VX"
# load(file="C:/Develop/data/vix_data/vix_cboe.RData")
# price_s <- Cl(vix_env$chain_ed)

## VTI ETF daily bars
# sym_bol <- "VTI"
# price_s <- Cl(rutils::etf_env$VTI)

## SPY ETF minute bars
# sym_bol <- "SPY"
# price_s <- Cl(HighFreq::SPY["2011"])["T09:31:00/T15:59:00"]

# re_turns <- rutils::diff_it(log(price_s))

cap_tion <- paste("VTI Strategy Using Rolling Kelly Weight")
# cap_tion <- paste("Contrarian Strategy for", sym_bol, "Using the Hampel Filter Over Prices")

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(cap_tion),
  
  fluidRow(
    # The Shiny App is re-calculated when the actionButton is clicked and the re_calculate variable is updated
    # column(width=12, 
    #        h4("Click the button 'Recalculate the Model' to re-calculate the Shiny App."),
    #        actionButton("re_calculate", "Recalculate the Model"))
  ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input end points interval
    # column(width=3, selectInput("inter_val", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback", min=100, max=500, value=200, step=1)),
    # Input look-back lag interval
    # column(width=3, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1)),
    # Input threshold interval
    # column(width=3, sliderInput("thresh_old", label="threshold", min=1.0, max=10.0, value=1.8, step=0.2))
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
    # Input the strategy coefficient: co_eff=1 for momentum, and co_eff=-1 for contrarian
    # column(width=3, selectInput("co_eff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    # column(width=3, numericInput("bid_offer", label="bid-offer:", value=0.001, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphOutput("dy_graph"), width=12)
  
)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {
  
  # Re-calculate the data and rerun the model
  da_ta <- reactive({
    # Get model parameters from input argument
    look_back <- input$look_back
    # lagg <- input$lagg
    # max_eigen <- isolate(input$max_eigen)
    # thresh_old <- input$thresh_old
    # look_lag <- isolate(input$look_lag
    # lamb_da <- isolate(input$lamb_da)
    # typ_e <- isolate(input$typ_e)
    # al_pha <- isolate(input$al_pha)
    # percen_tile <- isolate(input$percen_tile)
    # co_eff <- as.numeric(isolate(input$co_eff))
    # bid_offer <- isolate(input$bid_offer)
    # Model is re-calculated when the re_calculate variable is updated
    # input$re_calculate

    
    # look_back <- 11
    # half_window <- look_back %/% 2
    
    # Rerun the VTI model
    # var_rolling <- roll::roll_var(re_turns, width=look_back)
    # weight_s <- roll::roll_sum(re_turns, width=look_back)/look_back
    # weight_s <- weight_s/var_rolling
    # weight_s <- zoo::na.locf(weight_s, fromLast=TRUE)
    # weight_s <- drop(HighFreq::lag_vec(weight_s))
    # weight_s <- 10*weight_s/sum(abs(range(weight_s)))
    # weal_th <- cumprod(1 + weight_s*re_turns)

    # Rerun the VTI and IEF model
    var_rolling <- roll::roll_var(re_turns, width=look_back)
    weight_s <- roll::roll_sum(re_turns, width=look_back)/look_back
    weight_s <- weight_s/var_rolling
    weight_s <- zoo::na.locf(weight_s, fromLast=TRUE)
    # Calculate compounded wealth from returns
    weight_s <- HighFreq::lag_it(weight_s)
    # weight_s <- 10*weight_s/sum(abs(range(weight_s)))
    weight_s <- apply(weight_s, 2, function(x) 10*x/sum(abs(range(x))))
    weal_th <- cumprod(1 + rowSums(weight_s*re_turns))
    weal_th <- xts(weal_th, index(re_turns))
    
        
    # Calculate position_s and pnls from z-scores and ran_ge
    # position_s <- rep(NA_real_, NROW(price_s))
    # position_s[1] <- 0
    # thresh_old <- 3*mad(z_scores)
    # position_s <- ifelse(z_scores > thresh_old, -1, position_s)
    # position_s <- ifelse(z_scores < (-thresh_old), 1, position_s)
    # position_s <- ifelse(z_scores > thresh_old*mad_zscores, -1, position_s)
    # position_s <- ifelse(z_scores < (-thresh_old*mad_zscores), 1, position_s)
    # position_s <- na.locf(position_s)
    # positions_lag <- rutils::lag_it(position_s, lagg=lagg)
    # pnl_s <- cumsum(positions_lag*re_turns)
    pnl_s <- cbind(weal_th, cumsum(re_turns))
    colnames(pnl_s) <- c("Strategy", "Index")
    # pnl_s[rutils::calc_endpoints(pnl_s, inter_val="minutes")]
    # pnl_s[rutils::calc_endpoints(pnl_s, inter_val="hours")]
    pnl_s
  })  # end reactive code
  
  # Return to the output argument a dygraph plot with two y-axes
  output$dy_graph <- renderDygraph({
    col_names <- colnames(da_ta())
    dygraphs::dygraph(da_ta(), main=cap_tion) %>%
      dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
      dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
      dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue")
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
