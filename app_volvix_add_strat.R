##############################
# This is a shiny app for simulating a contrarian strategy 
# using z-scores of regressions for VXX and SVXY prices versus 
# the rolling VTI volatility.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(rutils)
library(shiny)
library(dygraphs)

## Model and data setup

# Load the VIX data
svx_y <- log(quantmod::Cl(get("SVXY", rutils::etf_env)))
date_s <- zoo::index(svx_y)
vx_x <- log(quantmod::Cl(get("VXX", rutils::etf_env)))
vx_x <- vx_x[date_s]
vt_i <- log(get("VTI", rutils::etf_env))
vt_i <- vt_i[date_s]
vti_close <- quantmod::Cl(vt_i)

cap_tion <- paste("Regression Z-score of VXX and SVXY Prices Versus VTI Volatility")

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(cap_tion),

  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("sym_bol", label="Symbol to Trade",
                                choices=rutils::etf_env$sym_bols, selected="VTI")),
    # Input VIX symbol
    # column(width=2, selectInput("symbol_vix", label="Symbol VIX",
    #                             choices=c("VXX", "SVXY"), selected="VXX")),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-offer spread
    column(width=2, numericInput("bid_offer", label="Bid-offer:", value=0.0000, step=0.0001))
  ),  # end fluidRow

  fluidRow(
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Look-back", min=2, max=100, value=50, step=1)),
    # Input threshold interval
    column(width=2, sliderInput("thresh_old", label="Threshold", min=0.01, max=0.5, value=0.1, step=0.01)),
    # Input the strategy coefficient: co_eff=1 for momentum, and co_eff=-1 for contrarian
    column(width=2, selectInput("co_eff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # column(width=2, sliderInput("look_back", label="look_back:", min=1, max=21, value=5, step=1)),
    # column(width=2, sliderInput("slow_back", label="slow_back:", min=11, max=251, value=151, step=1)),
    # Input the trade lag
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=8, value=1, step=1))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph", width="100%", height="600px"), height=10, width=12)

)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {

  # Create an empty list of reactive values.
  value_s <- reactiveValues()

  # Load the data
  oh_lc <- reactive({
    
    sym_bol <- input$sym_bol
    cat("Loading data for ", sym_bol, "\n")
    
    get(sym_bol, rutils::etf_env)[date_s]

  })  # end Load the data
  

  # Recalculate the strategy
  pnl_s <- reactive({
    
    sym_bol <- input$sym_bol
    cat("Recalculating strategy for ", sym_bol, "\n")
    # Get model parameters from input argument
    look_back <- input$look_back
    co_eff <- as.numeric(input$co_eff)
    lagg <- input$lagg

    # Calculate cumulative returns
    oh_lc <- oh_lc()
    clos_e <- log(quantmod::Cl(oh_lc))
    re_turns <- rutils::diff_it(clos_e)
    # re_turns <- re_turns/sd(re_turns)
    cum_rets <- cumsum(re_turns)
    n_rows <- NROW(re_turns)

    # Calculate rolling volatility
    vari_ance <- HighFreq::roll_var_ohlc(oh_lc=vt_i, look_back=look_back, scal_e=FALSE)

    # Calculate trailing SVXY z-scores
    de_sign <- cbind(sqrt(vari_ance), vx_x, vti_close)
    res_ponse <- svx_y
    svxy_scores <- drop(HighFreq::roll_zscores(res_ponse=res_ponse, de_sign=de_sign, look_back=look_back))
    svxy_scores[1:look_back] <- 0
    svxy_scores[is.infinite(svxy_scores)] <- 0
    svxy_scores[is.na(svxy_scores)] <- 0

    # Calculate trailing VXX z-scores
    de_sign <- cbind(sqrt(vari_ance), svx_y, vti_close)
    res_ponse <- vx_x
    vxx_scores <- drop(HighFreq::roll_zscores(res_ponse=res_ponse, de_sign=de_sign, look_back=look_back))
    vxx_scores[1:look_back] <- 0
    vxx_scores[is.infinite(vxx_scores)] <- 0
    vxx_scores[is.na(vxx_scores)] <- 0
    
    z_scores <- (svxy_scores + vxx_scores)/sqrt(look_back)
    
    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the in_dic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # position_s <- ifelse(in_dic == indic_lag, in_dic, position_s)
    
    in_dic <- rep(NA_integer_, n_rows)
    in_dic[1] <- 0
    # Flip position if the scaled returns exceed thresh_old
    thresh_old <- input$thresh_old
    in_dic[z_scores > thresh_old] <- co_eff
    in_dic[z_scores < (-thresh_old)] <- (-co_eff)
    in_dic <- zoo::na.locf(in_dic, na.rm=FALSE)

    
    indic_sum <- HighFreq::roll_vec(se_ries=in_dic, look_back=lagg)
    indic_sum[1:lagg] <- 0
    position_s <- rep(NA_integer_, n_rows)
    position_s[1] <- 0
    position_s <- ifelse(indic_sum == lagg, 1, position_s)
    position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
    position_s <- zoo::na.locf(position_s, na.rm=FALSE)
    position_s[1:lagg] <- 0

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
    pnl_s <- position_s*re_turns
    
    # Calculate transaction costs
    cost_s <- 0.5*input$bid_offer*abs(in_dic)
    pnl_s <- (pnl_s - cost_s)

    # Scale the pnl_s so they have same SD as re_turns
    pnl_s <- pnl_s*sd(re_turns[re_turns<0])/sd(pnl_s[pnl_s<0])
    
    # Bind together strategy pnl_s
    pnl_s <- cbind(re_turns, pnl_s)
    
    # Calculate Sharpe ratios
    sharp_e <- sqrt(252)*sapply(pnl_s, function(x) mean(x)/sd(x[x<0]))
    value_s$sharp_e <- round(sharp_e, 3)

    # Bind with indicators
    pnl_s <- cumsum(pnl_s)
    pnl_s <- cbind(pnl_s, cum_rets[indic_buy], cum_rets[indic_sell])
    colnames(pnl_s) <- c(paste(input$sym_bol, "Returns"), "Strategy", "Buy", "Sell")

    pnl_s

  })  # end Recalculate the strategy
  

  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph({
    
    cat("Plotting for ", input$sym_bol, "\n")
    
    # Get the pnl_s
    pnl_s <- pnl_s()
    col_names <- colnames(pnl_s)
    
    # Get Sharpe ratios
    sharp_e <- value_s$sharp_e
    # Get number of trades
    n_trades <- value_s$n_trades
    
    cap_tion <- paste("Strategy for", input$sym_bol, "Regression Z-score / \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharp_e, collapse=" / "), "/ \n",
                      "Number of trades=", n_trades)
    
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
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
