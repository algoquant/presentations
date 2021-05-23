##############################
# This is a shiny app for simulating an autoregressive
# strategy using the principal components of average 
# returns as predictors.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

cap_tion <- paste("EWMA Moving Average Crossover Strategy")

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(cap_tion),

  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("sym_bol", label="Symbol",
                                choices=rutils::etf_env$sym_bols, selected="VTI")),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-offer spread
    column(width=2, numericInput("bid_offer", label="Bid-offer:", value=0.0000, step=0.0001))
  ),  # end fluidRow

  fluidRow(
    # Input the look-back interval
    column(width=2, sliderInput("max_back", label="Max Look-back", min=3, max=50, value=10, step=1)),
    # Input the look-back interval
    column(width=2, sliderInput("max_eigen", label="Max Eigen", min=2, max=6, value=3, step=1))
    # Input the trade lag
    # column(width=2, sliderInput("lagg", label="lagg", min=1, max=8, value=2, step=1))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph", width="100%", height="550px"), height=10, width=11)

)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {

  # Create an empty list of reactive values.
  value_s <- reactiveValues()

  # Load the data
  da_ta <- reactive({
    
    sym_bol <- input$sym_bol
    cat("Loading Data For ", sym_bol, "\n")
    
    oh_lc <- get(sym_bol, rutils::etf_env)
    n_rows <- NROW(oh_lc)
    clos_e <- log(quantmod::Cl(oh_lc))
    re_turns <- rutils::diff_it(clos_e)
    # cum_rets <- cumsum(re_turns)
    vol_ume <- quantmod::Vo(oh_lc)
    re_turns <- re_turns/sd(re_turns)
    # vol_ume <- quantmod::Vo(oh_lc)
    
    # Need to scale the volume by the rolling average volume
    look_back <- 11
    volume_rolling <- roll::roll_mean(vol_ume, width=look_back)
    volume_rolling <- zoo::na.locf(volume_rolling, fromLast=TRUE)
    vol_ume <- vol_ume/volume_rolling
    
    # Divide  the returns by the volume - use trading time (volume clock)
    # rets_scaled <- ifelse(vol_ume > 0, re_turns/vol_ume, 0)
    rets_scaled <- re_turns/vol_ume
    rets_scaled <- rets_scaled/sd(rets_scaled)
    
    cbind(re_turns, rets_scaled)
    
  })  # end Load the data
  

  # Recalculate the strategy
  predic_tor <- reactive({
    
    cat("Recalculating PCA Predictor For ", input$sym_bol, "\n")
    
    # Get model parameters from input argument
    max_back <- input$max_back
    # lagg <- input$lagg

    # Calculate cumulative returns
    re_turns <- da_ta()[, 1]
    rets_scaled <- da_ta()[, 2]
    date_s <- zoo::index(re_turns)
    n_rows <- NROW(re_turns)
    

    look_backs <- 2:max_back
    de_sign <- lapply(look_backs, function(x) sqrt(x)*roll::roll_mean(rets_scaled, x))
    de_sign <- do.call(cbind, de_sign)
    de_sign[1, ] <- 0
    de_sign <- zoo::na.locf(de_sign)
    # sum(is.na(de_sign))
    de_sign <- cbind(rets_scaled, de_sign)
    
    # Calculate covariance matrix of de_sign
    cov_mat <- cov(de_sign)
    # Calculate eigenvectors and eigenvalues
    ei_gen <- eigen(cov_mat)
    
    # Define predictors as the principal components of de_sign
    # eigen_vec <- ei_gen$vectors
    predic_tor <- xts::xts(de_sign %*% ei_gen$vectors, order.by=date_s)
    # colnames(predic_tor) <- paste0("pc", 1:NCOL(predic_tor))
    # round(cov(predic_tor), 3)
    predic_tor <- rutils::lag_it(predic_tor)
    predic_tor <- cbind(rep(1, n_rows), predic_tor)
    predic_tor

  })  # end Recalculate the strategy
  
  
  # Recalculate the strategy
  pnl_s <- reactive({
    
    cat("Recalculating Strategy For ", input$sym_bol, "\n")
    
    # Get model parameters from input argument
    max_eigen <- input$max_eigen

    predic_tor <- predic_tor()
    re_turns <- da_ta()[, 1]
    
    n_rows <- NROW(re_turns)
    in_sample <- 1:(n_rows %/% 4)
    out_sample <- (n_rows %/% 4 + 1):n_rows
    
    # res_ponse <- rutils::lag_it(de_sign[, max_back], lagg=(-max_back))
    res_ponse <- sqrt(max_eigen)*roll::roll_mean(re_turns, max_eigen)
    res_ponse[1:(max_eigen-1)] <- 0
    res_ponse <- rutils::lag_it(res_ponse, lagg=(-max_eigen))
    
    # Calculate in-sample fitted coefficients
    in_verse <- MASS::ginv(predic_tor[in_sample, 1:max_eigen])
    coeff_fit <- drop(in_verse %*% res_ponse[in_sample])
    
    # Calculate out-sample forecasts of re_turns
    # forecast_s <- drop(predic_tor[out_sample, 1:3] %*% coeff_fit[1:3])
    forecast_s <- drop(predic_tor[out_sample, 1:max_eigen] %*% coeff_fit)
    # Lag the positions to trade in next period
    position_s <- sign(rutils::lag_it(forecast_s))
    
    # Calculate indicator of flipping the positions
    in_dic <- rutils::diff_it(position_s)
    # Calculate number of trades
    value_s$n_trades <- sum(abs(in_dic)>0)
    
    # Add buy/sell indicators for annotations
    indic_buy <- (in_dic > 0)
    indic_sell <- (in_dic < 0)
    
    # Calculate strategy pnl_s
    re_turns <- re_turns[out_sample]
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
    cum_rets <- cumsum(re_turns)
    pnl_s <- cbind(pnl_s, cum_rets[indic_buy], cum_rets[indic_sell])
    colnames(pnl_s) <- c(paste(input$sym_bol, "Returns"), "Strategy", "Buy", "Sell")
    
    pnl_s
    
  })  # end Recalculate the strategy
  
  
  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph({
    
    # Get the pnl_s
    pnl_s <- pnl_s()
    col_names <- colnames(pnl_s)
    
    # Get Sharpe ratios
    sharp_e <- value_s$sharp_e
    # Get number of trades
    n_trades <- value_s$n_trades
    
    cap_tion <- paste("Strategy for", input$sym_bol, "Returns Scaled by the Trading Volumes / \n", 
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
