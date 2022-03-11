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

cap_tion <- paste("Autoregressive Strategy Using the Principal Components")

## End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel(cap_tion),

  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol",
                                choices=rutils::etfenv$symbolv, selected="VTI")),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-offer spread
    column(width=2, numericInput("bid_offer", label="Bid-offer:", value=0.0000, step=0.0001))
  ),  # end fluidRow

  fluidRow(
    # Input the look-back interval
    column(width=2, sliderInput("max_back", label="Max Look-back", min=3, max=50, value=10, step=1)),
    # Input the response look-back interval
    column(width=2, sliderInput("numagg", label="Aggregation Interval", min=2, max=20, value=5, step=1)),
    # Input the look-back interval
    column(width=2, sliderInput("max_eigen", label="Max Eigen", min=2, max=20, value=3, step=1))
    # Input the trade lag
    # column(width=2, sliderInput("lagg", label="lagg", min=1, max=8, value=2, step=1))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot", width="100%", height="600px"), height=10, width=12)

)  # end fluidPage interface


## Define the server code
servfunc <- function(input, output) {

  # Create an empty list of reactive values.
  value_s <- reactiveValues()

  # Load the data
  datav <- reactive({
    
    symbol <- input$symbol
    cat("Loading Data For ", symbol, "\n")
    
    ohlc <- get(symbol, rutils::etfenv)
   .n_rows <- NROW(ohlc)
    closep <- log(quantmod::Cl(ohlc))
    returns <- rutils::diffit(closep)
    returns <- returns/sd(returns)

    ## Divide the returns by the volume - use trading time (volume clock)
    # Need to scale the volume by the rolling average volume
    # volumes <- quantmod::Vo(ohlc)
    # volumes[volumes == 0] <- NA
    # volumes <- zoo::na.locf(volumes)
    # look_back <- 11
    # volume_rolling <- roll::roll_mean(volumes, width=look_back, min_obs=1)
    # volume_rolling <- zoo::na.locf(volume_rolling, fromLast=TRUE)
    # volumes <- volumes/volume_rolling
    
    # Divide  the returns by the volume - use trading time (volume clock)
    # rets_scaled <- ifelse(volumes > 0, returns/volumes, 0)
    # rets_scaled <- returns/volumes
    # rets_scaled <- rets_scaled/sd(rets_scaled)

    # Don't scale by the volume
    rets_scaled <- returns
    
    cbind(returns, rets_scaled)
    
  })  # end Load the data
  

  # Recalculate the design
  design <- reactive({
    
    cat("Recalculating PCA Predictor For ", input$symbol, "\n")
    
    # Get model parameters from input argument
    max_back <- input$max_back
    # lagg <- input$lagg

    # Calculate cumulative returns
    returns <- datav()[, 1]
    rets_scaled <- datav()[, 2]
    dates <- zoo::index(returns)
   .n_rows <- NROW(returns)
    
    # response <- rutils::lagit(design[, max_back], lagg=(-max_back))
    numagg <- input$numagg
    response <- sqrt(numagg)*roll::roll_mean(returns, numagg, min_obs=1)
    # response[1:(numagg-1)] <- 0
    
    look_backs <- numagg*(1:max_back)
    # design <- lapply(look_backs, function(x) sqrt(x)*roll::roll_mean(rets_scaled, x, min_obs=1))
    design <- lapply(look_backs, rutils::lagit, input=response)
    design <- do.call(cbind, design)
    # design[1, ] <- 0
    # design <- zoo::na.locf(design)
    # sum(is.na(design))
    design <- cbind(response, design)
    
    response <- rutils::lagit(response, lagg=(-numagg))
    
    ## Define predictors as the principal components of design
    # Calculate covariance matrix of design
    # covmat <- cov(design)
    # Calculate eigenvectors and eigenvalues
    # eigend <- eigen(covmat)
    
    # Define predictors as the principal components of design
    # eigen_vec <- eigend$vectors
    # design <- xts::xts(design %*% eigend$vectors, order.by=dates)
    # colnames(design) <- paste0("pc", 1:NCOL(design))
    # round(cov(design), 3)
    cbind(response, rep(1,.n_rows), design)

  })  # end Recalculate the design
  
  
  # Recalculate the strategy
  pnls <- reactive({
    
    cat("Recalculating Strategy For ", input$symbol, "\n")
    
    # Get model parameters from input argument
    max_eigen <- input$max_eigen

    response <- design()[, 1]
    predictor <- design()[, -1]
    returns <- datav()[, 1]
    # max_eigen <- min(max_eigen, NCOL(predictor))
    max_eigen <- NCOL(predictor)
    
   .n_rows <- NROW(returns)
    in_sample <- 1:.n_rows %/% 2)
    out_sample <- .n_rows %/% 2 + 1).n_rows
    
    # Calculate in-sample fitted coefficients
    inverse <- MASS::ginv(predictor[in_sample, 1:max_eigen])
    coeff_fit <- drop(inverse %*% response[in_sample])
    
    # Calculate out-sample forecasts of returns
    # forecasts <- drop(predictor[out_sample, 1:3] %*% coeff_fit[1:3])
    forecasts <- drop(predictor[out_sample, 1:max_eigen] %*% coeff_fit)
    # Lag the positions to trade in next period
    position_s <- sign(rutils::lagit(forecasts))
    
    # Calculate indicator of flipping the positions
    indic <- rutils::diffit(position_s)
    # Calculate number of trades
    value_s$n_trades <- sum(abs(indic) > 0)
    
    # Add buy/sell indicators for annotations
    indic_buy <- (indic > 0)
    indic_sell <- (indic < 0)
    
    # Calculate strategy pnls
    returns <- returns[out_sample]
    pnls <- position_s*returns
    
    # Calculate transaction costs
    costs <- 0.5*input$bid_offer*abs(indic)
    pnls <- (pnls - costs)
    
    # Scale the pnls so they have same SD as returns
    pnls <- pnls*sd(returns[returns<0])/sd(pnls[pnls<0])
    
    # Bind together strategy pnls
    pnls <- cbind(returns, pnls)
    
    # Calculate Sharpe ratios
    sharp_e <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    value_s$sharp_e <- round(sharp_e, 3)
    
    # Bind with indicators
    pnls <- cumsum(pnls)
    if (value_s$n_trades > 1) {
      cum_rets <- cumsum(returns)
      pnls <- cbind(pnls, cum_rets[indic_buy], cum_rets[indic_sell])
      colnames(pnls) <- c(paste(input$symbol, "Returns"), "Strategy", "Buy", "Sell")
    }  # end if
    
    pnls
    
  })  # end Recalculate the strategy
  
  
  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    # Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Get Sharpe ratios
    sharp_e <- value_s$sharp_e
    # Get number of trades
    n_trades <- value_s$n_trades
    
    cap_tion <- paste("Strategy for", input$symbol, "Returns Scaled by the Trading Volumes / \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharp_e, collapse=" / "), "/ \n",
                      "Number of trades=", n_trades)
    
    # Plot with annotations
    add_annotations <- input$add_annotations
    
    if (add_annotations == "True") {
      dygraphs::dygraph(pnls, main=cap_tion) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red") %>%
        dySeries(name=colnamev[3], axis="y", label=colnamev[3], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=colnamev[4], axis="y", label=colnamev[4], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == "False") {
      dygraphs::dygraph(pnls[, 1:2], main=cap_tion) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red")
    }  # end if
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfunc)
