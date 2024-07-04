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

captiont <- paste("Autoregressive Strategy Using the Principal Components")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symboln", label="Symbol",
                                choices=rutils::etfenv$symbolv, selected="VTI")),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-ask spread
    column(width=2, numericInput("bidask", label="Bid-ask:", value=0.0000, step=0.0001))
  ),  # end fluidRow

  fluidRow(
    # Input the look-back interval
    column(width=2, sliderInput("maxback", label="Max Look-back", min=3, max=50, value=10, step=1)),
    # Input the response look-back interval
    column(width=2, sliderInput("numagg", label="Aggregation Interval", min=2, max=20, value=5, step=1)),
    # Input the look-back interval
    column(width=2, sliderInput("dimax", label="Max Eigen", min=2, max=20, value=3, step=1))
    # Input the trade lag
    # column(width=2, sliderInput("lagg", label="lagg", min=1, max=8, value=2, step=1))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot", width="100%", height="600px"), height=10, width=12)

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()

  # Load the data
  datav <- shiny::reactive({
    
    symboln <- input$symboln
    cat("Loading Data For ", symboln, "\n")
    
    ohlc <- get(symboln, rutils::etfenv)
    nrows <- NROW(ohlc)
    closep <- log(quantmod::Cl(ohlc))
    retp <- rutils::diffit(closep)
    retp <- retp/sd(retp)

    ## Divide the returns by the volume - use trading time (volume clock)
    # Need to scale the volume by the rolling average volume
    # volumes <- quantmod::Vo(ohlc)
    # volumes[volumes == 0] <- NA
    # volumes <- zoo::na.locf(volumes)
    # lookb <- 11
    # volume_rolling <- roll::roll_mean(volumes, width=lookb, min_obs=1)
    # volume_rolling <- zoo::na.locf(volume_rolling, fromLast=TRUE)
    # volumes <- volumes/volume_rolling
    
    # Divide  the returns by the volume - use trading time (volume clock)
    # rets <- ifelse(volumes > 0, retp/volumes, 0)
    # rets <- retp/volumes
    # rets <- rets/sd(rets)

    # Don't scale by the volume
    rets <- retp
    
    cbind(retp, rets)
    
  })  # end Load the data
  

  # Recalculate the predictor
  predv <- shiny::reactive({
    
    cat("Recalculating PCA Predictor For ", input$symboln, "\n")
    
    # Get model parameters from input argument
    maxback <- input$maxback
    # lagg <- input$lagg

    # Calculate cumulative returns
    retp <- datav()[, 1]
    rets <- datav()[, 2]
    dates <- zoo::index(retp)
    nrows <- NROW(retp)
    
    # respv <- rutils::lagit(predv[, maxback], lagg=(-maxback))
    numagg <- input$numagg
    respv <- sqrt(numagg)*roll::roll_mean(retp, numagg, min_obs=1)
    # respv[1:(numagg-1)] <- 0
    
    lookbs <- numagg*(1:maxback)
    # predv <- lapply(lookbs, function(x) sqrt(x)*roll::roll_mean(rets, x, min_obs=1))
    predv <- lapply(lookbs, rutils::lagit, input=respv)
    predv <- do.call(cbind, predv)
    # predv[1, ] <- 0
    # predv <- zoo::na.locf(predv)
    # sum(is.na(predv))
    predv <- cbind(respv, predv)
    
    respv <- rutils::lagit(respv, lagg=(-numagg))
    
    ## Define predictors as the principal components of predictor
    # Calculate covariance matrix of predictor
    # covmat <- cov(predv)
    # Calculate eigenvectors and eigenvalues
    # eigend <- eigen(covmat)
    
    # Define predictors as the principal components of predictor
    # eigenvec <- eigend$vectors
    # predv <- xts::xts(predv %*% eigend$vectors, order.by=dates)
    # colnames(predv) <- paste0("pc", 1:NCOL(predv))
    # round(cov(predv), 3)
    cbind(respv, rep(1, nrows), predv)

  })  # end Recalculate the predictor
  
  
  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    cat("Recalculating Strategy For ", input$symboln, "\n")
    
    # Get model parameters from input argument
    dimax <- input$dimax

    respv <- predv()[, 1]
    predv <- predv()[, -1]
    retp <- datav()[, 1]
    # dimax <- min(dimax, NCOL(predv))
    dimax <- NCOL(predv)
    
    nrows <- NROW(retp)
    insample <- 1:(nrows %/% 2)
    outsample <- (nrows %/% 2 + 1):nrows
    
    # Calculate in-sample fitted coefficients
    inverse <- MASS::ginv(predv[insample, 1:dimax])
    coeff <- drop(inverse %*% respv[insample])
    
    # Calculate out-sample forecasts of returns
    # fcasts <- drop(predv[outsample, 1:3] %*% coeff[1:3])
    fcasts <- drop(predv[outsample, 1:dimax] %*% coeff)
    # Lag the positions to trade in next period
    posv <- sign(rutils::lagit(fcasts))
    
    # Calculate indicator of flipping the positions
    indic <- rutils::diffit(posv)
    # Calculate number of trades
    values$ntrades <- sum(abs(indic) > 0)
    
    # Add buy/sell indicators for annotations
    longi <- (indic > 0)
    shorti <- (indic < 0)
    
    # Calculate strategy pnls
    retp <- retp[outsample]
    pnls <- posv*retp
    
    # Calculate transaction costs
    costs <- 0.5*input$bidask*abs(indic)
    pnls <- (pnls - costs)
    
    # Scale the pnls so they have same SD as returns
    pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
    
    # Bind together strategy pnls
    pnls <- cbind(retp, pnls)
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    
    # Bind with indicators
    pnls <- cumsum(pnls)
    if (values$ntrades > 1) {
      retsum <- cumsum(retp)
      pnls <- cbind(pnls, retsum[longi], retsum[shorti])
      colnames(pnls) <- c(paste(input$symboln, "Returns"), "Strategy", "Buy", "Sell")
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
    sharper <- values$sharper
    # Get number of trades
    ntrades <- values$ntrades
    
    captiont <- paste("Strategy for", input$symboln, "Returns Scaled by the Trading Volumes / \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
                      "Number of trades=", ntrades)
    
    # Plot with annotations
    add_annotations <- input$add_annotations
    
    if (add_annotations == "True") {
      dygraphs::dygraph(pnls, main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red") %>%
        dySeries(name=colnamev[3], axis="y", drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=colnamev[4], axis="y", drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == "False") {
      dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red")
    }  # end if
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
