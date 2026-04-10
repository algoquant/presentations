##############################
# This is a shiny app for simulating a contrarian strategy using
# the z-scores of the running regressions of the daily SPY returns 
# versus the VIX index returns.
#
# Go short SPY if the z-score is above the positive threshold.
# Go long SPY if the z-score is below the negative threshold.
#
# The z-score is equal to the regression residual divided by 
# the running standard deviation.
#
# The running regression is calculated using the function
# HighFreq::run_reg().
# 
# Just press the "Run App" button on upper right of this panel.
##############################


## Model and data setup
## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Get the VIX index prices
load(file="/Users/jerzy/Develop/data/vix/vix_data.RData")
vixohlc <- vixenv$vixohlc
datev <- zoo::index(vixohlc)
# Get the SPY prices
symboln <- "SPY"
rets <- na.omit(get(symboln, rutils::etfenv$returns))
rets <- rets[datev]
datev <- zoo::index(rets)
vixohlc <- vixohlc[datev]
vixp <- quantmod::Cl(vixohlc)
nrows <- NROW(vixohlc)

retvix <- rutils::diffit(vixp)/rutils::lagit(vixp)
retvix[1, ] <- 0.01
# highp <- quantmod::Hi(vixohlc)
# lowp <- quantmod::Lo(vixohlc)
# hilo <- highp - lowp
# hilo <- hilo + rutils::lagit(hilo, lagg=1)
# hilo <- vixp

captiont <- paste("EMA Regression Stock vs VIX Returns Strategy")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input lambda decay parameter
    column(width=2, sliderInput("lambdaf", label="Lambda",
                                min=0.1, max=0.99, value=0.14, step=0.01)),
    # Input threshold for z-scores
    column(width=2, sliderInput("threshv", label="Threshold:", 
                                min=0.5, max=9.0, value=1.0, step=0.1)),
  ),  # end fluidRow

  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()

  ## Calculate the z-scores
  zscores <- shiny::reactive({
    
    cat("Recalculating z-scores for ", symboln, "\n")
    lambdaf <- input$lambdaf
    # Calculate the EMA returns and volatility.
    # volma <- HighFreq::run_var(hilo, lambda=lambdaf)
    # vixma <- volma[, 1]
    # volma <- sqrt(volma[, 2])
    # zscores <- (hilo - vixma)/volma
    # minp <- HighFreq::run_min(hilo, lambda=lambdaf)
    # maxp <- HighFreq::run_max(hilo, lambda=lambdaf)
    # Create a list of regression parameters
    controll <- HighFreq::param_reg(residscale="scale")
    # Calculate the trailing z-scores
    zscores <- HighFreq::run_reg(respv=rets, predm=retvix, lambdaf=lambdaf, controll=controll)
    zscores[1, ] <- 0.0
    zscores <- zscores[, 2, drop=FALSE]
    # zscores <- zscores/sd(zscores)
    return(zscores)
    
  })  # end zscores
  
  
  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    cat("Recalculating strategy for ", symboln, "\n")
    # Get model parameters from input argument
    threshv <- input$threshv

    zscores <- zscores()
    # Calculate the positions and PnLs
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    # posv <- ifelse(hilo > (1.0 + threshv)*minp, -1, posv)
    # posv <- ifelse(hilo < (1.0 - threshv)*maxp, 1, posv)
    posv <- ifelse(zscores > threshv, -1, posv)
    posv <- ifelse(zscores < -threshv, 1, posv)
    posv <- zoo::na.locf(posv, na.rm=FALSE)
    posv <- rutils::lagit(posv, lagg=1)
    
    # Calculate indicator of flipped positions
    flipi <- rutils::diffit(posv)
    # Calculate number of trades
    values$ntrades <- sum(abs(flipi)>0)

    # Calculate the strategy PnLs
    pnls <- posv*rets
    # Scale the PnL volatility to that of SPY
    # pnls <- pnls*sd(rets[rets<0])/sd(pnls[pnls<0])
    # Bind together strategy pnls
    pnls <- cbind(rets, pnls)
    colnames(pnls) <- c(symboln, "Strategy")
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    pnls <- cumsum(pnls)
    pnls <- cbind(pnls, posv)
    colnames(pnls)[3] <- "Position"
    return(pnls)

  })  # end Recalculate the strategy
  

  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    # Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Get Sharpe ratios
    sharper <- values$sharper
    ntrades <- values$ntrades
    
    # Standard plot without shading
    captiont <- paste("EMA VIX Strategy", "\n",
                      paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
                      "Number of trades=", ntrades)
    # endw <- rutils::calc_endpoints(pnls, interval="weeks")
    # dygraphs::dygraph(pnls[endw], main=captiont) %>%
    #   dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
    #   dyLegend(show="always", width=300)

    # captiont <- paste("Strategy for", input$symbol, "/ \n", 
    #                   paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
    #                   "Number of trades=", ntrades)
    
    # Plot dygraph without shading
    dyplot <- dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
      dyLegend(show="always", width=200)
    
    # Plot dygraph with shading
    # Create colors for background shading
    # posv <- sign(pnls[, "Position"])
    # indic <- (rutils::diffit(posv) != 0) # Indices of crosses
    # crossd <- c(datev[indic], datev[nrows]) # Dates of crosses
    # shadev <- ifelse(posv[indic] == -1, "antiquewhite", "lightgreen")
    # Create dygraph object without plotting it
    # dyplot <- dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
    #   dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
    #   dyLegend(show="always", width=200)
    # Add shading to dygraph object
    # for (i in 1:NROW(shadev)) {
    #   dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
    # }  # end for
    
    # Plot the dygraph object
    return(dyplot)


  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
