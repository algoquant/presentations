##############################
# This is a shiny app for simulating an EMA volatility 
# threshold strategy.
# Just press the "Run App" button on upper right of this panel.
#
# When the volatility is above the fixed threshold, 
# go short the market (SPY).  
# When the volatility is below the threshold, go long 
# the market.
# The position is equal to the difference between the
# EMA volatility minus the threshold value.
# Doesn't work well when prices gradually decline.
# 
##############################


## Model and data setup
## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Get the percentage SPY returns
symbolv <- "SPY"
retp <- na.omit(get(symbolv, rutils::etfenv$returns))
datev <- zoo::index(retp)
nrows <- NROW(retp)


captiont <- paste("EMA Volatility Crossover Strategy for", symbolv)

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input look-back interval
    column(width=2, sliderInput("lambdaf", label="Lambda",
                                min=0.1, max=0.4, value=0.13, step=0.01)),
    # Input look-back intervals
    column(width=2, sliderInput("thresholdv", label="Threshold:", 
                                min=0.001, max=0.02, value=0.007, step=0.001)),
  ),  # end fluidRow

  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()

  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    cat("Recalculating strategy for ", symbolv, "\n")
    # Get model parameters from input argument
    lambdaf <- input$lambdaf
    thresholdv <- input$thresholdv

    # Calculate the EMA returns and volatility.
    volma <- HighFreq::run_var(retp, lambda=lambdaf)
    retma <- volma[, 1]
    volma <- sqrt(volma[, 2])
    # Calculate the downside volatility
    # retn <- retp
    # retn[retn > 0] <- 0
    # volma <- HighFreq::run_mean(retn^2, lambda=lambdaf)
    # volma <- sqrt(volma)
    
    # Calculate the positions and PnLs
    posv <- -(rutils::lagit((volma - thresholdv), lagg=1))
    pnls <- retp*posv
    # Scale the PnL volatility to that of SPY
    pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
    # Bind together strategy pnls
    pnls <- cbind(retp, pnls)
    colnames(pnls) <- c(symbolv, "Strategy")
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    pnls <- cumsum(pnls)
    pnls <- cbind(pnls, posv)
    colnames(pnls)[3] <- "Position"
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

    # Standard plot without shading
    captiont <- paste("EMA Volatility Strategy", "\n",
                      paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "))
    # endw <- rutils::calc_endpoints(pnls, interval="weeks")
    # dygraphs::dygraph(pnls[endw], main=captiont) %>%
    #   dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
    #   dyLegend(show="always", width=300)

    # captiont <- paste("Strategy for", input$symbol, "/ \n", 
    #                   paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
    #                   "Number of trades=", ntrades)
    
    # Plot dygraph with shading
    # Create colors for background shading
    posv <- sign(pnls[, "Position"])
    indic <- (rutils::diffit(posv) != 0) # Indices of crosses
    crossd <- c(datev[indic], datev[nrows]) # Dates of crosses
    shadev <- ifelse(posv[indic] == -1, "antiquewhite", "lightgreen")
    # Create dygraph object without plotting it
    dyplot <- dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
      dyLegend(show="always", width=200)
    # Add shading to dygraph object
    for (i in 1:NROW(shadev)) {
      dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
    }  # end for
    # Plot the dygraph object
    dyplot




  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
