##############################
# This is a shiny app for simulating a momentum strategy 
# for ETFs.
# The momentum weights are proportional to the Kelly 
# ratio calculated using the EMA return and variance.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(rutils)
library(shiny)
library(dygraphs)

## Model and data setup

# library(parallel)  ##  Load package parallel
# ncores <- detectCores() - 1

# Select the ETF symbols
symbolv <- c("QQQ", "IEF", "GLD", "USO")
# symbolv <- rutils::etfenv$symbolv
# symbolv <- symbolv[grep("^X", symbolv)]
# symbolv <- c("SPY", "TLT", symbolv)
nstocks <- NROW(symbolv)
# # Calculate the percentage stock returns
retp <- na.omit(rutils::etfenv$returns[, symbolv])
datev <- zoo::index(retp)
retm <- retp$SPY

# All Weather portfolio weights
weightaw <- c(0.30, 0.55, 0.05, 0.1)
# weightaw <- calc_sharpe(retp, "calmar")
weightaw <- weightaw/sqrt(sum(weightaw^2))
retaw <- retp %*% weightaw
retaw <- xts::xts(retaw, order.by=datev)

captiont <- paste("EMA Momentum Strategy for ", paste(symbolv, collapse=", "))

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  
  titlePanel(captiont),

  fluidRow(
    ##  Input lambda decay parameter
    column(width=2, sliderInput("lambdaf", label="Decay factor", min=0.9, max=0.999, value=0.992, step=0.001)),
  ),  # end fluidRow

  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="700px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  ## Create an empty list of reactive values.
  globals <- reactiveValues()

  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    # symboln <- input$symboln
    # cat("Recalculating strategy for ", symboln, "\n")
    cat("Recalculating strategy...\n")
    # Get model parameters from input argument
    lambdaf <- input$lambdaf
    # coeff <- as.numeric(input$coeff)
    # lagg <- input$lagg

    # Calculate the EMA prices and variance
    varp <- HighFreq::run_var(retp, lambdaf=lambdaf)
    retma <- varp[, 1:nstocks]
    varp <- (varp[, (nstocks + 1):(2*nstocks)])
    varp[1:5, ] <- 1
    weightv <- retma/varp
    weightv <- weightv/sqrt(rowSums(weightv^2))
    # weightv <- weightv/(rowSums(abs(weightv)))
    weightv <- rutils::lagit(weightv, 1)
    # Combine the PnLs into a single xts series
    pnls <- rowSums(retp * weightv)
    pnls <- cbind(retaw, pnls, 0.5*(retaw+pnls))
    colnames(pnls) <- c("AllWeather", "Momentum", "Combined")

    # Calculate the performance ratios
    # ratiov <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    ratiov <- HighFreq::calc_sharpe(pnls, "calmar")
    globals$ratiov <- round(ratiov, 3)

    return(pnls)

  })  # end Recalculate the strategy
  

  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    cat("Plotting...\n")

    # Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Get performance ratios
    ratiov <- globals$ratiov
    captiont <- paste("Calmar ", paste0(paste0(colnamev, "=", ratiov), collapse=" / "))
    
    # Plot a dygraph of the momentum strategy
    endw <- rutils::calc_endpoints(pnls, interval="weeks")
    dygraphs::dygraph(cumsum(pnls)[endw], main=captiont) %>%
      dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
      dyLegend(show="always", width=300)

  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
