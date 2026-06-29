##############################
# This is a shiny app for simulating a momentum strategy 
# for VTI, IEF, and DBC.
# The momentum weights are proportional to the Kelly 
# ratio calculated over a look-back period of in-sample 
# returns.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(rutils)
library(shiny)
library(dygraphs)

## Model and data setup

# Objective function equal to the Kelly ratio
objfun <- function(retp) {
  retp <- na.omit(retp)
  if (NROW(retp) > 3) {
    varv <- var(retp)
    if (varv > 0) mean(retp)/varv else 0
  } else 0
}  ##  end objfun

# Momentum strategy function
sim_momw <- function(retp, objfun, lookb=12, rebalf="months", volt=0.01, 
                     bidask=0.0, endd=rutils::calc_endpoints(retp, interval=rebalf), ...) {
  ##  Perform loop over end points
  npts <- NROW(endd)
  pnls <- lapply(1:(npts-1), function(tday) {
    ##  Select the in-sample returns
    startp <- endd[max(1, tday-lookb)]
    retis <- retp[startp:endd[tday], ]
    intos <- (endd[tday]+1):endd[tday+1]
    if (NROW(retis) < 4) return(numeric(NROW(intos)))
    ##  Calculate weights proportional to the performance
    perfstat <- sapply(retis, objfun)
    weightv <- perfstat
    ##  Calculate the in-sample portfolio returns
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    ##  Scale weights so in-sample pnl volatility is same as target
    weightv <- weightv*volt/sd(pnlis)
    ##  Calculate the out-of-sample momentum returns
    pnlos <- HighFreq::mult_mat(weightv, retp[intos])
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    return(drop(pnlos))
  })  ##  end lapply
  return(rutils::do_call(c, pnls))
}  ##  end sim_momw


captiont <- paste("Momentum Strategy for VTI, IEF, DBC ETFs")

# library(parallel)  ##  Load package parallel
# ncores <- detectCores() - 1

# Select the ETF symbols starting with X:
symbolv <- c("VTI", "IEF", "DBC")
# symbolv <- rutils::etfenv$symbolv
# symbolv <- symbolv[grep("^X", symbolv)]
# symbolv <- c("SPY", "TLT", symbolv)
nstocks <- NROW(symbolv)
# # Calculate the percentage stock returns
retp <- na.omit(rutils::etfenv$returns[, symbolv])
datev <- zoo::index(retp)
retm <- retp$SPY

# All Weather portfolio weights
weightaw <- c(0.30, 0.55, 0.15)
retaw <- retp %*% weightaw

# # Calculate a vector of weekly end points
endd <- rutils::calc_endpoints(retp, interval="months")
npts <- NROW(endd)
# 
# pnlc <- 0.0
# pnlema <- 0.0

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  
  titlePanel(captiont),

  fluidRow(
    # Input Look back interval
    column(width=2, sliderInput("lookb", label="Look-back", min=3, max=15, value=7, step=1)),
    # Input lambda decay parameter
    # column(width=2, sliderInput("lambdaf", label="lambda:", min=0.7, max=0.99, value=0.8, step=0.01)),
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
    lookb <- input$lookb
    # lambdaf <- input$lambdaf
    # coeff <- as.numeric(input$coeff)
    # lagg <- input$lagg
    # lambdaf <- input$lambdaf
    
    # Combine the PnLs into a single xts series
    pnls <- sim_momw(retp=retp, lookb=lookb, endd=endd, objfun=objfun)
    
    pnls <- cbind(retaw, pnls)
    colnames(pnls) <- c("AllWeather", "Momentum")
    pnls <- xts::xts(pnls, order.by=datev)
    
    # pnls$Momentum <- pnls$Momentum*sd(retp$SPY)/sd(pnls$Momentum)
    
    # Calculate the Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    globals$sharper <- round(sharper, 3)

    return(pnls)

  })  # end Recalculate the strategy
  

  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    cat("Plotting...\n")

    # Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Get Sharpe ratios
    sharper <- globals$sharper
    captiont <- paste(paste0(c("AllWeather SR=", "Momentum SR="), sharper, collapse=" / "))
    
    # Plot a dygraph of the momentum strategy
    endw <- rutils::calc_endpoints(pnls, interval="weeks")
    dygraphs::dygraph(cumsum(pnls)[endw], main=captiont) %>%
      dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
      dyLegend(show="always", width=300)

  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
