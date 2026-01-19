##############################
# This is a shiny app for simulating a momentum strategy using the 
# idiosyncratic returns of sector ETFs starting with X.
# The momentum strategy is combined with a crossover of EMA of the 
# cumulative PnLs.  If the cumulative PnL is below its EMA, then
# the momentum weights are reversed.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(rutils)
library(shiny)
library(dygraphs)

## Model and data setup

captiont <- paste("Momentum Strategy With Idiosyncratic Returns of Sector ETFs X*")

library(parallel)  ##  Load package parallel
ncores <- detectCores() - 1

# Select the ETF symbols starting with X:
symbolv <- rutils::etfenv$symbolv
symbolv <- symbolv[grep("^X", symbolv)]
symbolv <- c("SPY", "TLT", symbolv)
nstocks <- NROW(symbolv)
# Calculate the percentage stock returns
retp <- na.omit(rutils::etfenv$returns[, symbolv])
datev <- zoo::index(retp)
retm <- retp$SPY

# Calculate a vector of weekly end points
endd <- rutils::calc_endpoints(retp, interval="weeks")
npts <- NROW(endd)

pnlc <- 0.0
pnlema <- 0.0

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  
  titlePanel(captiont),

  fluidRow(
    # Input Look back interval
    column(width=2, sliderInput("lookb", label="Look-back", min=20, max=300, value=190, step=5)),
    # Input lambda decay parameter
    column(width=2, sliderInput("lambdaf", label="lambda:", min=0.7, max=0.99, value=0.8, step=0.01)),
  ),  # end fluidRow

  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")

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
    lambdaf <- input$lambdaf
    # coeff <- as.numeric(input$coeff)
    # lagg <- input$lagg
    # lambdaf <- input$lambdaf
    

    pnls <- mclapply(3:(npts-1), function(tday) {
    # pnls <- lapply(3:(npts-1), function(tday) {
      ##  Select the in-sample returns
      startp <- endd[max(1, tday-lookb)]
      endp <- endd[tday-1]
      retis <- retp[startp:endp, -1]
      datis <- zoo::index(retis)
      retmis <- retm[datis]
      varm <- drop(var(retmis))
      retmm <- mean(retmis)
      
      # Calculate the in-sample stock volatilities, betas, and alphas
      riskret <- lapply(retis, function(rets) {
        stdev <- sd(rets)
        betac <- drop(cov(rets, retmis)/varm)
        resid <- rets - betac*retmis
        alphac <- mean(rets) - betac*retmm
        c(alpha=alphac, beta=betac, stdev=stdev, ivar=var(resid))
      })  ## end lapply
      riskret <- do.call(rbind, riskret)
      
      # Extract the risk vectors
      alphav <- riskret[, "alpha"]
      betav <- riskret[, "beta"]
      volv <- riskret[, "stdev"]
      ivarv <- riskret[, "ivar"]
      # Calculate the momentum weights
      weightv <- alphav/ivarv
      weightv[is.na(weightv)] <- 0
      ##  Select the out-of-sample returns
      retos <- retp[(endd[tday]+1):endd[tday+1], -1]
      datos <- zoo::index(retos)
      # Calculate the out-of-sample stock alphas
      retmos <- retm[datos]
      alphav <- lapply(colnames(retp[, -1]), function(symb) {
        retos[, symb] - retmos*betav[symb]
      }) ## end lapply
      alphav <- do.call(cbind, alphav)
      alphav[is.na(alphav)] <- 0
      # Calculate the out-of-sample pnls of low and high alpha stocks
      pnlos <- drop(alphav %*% weightv)
      # Calculate the equal-weighted portfolio pnls
      retew <- rowMeans(retos, na.rm=TRUE)
      # Scale the PnL volatility to that of the equal-weighted portfolio
      pnlos <- pnlos*sd(retew)/sd(pnlos)
      pnlsc <- sign(pnlc - pnlema)*pnlos
      pnlc <<- pnlc + mean(pnlos)
      pnlema <<- lambdaf*pnlema + (1-lambdaf)*pnlc
      return(xts(cbind(retew, pnlsc), order.by=datos))
      
    # })  ## end mclapply
    }, mc.cores=ncores)  ## end mclapply
  
    # Combine the PnLs into a single xts series
    pnls <- rutils::do_call(rbind, pnls)
    colnames(pnls) <- c("EqualWeight", "Momentum")
    pnls$Momentum <- pnls$Momentum*sd(retp$SPY)/sd(pnls$Momentum)
    
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
    captiont <- paste("Idiosyncratic ETF Momentum Strategy", "/ \n", 
                      paste0(c("EqWeight SR=", "Strategy SR="), sharper, collapse=" / "))
    
    # Plot a dygraph of the momentum strategy
    endw <- rutils::calc_endpoints(pnls, interval="weeks")
    dygraphs::dygraph(cumsum(pnls)[endw], main=captiont) %>%
      dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
      dyLegend(show="always", width=300)

  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
