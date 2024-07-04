##############################
# This is a shiny app for calculating the returns of 
# a static portfolio of ETFs defined by the weights input
# by the user.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

# Find ETFs with largest variance ratios
# pricev <- log(rutils::etfenv$prices)
retp <- rutils::etfenv$returns
symbolv <- sort(colnames(retp))
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "AIEQ", "QUAL", "USMV", "VLUE"))]
retp <- retp[, symbolv]
lagg <- 10

# Calculate the Hurst exponent from returns
calc_hurst_rets <- function(retp, lagg) {
  retp <- na.omit(retp)
  endp <- rutils::calc_endpoints(retp, interval=lagg)
  retc <- cumsum(retp)
  ranger <- sapply(seq_along(endp)[-1], function(it) {
    startp <- endp[it-1]
    endp <- endp[it]
    retp <- retp[startp:endp]
    retc <- retc[startp:endp]
    log((max(retc) - min(retc))/sd(retp))/log(endp-startp)
  })  # end sapply
  median(na.omit(ranger))
}  # end calc_hurst_rets

hurstv <- sapply(retp, calc_hurst_rets, lagg=lagg)
hurstv <- sort(hurstv, decreasing=TRUE)

# Select ETFs with largest variance ratios
ncols <- 4
symbolv <- names(hurstv)
namev <- symbolv[1:ncols]
retp <- rutils::etfenv$returns[, namev]
retp <- na.omit(retp)
datev <- zoo::index(retp)

objfun <- function(weightv, retp=retp) {
  # weightv <- c(1, weightv)
  weightv <- weightv/sqrt(sum(weightv^2))
  retp <- retp %*% weightv
  -sum(retp)/sd(retp)
}  # end objfun

# Perform portfolio optimization using optim
optiml <- optim(par=rep(1, ncols), 
                fn=objfun, 
                retp=retp,
                method="L-BFGS-B",
                upper=rep(100, ncols),
                lower=rep(-100, ncols))

# Portfolio weights
weightv <- optiml$par
weightv <- weightv/sqrt(sum(weightv^2))


# End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(paste("Portfolio Optimization for", ncols, "ETFs")),
  
  # Create single row with two slider inputs
  fluidRow(
    # Input weights
    column(width=2, sliderInput("weight1", label=paste0("Weight for ", namev[1], ":"),
                                min=-10, max=10, value=weightv[1], step=0.1)),
    column(width=2, sliderInput("weight2", label=paste0("Weight for ", namev[2], ":"),
                                min=-10, max=10, value=weightv[2], step=0.1)),
    column(width=2, sliderInput("weight3", label=paste0("Weight for ", namev[3], ":"),
                                min=-10, max=10, value=weightv[3], step=0.1)),
    column(width=2, sliderInput("weight4", label=paste0("Weight for ", namev[4], ":"),
                                min=-10, max=10, value=weightv[4], step=0.1))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Recalculate the data and rerun the model
  datav <- shiny::reactive({
    # get model parameters from input argument
    weight1 <- input$weight1
    weight2 <- input$weight2
    weight3 <- input$weight3
    weight4 <- input$weight4

    weights <- c(weight1, weight2, weight3, weight4)
    (retp %*% weights)
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    pnls <- datav()
    # Variance ratio
    # hurstexp <- HighFreq::calc_var_ag(pnls, lagg)/HighFreq::calc_var_ag(pnls)/lagg
    # Hurst
    hurstexp <- calc_hurst_rets(pnls, lagg)
    # Autocorrelation
    # pnls <- (pnls - mean(pnls))
    # hurstexp <- mean(pnls*rutils::lagit(pnls))/drop(var(pnls))
    pnls <- xts::xts(cumsum(pnls), datev)
    dygraphs::dygraph(pnls, main=paste("Static Portfolio for", ncols, "ETFs", 
                                        "Hurst =", round(hurstexp, 4)))
    # colnamev <- colnames(datav())
    # dygraphs::dygraph(datav(), main="ETF Portfolio Optimization") %>%
    #   dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
    #   dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
    #   dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="red") %>%
    #   dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="blue")
  })  # end output plot
  
  # output$dygraph <- dygraphs::renderDygraph({
  #   dygraph(datav(), main="Rolling Portfolio Optimization Strategy") %>%
  #     dySeries("strategy", label="strategy", strokeWidth=1, color="red")
  # })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
