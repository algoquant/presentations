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
pricev <- log(rutils::etfenv$prices)
symbolv <- sort(colnames(prices))
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "IEF"))]
pricev <- prices[, symbolv]
lagg <- 5
ratio_s <- sapply(pricev, function(x) {
  cat("x=", names(x), "\n")
  x <- na.omit(x)
  if (NROW(x) > 100)
    drop(HighFreq::calcvar_ag(x, lagg)/HighFreq::calcvar_ag(x)/lagg)
  else NULL
})  # end sapply
ratio_s <- sort(unlist(ratio_s), decreasing=TRUE)
symbolv <- names(ratio_s)

# Select ETFs with largest variance ratios
ncols <- 4
namesv <- names(ratio_s)[1:ncols]
retv <- rutils::etfenv$returns[, namesv]
retv <- na.omit(retv)


# Calculate Hurst exponent from returns
endp <- rutils::calc_endpoints(retv, interval=lagg)
calc_hurst_rets <- function(rets, endp) {
  cumsumv <- cumsum(rets)
  range_ratios <- sapply(seq_along(endp)[-1], function(it) {
    startpoint <- endp[it-1]
    endpoint <- endp[it]
    rets <- rets[startpoint:endpoint]
    cumsumv <- cumsumv[startpoint:endpoint]
    log((max(cumsumv) - min(cumsumv))/sd(rets))/log(endpoint-startpoint)
  })  # end sapply
  median(na.omit(range_ratios))
}  # end calc_hurst_rets

# End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(paste("Portfolio Optimization for", ncols, "ETFs")),
  
  # Create single row with two slider inputs
  fluidRow(
    # Input weights
    column(width=2, sliderInput("weight1", label=paste0("Weight for ", namesv[1], ":"),
                                min=-10, max=10, value=5, step=0.1)),
    column(width=2, sliderInput("weight2", label=paste0("Weight for ", namesv[2], ":"),
                                min=-10, max=10, value=0, step=0.1)),
    column(width=2, sliderInput("weight3", label=paste0("Weight for ", namesv[3], ":"),
                                min=-10, max=10, value=0, step=0.1)),
    column(width=2, sliderInput("weight4", label=paste0("Weight for ", namesv[4], ":"),
                                min=-10, max=10, value=0, step=0.1))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
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
    (retv %*% weights)
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    pnls <- datav()
    # Variance ratio
    # tre_nd <- HighFreq::calcvar_ag(pnls, lagg)/HighFreq::calcvar_ag(pnls)/lagg
    # Hurst
    tre_nd <- calc_hurst_rets(pnls, endp)
    # Autocorrelation
    # pnls <- (pnls - mean(pnls))
    # tre_nd <- mean(pnls*rutils::lagit(pnls))/drop(var(pnls))
    pnls <- xts::xts(cumsum(pnls), zoo::index(retv))
    dygraphs::dygraph(pnls, main=paste("Static Portfolio for", ncols, "ETFs", 
                                        "Trend indicator =", round(tre_nd, 4)))
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
