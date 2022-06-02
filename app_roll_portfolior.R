##############################
# This is a shiny app for backtesting a rolling portfolio 
# optimization strategy, which produces an interactive 
# dygraphs plot.
# It only uses R code, not C++.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# load packages
library(shiny)
library(dygraphs)
library(rutils)

# Model and data setup
# source the model function
source("/Users/jerzy/Develop/lecture_slides/scripts/back_test.R")
# dimax <- 2
symbolv <- colnames(rutils::etfenv$returns)
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV"))]
nstocks <- NROW(symbolv)
rets <- rutils::etfenv$returns[, symbolv]
rets[1, is.na(rets[1, ])] <- 0
rets <- zoo::na.locf(rets, na.rm=FALSE)
rets <- na.omit(rets)
dates <- zoo::index(rets)
riskf <- 0.03/260
excess <- (rets - riskf)
# Calculate returns on equal weight portfolio
indeks <- xts::xts(rowMeans(rets), dates)

# End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel(paste0("Rolling Portfolio Optimization Strategy for ", nstocks, " ETFs")),
  
  # create single row with two slider inputs
  fluidRow(
    # Input number of eigenvalues for regularized matrix inverse
    column(width=3, sliderInput("dimax", label="Number of eigenvalues:",
                                min=2, max=(nstocks %/% 2), value=3, step=1)),
    # Input end points interval
    column(width=3, selectInput("interval", label="End points Interval",
                choices=c("weeks", "months", "years"), selected="months")),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback interval:",
                                min=1, max=30, value=12, step=1)),
    # Input the shrinkage intensity
    column(width=3, sliderInput("alpha", label="Shrinkage intensity:",
                                min=0.01, max=0.99, value=0.01, step=0.05))
  ),  # end fluidRow
  
  # create output plot panel
  dygraphs::dygraphOutput("dyplot", width="95%", height="600px")
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Recalculate the data and rerun the model
  pnls <- shiny::reactive({
    # get model parameters from input argument
    interval <- input$interval
    dimax <- input$dimax
    look_back <- input$look_back
    alpha <- input$alpha
    
    # Define end points
    endp <- rutils::calc_endpoints(rets, interval=interval)
    # endp <- ifelse(endp<(nstocks+1), nstocks+1, endp)
    endp <- endp[endp > (nstocks+1)]
    # npts <- NROW(endp)
    # Define startp
    # startp <- c(rep_len(1, look_back-1), endp[1:(npts-look_back+1)])
    # Rerun the model
    # pnls <- roll_portf(excess, rets, startp, endp, alpha, dimax)
    pnls <- roll_portf(excess=excess, returns=rets, look_back=look_back, endp=endp, alpha=alpha, dimax=dimax)
    # pnls <- sd(rutils::diffit(indeks))*pnls/sd(rutils::diffit(pnls))
    pnls <- rbind(indeks[paste0("/", start(pnls)-1)], pnls*sd(indeks)/sd(pnls))
    pnls <- cbind(indeks, pnls)
    colnames(pnls) <- c("Index", "Strategy")
    pnls
  })  # end reactive code
  
  # return to output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    pnls <- pnls()
    colnamev <- colnames(pnls)
    endp <- rutils::calc_endpoints(pnls, interval=input$interval)
    
    # Calculate the Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 3)
    captiont <- paste(colnamev, "SR =", sharper, collapse=" ")
    captiont <- paste("Rolling Portfolio Optimization Strategy: ", captiont)

    dygraphs::dygraph(cumsum(pnls)[endp], main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
      dyLegend(show="always", width=500)
    # dygraphs::dygraph(cumsum(pnls)[endp], main="Rolling Portfolio Optimization Strategy") %>%
    #   dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
    #   dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
    #   dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=3, col="blue") %>%
    #   dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=3, col="red")
  })  # end output plot
  
  # output$dygraph <- dygraphs::renderDygraph({
  #   dygraph(pnls(), main="Rolling Portfolio Optimization Strategy") %>%
  #     dySeries("strategy", label="strategy", strokeWidth=1, color="red")
  # })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
