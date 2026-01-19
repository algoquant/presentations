##############################
# This is a shiny app for simulating portfolio momentum 
# strategy for ETFs.
# The argument lagg is the number of periods to lag the 
# calibration interval relative to the test interval.  
# The default to lagg=1, which means the calibration 
# interval ends just before the test interval starts.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load packages
library(shiny)
library(dygraphs)
library(rutils)

# Model and data setup

captiont <- paste("Portfolio Momentum Strategy for Sector ETFs X*")

# Select the ETF symbols starting with X:
symbolv <- rutils::etfenv$symbolv
symbolv <- symbolv[grep("^X", symbolv)]
symbolv <- c("SPY", "TLT", symbolv)
nweights <- NROW(symbolv)
nstocks <- NROW(symbolv)
# Calculate the percentage stock returns
retp <- na.omit(rutils::etfenv$returns[, symbolv])
nrows <- NROW(retp)
datev <- zoo::index(retp)
# retm <- retp$SPY

# Calculate the vector of average daily excess returns.
# riskf is the daily risk-free rate.
# riskf <- 0.03/260
# excess <- returns - riskf

# Calculate equal weight portfolio
# ncols <- NCOL(retp)
indeks <- xts::xts(rowMeans(retp), datev)

# End setup code


## Define elements of the UI user interface
uifun <- shiny::fluidPage(
  
  titlePanel(captiont),
  
  fluidRow(
    # Input Look back interval
    column(width=2, sliderInput("lookb", label="Look-back", min=20, max=300, value=25, step=5)),
    column(width=2, sliderInput("dimax", label="dimax:", min=2, max=nweights, value=4, step=1)),
    # Input the trade lag
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=18, value=15, step=1)),
    # Define the shrinkage intensity
    # sliderInput("alpha", label="shrinkage intensity alpha:",
    #             min=0.01, max=0.99, value=0.5, step=0.05)
    # Input lambda decay parameter
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  
)  # end shinyUI interface


## Define the server code
servfun <- shiny::shinyServer(function(input, output) {

  # Recalculate the PnLs
  pnls <- reactive({
    # get model parameters from input
    lookb <- input$lookb
    dimax <- input$dimax
    lagg <- input$lagg
    # alpha <- input$alpha
    
    # Calculate a vector of end points
    endd <- rutils::calc_endpoints(retp, interval="weeks")
    endd <- endd[endd > 2*nweights]
    endd <- endd + lagg
    endd <- endd[endd <= nrows]
    npts <- NROW(endd)
    
    # Define the start points
    startp <- c(rep_len(lagg, lookb-1), endd[1:(npts-lookb+1)])
    
    # Create a list of portfolio optimization parameters
    controll <- HighFreq::param_portf(method="maxsharpe", dimax=dimax, lagg=lagg, scalew="sumsq")
    
    # Rerun the model
    pnls <- drop(HighFreq::roll_portf(retx=retp, retp=retp, startp=startp-1, endd=endd-1, controll=controll))

    pnls <- pnls*sd(indeks)/sd(pnls)
    pnls <- cbind(indeks, pnls)
    colnames(pnls) <- c("Equal weight", "Strategy")
    return(pnls)
    
  })  # end reactive code
  
  output$dyplot <- renderDygraph({
    
    pnls <- pnls()
    colv <- colnames(pnls)
    
    # Calculate the Sharpe and Sortino ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 3)
    
    captiont <- paste("Portfolio Momentum for ETFs", "/ \n", 
                      paste0(c("EqWeight SR=", "Strategy SR="), sharper, collapse=" / "))
    
    # Create the output plot
    endw <- rutils::calc_endpoints(pnls, interval="weeks")
    dygraphs::dygraph(cumsum(pnls)[endw], main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
      dyLegend(show="always", width=300)
    # dygraphs::dygraph(cumsum(pnls)[endw], main=captiont) %>%
    #   dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
    #   dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
    #   dySeries(name=colv[1], strokeWidth=1, axis="y", col="red") %>%
    #   dySeries(name=colv[2], strokeWidth=1, axis="y2", col="blue")
    
  })  # end output plot
  
})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
