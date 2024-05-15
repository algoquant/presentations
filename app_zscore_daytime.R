##############################
# This is a shiny app for simulating a contrarian Bollinger 
# strategy for daytime returns.
# It calculates the z-scores of the cumulative daytime returns, 
# equal to the difference between the cumulative returns minus 
# their trailing mean, divided by their volatility.
# The strategy holds a position in the stock of either one dollar 
# long or one dollar short.
# The strategy applies a threshold level to the z-scores, and 
# switches between long and short positions.
# The strategy switches to a long position if the z-score drops 
# below minus the threshold (indicating the prices are cheap), 
# and switches to a short position if the z-score exceeds the 
# threshold (indicating the prices are rich - expensive).
# It holds the position until the z-score crosses the opposite 
# threshold level.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Model and data setup

captiont <- paste("Bollinger Strategy for Daytime Returns")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  # Create single row with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symboln", label="Symbol", choices=rutils::etfenv$symbolv, selected="VTI")),
    # Input lambda decay parameter
    column(width=3, sliderInput("lambda", label="lambda:", min=0.01, max=0.99, value=0.4, step=0.01)),
    # Input threshold interval
    column(width=2, sliderInput("threshv", label="threshold", min=0.1, max=2.0, value=1.0, step=0.1))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="550px")
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values
  values <- reactiveValues()
  
  # Calculate the daytime returns
  retd <- shiny::reactive({

    symboln <- input$symboln
    cat("Loading data for ", symboln, "\n")

    # Calculate the log of OHLC VTI prices
    # symboln <- "VTI"
    ohlc <- get(symboln, rutils::etfenv)
    ohlc <- log(ohlc)
    openp <- quantmod::Op(ohlc)
    # highp <- quantmod::Hi(ohlc)
    # lowp <- quantmod::Lo(ohlc)
    closep <- quantmod::Cl(ohlc)
    # Calculate the close-to-close log returns, 
    # the daytime open-to-close returns 
    # and the overnight close-to-open returns.
    # retp <- rutils::diffit(closep)
    # colnames(retp) <- "daily"
    retd <- (closep - openp)
    colnames(retd) <- "daytime"
    # reton <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))
    # colnames(reton) <- "overnight"
    # Return the daytime returns
    retd
    
  })  # end Load the closing prices
  
  # Calculate the cumulative daytime returns
  retc <- shiny::reactive({
    
    cumsum(retd())
    
  })  # end Load the closing prices
  
  # Calculate the z-scores
  zscores <- shiny::reactive({
    
    cat("Calculating z-scores \n")
    # Get model parameters from input argument
    lambda <- input$lambda
    # retd <- retd()
    retc <- retc()

    retm <- rutils::lagit(HighFreq::run_mean(retc, lambda=lambda))
    retv <- sqrt(rutils::lagit(HighFreq::run_var(retc, lambda=lambda)))
    zscores <- ifelse(retv > 0, (retc - retm)/retv, 0)
    zscores

  })  # end reactive code
  
  # Calculate the pnls
  pnls <- shiny::reactive({
    
    symboln <- input$symboln
    cat("Calculating pnls \n")
    # Get model parameters from input argument
    threshv <- input$threshv
    retd <- retd()
    # retc <- retc()
    nrows <- NROW(retd)
    zscores <- zscores()
    
    # Calculate the positions from the z-scores
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    posv <- ifelse(zscores > threshv, -1, posv)
    posv <- ifelse(zscores < -threshv, 1, posv)
    posv <- zoo::na.locf(posv)
    posv <- rutils::lagit(posv, lagg=1)
    
    # Number of trades
    flipi <- rutils::diffit(posv)
    values$ntrades <- sum(abs(flipi) > 0)
    # captiont <- paste("Number of trades =", ntrades)
    
    pnls <- posv*retd
    pnls <- cbind(retd, pnls)
    colnames(pnls) <- c(symboln, "Strategy")
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    
    cumsum(pnls)
    
  })  # end reactive code
  
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    pnls <- pnls()
    # Get Sharpe ratios
    sharper <- values$sharper
    # Get number of trades
    ntrades <- values$ntrades
    colnamev <- colnames(pnls)
    captiont <- paste("Strategy for", input$symboln, "/ \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
                      "Number of trades=", ntrades)
    
    dygraphs::dygraph(pnls, main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
      dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red")
    
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
