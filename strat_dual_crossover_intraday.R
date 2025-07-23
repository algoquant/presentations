##############################
# This is a shiny app for simulating an intraday dual EMA 
# crossover strategy for ETFs.
# 
# Comments:
#   The dual EMA crossover works better than the single EMA.
#   In market hours the trending strategies work well for SPY, but not for TLT.
#   In extended hours the reverting strategies work very well, 
#   but not clear how to monetize using just limit orders.
#   Added online scrubbing of prices because of price spikes in extended hours.
# 
# 
# Copyright (C) 2025 Jerzy Pawlowski
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Compile the C++ function scrub_online()
# Compile the file in R by running this command:
# Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/test_temp.cpp")

# Load R packages
library(HighFreq)
library(shiny)
library(shinyWidgets)
library(dygraphs)


# Get all the file names with *.RData in the data/minutes directory
filen <- Sys.glob("/Users/jerzy/Develop/data/minutes/*.RData")
symbolv <- sapply(filen, function(x) {
  x <- strsplit(x, split="/")
  x <- last(x[[1]])
  strsplit(x, split="[.]")[[1]][1]
}, USE.NAMES=FALSE) # end sapply


# symboln <- "SPY"
# filen <- paste0("/Users/jerzy/Develop/data/minutes/", symboln, ".RData")
# load(filen)

# Create a series of intraday times in 10-minute intervals
timev <- seq(from=as.POSIXct("2025-06-13 06:00:00", tz="America/New_York"),
             to=as.POSIXct("2025-06-13 18:00:00", tz="America/New_York"),
             by="10 min")
# Remove the date
timev <- format(timev, format="%H:%M:%S")



## Model and data setup

captiont <- paste("Intraday Dual EMA Crossover Strategy")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  shiny::fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symboln", label="Symbol", choices=symbolv, selected="SPY")),
    # Input trending or reverting (contrarian) strategy
    column(width=2, selectInput("coeff", label="Trend (1) Revert (-1)", choices=c(1, -1), selected=(1))),
    # Input the bid-ask spread
    column(width=2, numericInput("bidask", label="Bid-ask [$]:", value=0.01, step=0.01)),
    # Input the scrubbing tolerance
    column(width=2, numericInput("threshv", label="Scrub threshold [$]:", value=2.0, step=0.1)),
  ),  # end fluidRow
  
  shiny::fluidRow(
    # Input the EMA decay factors
    column(width=5, sliderInput("lambdav", label="Fast and Slow Lambdas:", min=0.1, max=0.99, value=c(0.97, 0.99), step=0.01, width="100%")),
    # Input the time of day
    column(width=5, sliderTextInput("timev", label="Start and End Times:", choices=timev,
                                    selected=c(first(timev), last(timev)), width="100%")),
  ),  # end fluidRow

  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="80%", height="550px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Load the list of OHLC prices
  pricel <- shiny::reactive({
    # Get the symbol from the input
    symboln <- input$symboln
    cat("Loading OHLC prices for", symboln, "\n")
    # Load the data file
    filen <- paste0("/Users/jerzy/Develop/data/minutes/", symboln, ".RData")
    if (file.exists(filen)) {
      load(filen)
      return(pricel)
    } else {
      stop(paste("File not found:", filen))
    }  # end if
    
  })  # end Load the OHLC prices
  
  
  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    cat("Recalculating strategy", "\n")
    values$ntrades <- 0
    # Get model parameters from input arguments
    # Debounce the inputs to avoid excessive recalculations
    # Fast and slow lambdas
    lambdav <- shiny::debounce(reactive(input$lambdav), millis = 1000)
    lambdaf <- lambdav()[1]
    lambdas <- lambdav()[2]
    # Time of day interval
    timev <- shiny::debounce(reactive(input$timev), millis = 1000)
    startt <- timev()[1]
    endt <- timev()[2]
    timev <- paste0("T", startt, "/T", endt)
    coeff <- as.numeric(input$coeff)
    # Scrubbing threshold
    threshv <- input$threshv
    # look_back <- input$look_back
    # lagg <- input$lagg
    lagg <- 1
    pricel <- pricel()
    
    ntrades <- 0
    pnll <- lapply(pricel, function(ohlc) {
      pricev <- ohlc[, 4]
      scrub_online(pricev, threshv=threshv)
      pricev <- xts::xts(pricev, order.by=index(ohlc))
      pricev <- pricev[timev]
      retv <- rutils::diffit(pricev)
      nrows <- NROW(pricev)
      # Calculate EMA prices
      emaf <- HighFreq::run_mean(pricev, lambda=lambdaf)
      emas <- HighFreq::run_mean(pricev, lambda=lambdas)
      # Determine dates when the EMAs have crossed
      crossi <- sign(emaf - emas)
      posv <- rep(NA_integer_, nrows)
      posv[1] <- 0
      # posv <- ifelse(crossc == 2, coeff, posv)
      # posv <- ifelse(crossc == (-2), -coeff, posv)
      posv <- ifelse(crossi == 1, coeff, posv)
      posv <- ifelse(crossi == (-1), -coeff, posv)
      posv <- zoo::na.locf(posv, na.rm=FALSE)
      # Calculate indicator of flipped positions
      # flipi <- rutils::diffit(posv)
      # Calculate number of trades
      # Add buy/sell indicators for annotations
      # longi <- (flipi > 0)
      # shorti <- (flipi < 0)
      # Lag the positions to trade in next period
      posv <- rutils::lagit(posv, lagg=1)
      # Calculate strategy pnls
      pnls <- posv*retv
      # Calculate the transaction costs
      tradez <- abs(rutils::diffit(posv))
      ntrades <<- ntrades + sum(tradez > 0)
      costs <- 0.5*input$bidask*tradez
      pnls <- (pnls - costs)
      # pnls <- cbind(retv, pnls)
      # colnames(pnls) <- c("SPY", "Strategy")
      # return(pnls)
      return(xts::xts(matrix(c(SPY=sum(retv), Strategy=sum(pnls)), nr=1), as.Date(end(pricev))))
    })
    pnls <- do.call(rbind, pnll)
    colnames(pnls) <- c("SPY", "Strategy")
    values$ntrades <- ntrades
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    pnls <- cumsum(pnls)
    # cat("Finished", "\n")
    return(pnls)

  })  # end Recalculate the strategy
  

  # Plot the cumulative strategy pnls
  output$dyplot <- dygraphs::renderDygraph({
    
    # Get the pnls
    # cat("PLotting1", "\n")
    pnls <- pnls()
    colv <- colnames(pnls)
    
    # Get Sharpe ratios
    sharper <- values$sharper
    # Get number of trades
    ntrades <- values$ntrades
    # Get the symbol from the input
    symboln <- input$symboln
    
    captiont <- paste0("Strategy for ", symboln, " / \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), " / \n",
                      "Number of trades=", ntrades)
    
    # Plot with annotations
    add_annotations <- FALSE
    
    # Return to the output argument a dygraph plot with two y-axes
    if (add_annotations == TRUE) {
      dygraphs::dygraph(pnls, main=captiont) %>%
        dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
        dySeries(name=colv[1], axis="y", label=colv[1], strokeWidth=1, col="blue") %>%
        dySeries(name=colv[2], axis="y2", label=colv[2], strokeWidth=1, col="red") %>%
        dySeries(name=colv[3], axis="y", label=colv[3], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=colv[4], axis="y", label=colv[4], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == FALSE) {
      # cat("PLotting2", "\n")
      dygraphs::dygraph(pnls, main=captiont) %>%
        dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
        dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red") %>%
        # dySeries(name=colv[3], axis="y", strokeWidth=1, col="green") %>%
        # dySeries(name=colv[4], axis="y", strokeWidth=1, col="orange") %>%
        dyLegend(show="always", width=500)
    }  # end if
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
