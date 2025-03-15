##############################
# This is a shiny app for simulating an RSI
# strategy for ETFs.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# dtable <- data.table::fread("/Users/jerzy/Develop/data/SPY_second_20230929.csv")
# datev <- as.POSIXct(dtable$timestamp/1e3, origin="1970-01-01", tz="America/New_York")
# pricev <- xts::xts(dtable[, .(price, volume)], order.by=datev)
# colnames(pricev)[1] <- "SPY"
# nrows <- NROW(pricev)
# pricev <- pricev$SPY
# pricevv <- pricev
# pricev <- pricevv
# dygraph(pricev$SPY)

pricev <- log(rutils::etfenv$SPY$SPY.Close)
colnames(pricev) <- "SPY"
retv <- rutils::diffit(pricev)
nrows <- NROW(pricev)
# pricev <- cumsum(retv)


## Model and data setup

captiont <- paste("RSI Strategy")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbol
    # column(width=2, selectInput("symbol", label="Symbol", choices=rutils::etfenv$symbolv, selected="VTI")),
    # Input add annotations Boolean
    # column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-offer spread
    # column(width=2, numericInput("bid_offer", label="Bid-offer:", value=0.0000, step=0.0001))
    # Input the EWMA decay
    column(width=2, sliderInput("lambdaf", label="Lambda:", min=0.1, max=0.99, value=0.8, step=0.01)),
    # Input z-score threshold level
    column(width=2, sliderInput("threshz", label="Threshold", min=1, max=49, value=10, step=1)),
    # Input trending or reverting (contrarian) strategy
    # column(width=1, selectInput("coeff", label="Trend (1) Revert (-1)", choices=c(1, -1), selected=(-1)))
    # Input the trade lag
    # column(width=2, sliderInput("lagg", label="lagg", min=1, max=4, value=2, step=1))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="550px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()

  # Load the closing prices
  # Load the log returns
  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    cat("Recalculating strategy", "\n")

    # Calculate EWMA prices
    gainm <- HighFreq::run_mean(ifelse(retv > 0, retv, 0), input$lambdaf)
    lossm <- HighFreq::run_mean(ifelse(retv < 0, -retv, 0), input$lambdaf)
    # Calculate the RSI indicator
    rsii <- 100 * gainm/(gainm + lossm)
    rsii[1] <- 0
    # Calculate the RSI strategy
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    posv <- ifelse(rsii < input$threshz, 1, posv)
    posv <- ifelse(rsii > (100-input$threshz), -1, posv)
    posv <- zoo::na.locf(posv, na.rm=FALSE)
    posv <- rutils::lagit(posv, lagg=1)
    
    # Calculate indicator of flipped positions
    flipi <- rutils::diffit(posv)
    # Calculate number of trades
    values$ntrades <- sum(abs(flipi)>0)
    
    # Add buy/sell indicators for annotations
    longi <- (flipi > 0)
    shorti <- (flipi < 0)
    
    pnls <- retv*posv
    pnls <- cbind(retv, pnls)
    colnames(pnls)[2] <- "RSI"
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    pnls <- cumsum(pnls)

    pnls
    # cbind(pricev, rsii)

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
    
    captiont <- paste("Strategy for", "SPY", "/ \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
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
        dyLegend(show="always", width=500)
    } else if (add_annotations == FALSE) {
      # cat("PLotting2", "\n")
      dygraphs::dygraph(pnls, main=captiont) %>%
        dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
        dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red") %>%
        dyLegend(show="always", width=500)
    }  # end if
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
