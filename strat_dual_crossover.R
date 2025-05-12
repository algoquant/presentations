##############################
# This is a shiny app for simulating a dual EWMA moving average 
# crossover strategy for ETFs.
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
retv <- rutils::diffit(pricev)
nrows <- NROW(pricev)
pricev <- cumsum(retv)

# look_back <- 5
# half_back <- look_back %/% 2
# medianv <- roll::roll_median(pricev, width=look_back)
# medianv[1:look_back, ] <- pricev[1:look_back, ]
# medianv <- rutils::lagit(medianv, lagg=(-half_back), pad_zeros=FALSE)
# madv <- HighFreq::roll_var(retv, look_back=look_back, method="nonparametric")
# madv <- rutils::lagit(madv, lagg=(-half_back), pad_zeros=FALSE)
# zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
# range(zscores); mad(zscores)
# madz <- mad(zscores[abs(zscores) > 0])
# hist(zscores, breaks=10000, xlim=c(-3*madz, 3*madz))
# pricev[abs(zscores) > 5, ] <- NA
# pricev[1, ] <- pricev[2, ]
# pricev <- na.locf(pricev)

# datav <- cbind(pricev, zscores)
# colnames(datav) <- c("SPY", "Z-scores")
# colv <- colnames(datav)
# dygraphs::dygraph(datav, main="Z-scores") %>%
#   dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
#   dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
#   dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
#   dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red")


## Model and data setup

captiont <- paste("Dual EWMA Moving Average Crossover Strategy")

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
    # Input the EWMA decays
    column(width=2, sliderInput("lambdaf", label="Fast lambda:", min=0.1, max=0.99, value=0.5, step=0.01)),
    column(width=2, sliderInput("lambdas", label="Slow lambda:", min=0.1, max=0.99, value=0.8, step=0.01)),
    # Input trending or reverting (contrarian) strategy
    column(width=1, selectInput("coeff", label="Trend (1) Revert (-1)", choices=c(1, -1), selected=(-1)))
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
    # Get model parameters from input argument
    lambdaf <- input$lambdaf
    lambdas <- input$lambdas
    # look_back <- input$look_back
    # lagg <- input$lagg
    lagg <- 1
    
    # Calculate EWMA prices
    emaf <- HighFreq::run_mean(pricev, lambda=lambdaf)
    emas <- HighFreq::run_mean(pricev, lambda=lambdas)

    # Determine dates when the emas have crossed
    crossi <- sign(emaf - emas)
    
    # Calculate cumulative sum of EWMA crossing indicator
    # crossc <- HighFreq::roll_sum(tseries=crossi, look_back=2)
    # crossc[1:2] <- 0
    # Calculate the positions
    # Flip position only if the crossi and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    coeff <- as.numeric(input$coeff)
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    # posv <- ifelse(crossc == 2, coeff, posv)
    # posv <- ifelse(crossc == (-2), -coeff, posv)
    posv <- ifelse(crossi == 1, coeff, posv)
    posv <- ifelse(crossi == (-1), -coeff, posv)
    posv <- zoo::na.locf(posv, na.rm=FALSE)
    # posv[1:lagg] <- 0
    
    # Calculate indicator of flipped positions
    flipi <- rutils::diffit(posv)
    # Calculate number of trades
    values$ntrades <- sum(abs(flipi)>0)
    
    # Add buy/sell indicators for annotations
    longi <- (flipi > 0)
    shorti <- (flipi < 0)
    
    # Lag the positions to trade in next period
    posv <- rutils::lagit(posv, lagg=1)
    
    # Calculate strategy pnls
    pnls <- posv*retv
    
    # Calculate transaction costs
    # costs <- 0.5*input$bid_offer*abs(flipi)
    # pnls <- (pnls - costs)

    # Scale the pnls so they have same SD as the returns
    # pnls <- pnls*sd(retv[retv<0])/sd(pnls[pnls<0])
    
    # Bind together strategy pnls
    # pnls <- cbind(retv, pnls)
    # colnames(pnls) <- c("SPY", "Strategy")
    pnls <- cbind(retv, pnls)
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    pnls <- cumsum(pnls)
    
    pnls <- cbind(pnls, emas, emaf)
    colnames(pnls) <- c("SPY", "Strategy", "MAS", "MAF")
    
    
    # Bind strategy pnls with indicators
    # pnls <- cbind(pnls, pricev[longi], pricev[shorti])
    # colnames(pnls) <- c("SPY", "Strategy", "Buy", "Sell")
    
    # cat("Finished", "\n")
    pnls

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
        dySeries(name=colv[3], axis="y", label=colv[3], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=colv[4], axis="y", label=colv[4], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == FALSE) {
      # cat("PLotting2", "\n")
      dygraphs::dygraph(pnls, main=captiont) %>%
        dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
        dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red") %>%
        dySeries(name=colv[3], axis="y", strokeWidth=1, col="green") %>%
        dySeries(name=colv[4], axis="y", strokeWidth=1, col="orange") %>%
        dyLegend(show="always", width=500)
    }  # end if
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
