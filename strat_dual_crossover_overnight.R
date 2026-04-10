##############################
# This is a shiny app for simulating a dual EMA crossover strategy 
# for daytime, overnight, or daily returns.
# 
# Comments:
#   The overnight returns alone achieve Sharpe ratio of 1.001, 
#   and the strategy 1.129.  Combined achieves 1.407. 
#   So the main benefit of the strategy is diversification and alpha.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

captiont <- paste("Dual EMA Crossover Strategy For Daily, Daytime, Overnight Returns")

# Calculate the log of OHLC SPY prices
ohlc <- log(rutils::etfenv$SPY)
openp <- quantmod::Op(ohlc)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
closep <- quantmod::Cl(ohlc)
# Calculate the close-to-close log returns,
# the daytime open-to-close returns
# and the overnight close-to-open returns.
daily <- rutils::diffit(closep)
colnames(daily) <- "daily"
daytime <- (closep - openp)
colnames(daytime) <- "daytime"
overnight <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))
colnames(overnight) <- "overnight"

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol", choices=c("daily", "daytime", "overnight"), selected="overnight")),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # If trend=1 then trending, If trend=(-1) then contrarian
    column(width=2, selectInput("trend", label="Trend coefficient", choices=c(1, -1), selected=(1)))
    # Input the bid-ask spread
    # column(width=2, numericInput("bidask", label="Bid-ask:", value=0.0000, step=0.0001))
  ),  # end fluidRow

  fluidRow(
    # Input the EMA decays
    column(width=2, sliderInput("lambdaf", label="Fast lambda:", min=0.3, max=0.99, value=0.92, step=0.01)),
    column(width=2, sliderInput("lambdas", label="Slow lambda:", min=0.3, max=0.99, value=0.97, step=0.01)),
    # Input the trade lag
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=4, value=1, step=1))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="550px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()

  # Load the closing prices
  priceret <- shiny::reactive({

    symbol <- input$symbol
    cat("Loading data for ", symbol, "\n")
    # Get the symbol data from the workspace
    datav <- get(symbol)
    return(cbind(cumsum(datav), datav))
    
  })  # end Load the closing prices
  
  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    cat("Recalculating strategy for ", input$symbol, "\n")
    # Get model parameters from input argument
    lambdaf <- input$lambdaf
    lambdas <- input$lambdas
    lagg <- input$lagg
    trend <- as.numeric(input$trend)
    # Get the prices and returns
    priceret <- priceret()
    pricev <- priceret[, 1]
    retv <- priceret[, 2]
    nrows <- NROW(priceret)
    
    # Calculate EMA prices
    emaf <- HighFreq::run_mean(pricev, lambda=lambdaf)
    emas <- HighFreq::run_mean(pricev, lambda=lambdas)

    # Determine dates when the emas have crossed
    crossi <- sign(emaf - emas)
    
    # Calculate cumulative sum of EMA crossing indicator
    crossc <- HighFreq::roll_sum(timeser=crossi, lookb=lagg)
    crossc[1:lagg] <- 0
    # Calculate the positions
    # Flip position only if the crossi and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    posv <- ifelse(crossc == lagg, 1, posv)
    posv <- ifelse(crossc == (-lagg), -1, posv)
    posv <- zoo::na.locf(posv, na.rm=FALSE)
    posv[1:lagg] <- 0
    
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
    pnls <- trend*posv*retv
    
    # Calculate transaction costs
    # costs <- 0.5*input$bidask*abs(flipi)
    # pnls <- (pnls - costs)

    # Scale the pnls so they have same SD as the returns
    # pnls <- pnls*sd(retv[retv<0])/sd(pnls[pnls<0])
    
    # Bind together strategy pnls
    pnls <- cbind(retv, pnls, 0.5*(retv+pnls))
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)

    # Bind strategy pnls with indicators
    pnls <- cumsum(pnls)
    # pnls <- cbind(pnls, emaf, emas)
    # colnames(pnls) <- c(paste(input$symbol, "Returns"), "Strategy", "EMAF", "EMAS")
    colnames(pnls) <- c(paste(input$symbol, "Returns"), "Strategy", "Combined")
    # pnls <- cbind(pnls, pricev[longi], pricev[shorti])
    # colnames(pnls) <- c(paste(input$symbol, "Returns"), "Strategy", "Buy", "Sell")

    pnls

  })  # end Recalculate the strategy
  

  # Plot the cumulative strategy pnls
  output$dyplot <- dygraphs::renderDygraph({
    
    # Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Get Sharpe ratios
    sharper <- values$sharper
    # Get number of trades
    ntrades <- values$ntrades
    
    captiont <- paste("Strategy for", input$symbol, "/ \n", 
                      paste0(colnamev, "=", sharper, collapse=" / "), "/ \n",
                      "Number of trades=", ntrades)
    
    # captiont <- paste("Strategy for", input$symbol, "/ \n", 
    #                   paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
    #                   "Number of trades=", ntrades)
    
    # Plot with annotations
    add_annotations <- input$add_annotations
    
    # Return to the output argument a dygraph plot with two y-axes
    if (add_annotations == "True") {
      dygraphs::dygraph(pnls, main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red") %>%
        dySeries(name=colnamev[3], axis="y", drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=colnamev[4], axis="y", drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == "False") {
      colorv <- c("blue", "red", "green", "orange")
      dygraphs::dygraph(pnls, main=captiont) %>%
      # dygraphs::dygraph(pnls[, c(1, 3, 4)], main=captiont) %>%
        dyOptions(colors=colorv, strokeWidth=2) %>%
          dyLegend(show="always", width=300)
        # dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        # dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        # dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
        # dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red")
    }  # end if
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
