##############################
# This is a shiny app for simulating a Bollinger strategy 
# for intraday 1-minute price bars of single stocks.
# 
# The indicator of the strategy is the change in price 
# compared to the open price of the day.
# If the indicator exceeds the threshold value, then the 
# strategy takes either a long or a short $1 position in 
# the stock.
# The strategy can be either trending or reverting. 
#
# Just press the "Run App" button on upper right of this panel.
##############################


## Model and data setup
## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
# library(shinyWidgets)
library(dygraphs)

# Minutes in business year
numm <- 252*6.5*60

# Load the list of OHLC price bars
# Get all the file names with *.RData in the data/minutes directory
filev <- Sys.glob("/Users/jerzy/Develop/data/minutes/*_list.RData")
# Extract the symbol names from the file names
filev <- sapply(filev, function(x) {
  x <- strsplit(x, split="/")
  x <- last(x[[1]])
  x <- strsplit(x, split="[.]")[[1]][1]
  # x <- strsplit(x, split="_")
  # x <- first(x[[1]])
  return(x)
}, USE.NAMES=FALSE) # end sapply


# timev <- "T12:30:00/T16:00:00"
# Create a series of intraday time stringss in 10-minute intervals
minutev <- c("00:00", "10:00", "20:00", "30:00", "40:00", "50:00")
hourv <- sprintf("%02d", 06:18)
timev <- paste0(rep(hourv, each=length(minutev)), ":", rep(minutev, times=length(hourv)))

directv <- -1
# Default values for the time interval
startt <- "09:30:00"
endt <- "14:30:00"

# volt <- 0.01 ##  Volatility target for scaling the strategy PnLs
# varfloor <- 1e-8 ##  Variance floor to prevent division by zero in Kelly ratio calculations

captiont <- paste("Bollinger Strategy")

# ## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input file name
    column(width=2, selectInput("filen", label="File name", choices=filev, selected=filev[2])),
    ##  Input volatility threshold parameter
    column(width=2, sliderInput("threshl", label="Threshold", min=0.001, max=0.004, value=0.0015, step=0.0001)),
    # Input the time of day
    column(width=4, shinyWidgets::sliderTextInput("timeval", label="Start and End Times:", choices=timev,
                                    selected=c(startt, endt), width="100%")),
    # Input trending or reverting (contrarian) strategy
    column(width=2, selectInput("directv", label="Trend (1) Revert (-1)", choices=c(1, -1), selected=(1))),
    # Input the bid-ask spread
    column(width=2, numericInput("bidask", label="Bid-ask [$]:", value=0.01, step=0.01)),
  ),  ##  end fluidRow

  ##  Render the plot in a new row
  fluidRow(
    dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  ),  ##  end fluidRow
  
)  ##  end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  ##  Create an empty list of reactive values.
  values <- reactiveValues()

  # Load the list of prices
  ohlcl <- shiny::reactive({
    # Get the file name from the input
    filen <- input$filen
    # Load the data file
    filen <- paste0("/Users/jerzy/Develop/data/minutes/", filen, ".RData")
    cat("Loading OHLC bars from the file: ", filen, "\n")
    if (file.exists(filen)) {
      loadd <- load(filen)
    } else {
      stop(paste("File not found:", filen))
    }  # end if
    return(ohlcl)
    
  })  # end Load the OHLC prices
  
  
  ##  Recalculate the strategy
  pnls <- shiny::reactive({
    
    # symboln <- input$symboln
    # cat("Recalculating PnLs for", symboln, "\n")
    # volscale <- input$volscale
    ohlcl <- ohlcl()
    # Reference price for calculating the price change
    priceref <- ohlcl[[1]][1, 1]
    symboln <- rutils::get_name(colnames(priceref))
    values$symboln <- symboln
    priceref <- as.numeric(priceref)
    
    # threshl <- input$threshl
    directv <- as.numeric(input$directv)
    # volt <- input$volt
    # Time of day interval
    timeval <- shiny::debounce(reactive(input$timeval), millis = 1000)
    startt <- timeval()[1]
    endt <- timeval()[2]
    timer <- paste0("T", startt, "/T", endt)

    ##  Calculate the strategy PnLs
    # Perform a loop over the list of OHLC bars, and calculate the strategy PnLs for each day.
    ntrades <- 0 ## Number of trades
    pnls <- lapply(ohlcl, function(x) {
      closep <- Cl(x) # Extract the close prices
      # Subset the close prices to the specified time range
      closep <- closep[timer]
      priceref <- as.numeric(first(closep))
      threshp <- input$threshl*priceref
      closep <- closep - priceref
      posv <- rep(NA_integer_, NROW(closep)) ##  Stock positions
      posv[1] <- 0
      posv <- ifelse(closep > threshp, directv, posv)
      posv <- ifelse(closep < -threshp, -directv, posv)
      posv <- zoo::na.locf(posv)
      # Lag the positions to trade in the next period
      posv <- rutils::lagit(posv, lagg=1)
      retp <- rutils::diffit(closep)
      pnls <- posv*retp
      # Calculate the transaction costs
      tradez <- abs(rutils::diffit(posv))
      ntrades <<- ntrades + sum(tradez > 0)
      costs <- 0.5*input$bidask*tradez
      pnls <- (pnls - costs)
      pnls <- cbind(retp, pnls)
      return(pnls)
    })  ## end lapply over ohlcl list of xts objects
    pnls <- do.call(rbind, pnls)
    values$ntrades <- ntrades
    
    # pnls <- cbind(cumsum(retp), probv)
    colnames(pnls) <- c(symboln, "Strategy")
    
    ##  Calculate Sharpe ratios
    sharper <- sqrt(numm)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    names(sharper) <- colnames(pnls)
    values$sharper <- round(sharper, 3)
    
    pnls <- cumsum(pnls)
    return(pnls)

  })  ##  end Recalculate the strategy
  

  ##  Plot the cumulative scaled returns
  ##  Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    ##  Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    ##  Get Sharpe ratios
    sharper <- values$sharper
    # Get number of trades
    ntrades <- values$ntrades
    ##  Create the caption with Sharpe ratios
    captiont <- paste0(c(paste0(names(sharper), " SR=", sharper), 
                       paste0("Number of trades=", ntrades)), collapse=" / \n")
    
    ##  Plot dygraph of the cumulative PnLs
    dyplot <- dygraphs::dygraph(pnls, main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
      dyLegend(show="always", width=300)
    
    ##  Plot dygraph with two y-axes
    # dyplot <- dygraphs::dygraph(pnls, main=captiont) %>%
    #   dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
    #   dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
    #   dySeries(axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
    #   dySeries(axis="y2", label=colnamev[2], strokeWidth=1, col="red") %>%
    #   dyLegend(show="always", width=300)
      
    ##  Return the dygraph object
    return(dyplot)


  })  ##  end output plot

}  ##  end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
