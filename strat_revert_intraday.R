##############################
# This is a shiny app for simulating an intraday reverting 
# strategy.
# The strategy reverses its position when the trend reverses.
# The strategy buys one share if the price is below the 
# reference price, or it sells one share short if the 
# price is above the reference price.
# The reference price is either the initial price or the 
# EMA price. 
# If the decay factor lambdaf is NULL (the default) then 
# the reference price is the initial price.
# If the decay factor lambdaf is not NULL then the 
# reference price is the EMA price.
# The strategy is implemented in the C++ function trend_follow().
# It flips the sign of the PnLs to implement the reverting strategy.

# You must compile the C++ file by running this command in R:
# Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/back_test.cpp")

# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(shinyWidgets)
library(dygraphs)

## Model and data setup

# Minutes in business year
numm <- 252*6.5*60

# Compile the C++ function trend_follow() for backtesting the strategy
if (!exists("trend_follow")) {
  Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/back_test.cpp")
} # end if

# Load the list of OHLC price bars
# Get all the file names with *.RData in the data/minutes directory
filev <- Sys.glob("/Users/jerzy/Develop/data/minutes/*_minutes.RData")
# Extract the symbol names from the file names
filev <- sapply(filev, function(x) {
  x <- strsplit(x, split="/")
  x <- last(x[[1]])
  x <- strsplit(x, split="[.]")[[1]][1]
  # x <- strsplit(x, split="_")
  # x <- first(x[[1]])
  return(x)
}, USE.NAMES=FALSE) # end sapply


## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Intraday EMA Revert Strategy"),

  fluidRow(
    # Input file name
    column(width=2, selectInput("filen", label="File name", choices=filev, selected=filev[2])),
    # Input the lambda decay factor
    column(width=2, sliderInput("lambdaf", label="Lambda:", min=0.1, max=0.99, value=0.9, step=0.01)),
    # Input the Z-score factor
    # column(width=2, sliderInput("zfact", label="Z-factor", min=1.0, max=5.0, value=5.0, step=1.0)),
    # Input the time of day
    # column(width=4, shinyWidgets::sliderTextInput("timeval", label="Start and End Times:", choices=timev,
    #                                 selected=c(first(timev), last(timev)), width="100%")),
    # Input the start time
    # column(width=2, selectInput("startt", label="Start time", choices=timev, selected="13:00")),
    # Input the end time
    # column(width=2, selectInput("endt", label="End time", choices=timev, selected="16:00")),
    # Input add annotations Boolean
    # column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False"))
  ),  # end fluidRow

  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="700px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()

  # Load the list of prices
  ohlc <- shiny::reactive({
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
    return(ohlc)
    
  })  # end Load the OHLC prices
  
  
  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    # List of prices
    ohlc <- ohlc()
    pricev <- quantmod::Cl(ohlc)
    pricev <- pricev["T09:30:00/T16:00:00"]
    retv <- rutils::diffit(pricev)
    symboln <- rutils::get_name(colnames(pricev))
    values$symboln <- symboln
    lambdaf <- input$lambdaf
    
    cat("Recalculating strategy for: ", symboln, "\n")

    # Simulate the trending strategy using the C++ function trend_follow()
    pnlpos <- trend_follow(pricev, lambda=lambdaf)
    # Calculate the number of trades and the PnLs
    values$ntrades <- sum(abs(rutils::diffit(pnlpos[, 2])) > 0)
    # Flip the sign of the PnLs to implement the reverting strategy
    pnls <- -pnlpos[, 1]
    pnls <- cbind(retv, pnls)
    colnames(pnls) <- c(symboln, "Strategy")

    # Calculate the Sharpe ratios
    sharper <- sqrt(numm)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    # cat("sharper =", values$sharper, "\n")
    
    pnls <- cumsum(pnls)
    return(pnls)

  })  # end Recalculate the strategy
  

  # Plot the cumulative strategy PnLs
  output$dyplot <- dygraphs::renderDygraph({
    
    # Get the PnLs
    symboln <- values$symboln
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Get Sharpe ratios
    sharper <- values$sharper
    ntrades <- values$ntrades
    captiont <- paste0("Sharpe ", paste0(c(symboln, "Strategy"), " = ", sharper, collapse=" / "), "/ \n",
                       "Number trades =", ntrades)
    # captiont <- paste0("Sharpe ", paste0(c(symboln, "Strategy"), " = ", sharper, collapse=" / "))
    
    # Plot with annotations
    # add_annotations <- input$add_annotations
    add_annotations <- FALSE
    
    # Return to the output argument a dygraph plot with two y-axes
    if (isTRUE(add_annotations)) {
      dygraphs::dygraph(pnls, main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red") %>%
        dySeries(name=colnamev[3], axis="y", drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=colnamev[4], axis="y", drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else {
      endd <- rutils::calc_endpoints(pnls, interval="minutes")
      dygraphs::dygraph(pnls[endd, ], main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red") %>%
        dyLegend(show="always", width=300)
    }  # end if
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
