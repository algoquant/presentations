##############################
# This is a shiny app for simulating the intraday ratchet 
# strategy, which is a mean-reversion strategy that bets 
# on the price reverting to the open price.
# The strategy is implemented in the C++ function ratchetx().
# The function ratchetx() calculates the z-score equal to 
# the difference between the current price minus the open
# price, multiplied by the z-factor.
# If the z-score is positive, the strategy sells shares 
# short, proportional to the z-score.
# If the z-score is negative, the strategy buys shares, 
# proportional to the z-score.
# The strategy accumulates an inventory of shares. 
# It continues selling shares as the z-score keeps rising,
# and it continues buying as the z-score keeps dropping.
# The strategy waits to sell its inventory only after 
# the z-score has changed its sign, but not before that.
# 
# You must compile the C++ file by running this command in R:
# Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/back_test.cpp")


# Runs the C++ function ratchet() from /Users/jerzy/Develop/Rcpp/back_test.cpp
#
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(shinyWidgets)
library(dygraphs)

## Model and data setup

# Create vector of start and end times for the selectInput() widget
# timev <- c("09:30", "10:00", "10:30", "11:00", "11:30", "12:00", "12:30", "13:00", "13:30", "14:00", "14:30", "15:00", "15:30", "16:00")
# Create a series of intraday times in 10-minute intervals
timev <- seq(from=as.POSIXct("2025-06-13 09:30:00", tz="America/New_York"),
             to=as.POSIXct("2025-06-13 16:00:00", tz="America/New_York"),
             by="10 min")
# Remove the date
timev <- format(timev, format="%H:%M:%S")

# Compile the C++ file
if (!exists("ratchetx")) {
  Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/back_test.cpp")
} # end if

# Load the OHLC prices
# if (!exists("pricel")) {
#   symboln <- "SPY"
#   cat("Loading the list of intraday minute prices for: ", symboln, "\n")
#   filen <- paste0("/Users/jerzy/Develop/data/minutes/", symboln, "_minutes_markets.RData")
#   loadd <- load(filen)
#   pricev <- quantmod::Cl(ohlc)
#   pricel <- split(pricev, f="days")
# } # end if

symbolv <- c("SPY", "NVDA", "XLK", "QQQ")
symboln <- symbolv[1]
captiont <- paste0("Intraday Ratchet Strategy for ", symboln)

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Intraday Ratchet Strategy"),

  fluidRow(
    # Input stock symbol
    column(width=1, selectInput("symboln", label="Symbol", choices=symbolv, selected="SPY")),
    # Input the lambda decay factor
    # column(width=2, sliderInput("lambdaf", label="Lambda:", min=0.5, max=0.999, value=0.9, step=0.001)),
    # Input the Z-score factor
    column(width=2, sliderInput("zfact", label="Z-factor", min=1.0, max=5.0, value=5.0, step=1.0)),
    # Input the position limit
    column(width=2, sliderInput("poslimit", label="Pos limit:", min=1, max=10, value=5, step=1)),
    # Input the time of day
    column(width=4, shinyWidgets::sliderTextInput("timev", label="Start and End Times:", choices=timev,
                                    selected=c(first(timev), last(timev)), width="100%")),
    # Input the start time
    # column(width=2, selectInput("timestart", label="Start time", choices=timev, selected="13:00")),
    # Input the end time
    # column(width=2, selectInput("timend", label="End time", choices=timev, selected="16:00")),
    # Input add annotations Boolean
    # column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False"))
  ),  # end fluidRow

  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()

  ##  Load the prices
  pricelist <- shiny::reactive({
    
    symboln <- input$symboln
    # cat("Loading prices for", symboln, "\n")
    
    # Load the OHLC prices
    if (!(exists("pricel") && symboln == rutils::get_name(colnames(pricel[[1]])[1]))) {
      cat("Loading the list of intraday minute prices for: ", symboln, "\n")
      filen <- paste0("/Users/jerzy/Develop/data/minutes/", symboln, "_minutes_markets.RData")
      # filen <- paste0("/Users/jerzy/Develop/data/minutes/", symboln, "_2025.RData")
      loadd <- load(filen)
      if (loadd == "ohlc") {
        pricev <- quantmod::Cl(ohlc)
        pricel <- split(pricev, f="days")
      } # end if
      return(pricel)
    } # end if

    return(pricel)
    
  })  ##  end Load the data
  
  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    symboln <- input$symboln
    cat("Recalculating strategy for: ", symboln, "\n")
    poslimit <- input$poslimit
    zfact <- input$zfact
    
    # Time of day interval
    timev <- shiny::debounce(reactive(input$timev), millis = 1000)
    timestart <- timev()[1]
    timend <- timev()[2]
    rangev <- paste0("T", timestart, "/T", timend)
    
    # List of prices
    pricel <- pricelist()
    
    # Perform a loop over the list of price vectors, and calculate the strategy PnLs for each day.
    pnll <- lapply(pricel, function(pricev) {
      # cat("Date: ", format(start(pricev)), "\n")
      if (NCOL(pricev) > 1)
        pricev <- quantmod::Cl(pricev)
      pricev <- pricev[rangev]
      if (NROW(pricev) == 0) {
        return(NULL)
      } # end if
      simout <- ratchetx(pricev, zfact=zfact, poslimit=poslimit)
      pnls <- simout[, 1]
      pnls <- xts::xts(pnls, order.by=index(pricev))
      retv <- rutils::diffit(pricev)
      pnls <- cbind(retv, pnls)
      return(pnls)
    }) ##  end lapply
    # Bind together strategy PnLs
    pnls <- rutils::do_call(rbind, pnll)
    # cat("dim(pnls): ", dim(pnls), "\n")
    colnames(pnls) <- c(symboln, "Strategy")

    # Calculate the Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    # cat("sharper =", values$sharper, "\n")
    
    pnls <- cumsum(pnls)
    return(pnls)

  })  # end Recalculate the strategy
  

  # Plot the cumulative strategy PnLs
  output$dyplot <- dygraphs::renderDygraph({
    
    # Get the PnLs
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Get Sharpe ratios
    sharper <- values$sharper

    captiont <- paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / ")
    
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
