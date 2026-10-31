##############################
# This is a shiny app for plotting stock prices and indicators.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

# Specify the strategies to load
stratv <- c("Ladder", "Ladder_wider", "SharpEMA")
# Specify the indicators to load
indicv <- c("zscore", "ema", "vol")
# Specify the symbols to load
symbolv <- c("MU", "SPY", "META", "AMAT", "AMD", "MSFT", "AVGO", "GOOGL", "JPM", "PANW", "TSLA", "AAPL", "AMZN", "NVDA", "CRM", "XLK", "XOM", "QCOM", "COIN", "ORCL", "PLTR", "WMT", "INTC", "NFLX")

# Specify the date to load
# Get all the file names for the symbol MU
filev <- Sys.glob("/Users/jerzy/Develop/trading/logs/Ladder_blotter_*.csv")
# Extract the dates from the file names
dater <- gsub(".*Ladder_blotter_(\\d{8})\\.csv", "\\1", filev)

captiont <- paste("Stock Prices With Indicators")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input strategy name
    column(width=2, selectInput("stratn", label="Strategy", choices=stratv, selected="Ladder")),
    # Input indicator name
    column(width=2, selectInput("indicn", label="Indicator", choices=indicv, selected="zscore")),
    # Input stock symbol
    column(width=2, selectInput("symboln", label="Symbol", choices=symbolv, selected="SPY")),
    # Input date
    column(width=2, selectInput("daten", label="Date", choices=dater, selected=dater[NROW(dater)])),
    # Input add annotations Boolean
    # column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-ask spread
    # column(width=2, numericInput("bidask", label="Bid-ask:", value=0.0000, step=0.0001))
  ),  # end fluidRow

  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="650px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()

  # Load the indicator
  indicd <- shiny::reactive({
    
    stratn <- input$stratn
    indicn <- input$indicn
    daten <- input$daten
    cat("Loading the indicator", indicn, "for", stratn, "for", daten, "\n")
    # Load the indicator
    filen <- paste0("/Users/jerzy/Develop/trading/logs/", stratn, "_", indicn, "_", daten, ".csv")
    indicd <- data.table::fread(filen, tz="")
    datev <- as.POSIXct(indicd$timestamp, FUN=as.POSIXct, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
    indicd <- xts::xts(indicd, order.by=datev)
    
    return(indicd)
    
  })  # end Load the OHLC prices
  

  # Load the OHLC prices
  ohlc <- shiny::reactive({
    
    symboln <- input$symboln
    daten <- input$daten
    cat("Loading the OHLC prices for", symboln, "for", daten, "\n")
    filen <- paste0("/Users/jerzy/Develop/data/minutes/live/", symboln, "_", daten, ".csv")
    ohlc <- data.table::fread(filen, tz="")
    datev <- as.POSIXct(ohlc$timestamp, FUN=as.POSIXct, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
    ohlc <- xts::xts(ohlc[, -(1:2)], order.by=datev)
    return(ohlc)
    
  })  # end Load the OHLC prices
  
  
  # Plot the OHLC prices with trade annotations
  output$dyplot <- dygraphs::renderDygraph({


    stratn <- input$stratn
    symboln <- input$symboln
    indicn <- input$indicn
    # Get the OHLC prices for the selected symbol
    ohlc <- ohlc()
    
    # Get the indicator for the selected symbol
    indicd <- indicd()
    indicd <- indicd[, symboln]
    
    # Combine prices and indicator into one xts object
    plot_data <- cbind(ohlc[, 1:4], indicd)
    colnames(plot_data)[5] <- indicn
    
    # Plot candlestick dygraph
    dyplot <- dygraphs::dygraph(plot_data) %>%
      dygraphs::dyCandlestick()
    
    if (indicn == "ema")
      # Add indicator with same y-axis
      dyplot <- dygraphs::dySeries(dyplot, indicn, strokeWidth=1, color="blue") %>%
      dyLegend(show="always", width=300)
    else
      # Add indicator with extra y-axis
      dyplot <- dygraphs::dySeries(dyplot, indicn, axis="y2", strokeWidth=1, color="blue") %>%
      dyLegend(show="always", width=300)
    
    return(dyplot)

  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
