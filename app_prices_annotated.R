##############################
# This is a shiny app for plotting stock prices with trade annotations.
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
# Specify the symbols to load
symbolv <- c("MU", "SPY", "META", "AMAT", "AMD", "MSFT", "AVGO", "GOOGL", "JPM", "PANW", "TSLA", "AAPL", "AMZN", "NVDA", "CRM", "XLK", "XOM", "QCOM", "COIN", "ORCL", "PLTR", "WMT", "INTC", "NFLX")

# First run the code to load the OHLC prices for all symbols in scratch.R
# Get all the file names with the OHLC prices
filev <- paste0("/Users/jerzy/Develop/data/minutes/chained/", symbolv, ".RData")

captiont <- paste("Stock Prices With Trade Annotations")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input strategy name
    column(width=2, selectInput("stratn", label="Strategy", choices=stratv, selected="Ladder")),
    # Input stock symbol
    column(width=2, selectInput("symboln", label="Symbol", choices=symbolv, selected="SPY")),
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

  # Load the orders
  orderm <- shiny::reactive({
    
    stratn <- input$stratn
    cat("Loading the orders for ", stratn, "\n")
    # Load the trade orders
    filen <- paste0("/Users/jerzy/Develop/trading/logs/", stratn, "_orders.csv")
    orderm <- data.table::fread(filen, tz="")
    
    return(orderm)
    
  })  # end Load the OHLC prices
  

  # Load the OHLC prices
  ohlc <- shiny::reactive({
    
    symboln <- input$symboln
    cat("Loading the OHLC prices ", symboln, "\n")
    filen <- paste0("/Users/jerzy/Develop/data/minutes/chained/", symboln, ".RData")
    load(filen)
    return(ohlc)
    
  })  # end Load the OHLC prices
  
  
  # Plot the OHLC prices with trade annotations
  output$dyplot <- dygraphs::renderDygraph({


    symboln <- input$symboln
    # Get the OHLC prices for the selected symbol
    ohlc <- ohlc()
    
    # Get the orders for the selected symbol
    orderm <- orderm()
    orderv <- orderm[orderm$symbol == symboln, ]
    if (NROW(orderv) > 0) {
      
      cat("Orders for symbol ", symboln, "\n")
      # Get the dates and times of the orders
      datetime <- as.POSIXct(orderv$timestamp, format="%Y-%m-%d %H:%M", tz="America/New_York")
      # Get the dates of the orders
      datev <- unique(as.Date(datetime))
      # Filter the OHLC prices for the dates of the orders
      ohlc <- ohlc[as.Date(index(ohlc)) %in% datev, ]
      
      # Plot candlestick dygraph
      dyplot <- dygraphs::dygraph(ohlc[, 1:4]) %>% 
        dygraphs::dyCandlestick()
      # Add annotations for buy/sell trades
      for (rown in 1:NROW(orderv)) {
        side <- if (orderv$limit_price[rown] < 0) "Buy" else "Sell"
        dyplot <- dygraphs::dyAnnotation(dyplot, x=datetime[rown],
                                         width=60, height=20, text=side,
                                         tooltip=paste(side, orderv$num_shares[rown], "shares at",
                                                       abs(orderv$limit_price[rown]), "on", orderv$timestamp[rown]))
      } ## end for loop
      
    } else {
      
      cat("No orders for symbol ", symboln, "\n")
      
      # Plot candlestick dygraph
      dyplot <- dygraphs::dygraph(ohlc[, 1:4]) %>% 
        dygraphs::dyCandlestick()
      
    } ## end if
    
    return(dyplot)

  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
