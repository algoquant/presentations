##############################
# This is a shiny app for a HighCharts candlestick plot of OHLC stock pricev, 
# with a moving average line.
# Just press the "Run App" button on the upper right of RStudio.
# Or run the app in MacBook terminal:  
#   Rscript -e "shiny::runApp('~/Develop/Presentations/app_highchartsma.R', port=7775)"
# On mini6 run: 
#   Rscript -e "shiny::runApp('~/Develop/R/app_highchartsma.R', host='0.0.0.0', port=7775)"
# And in browser load the page: http://206.72.197.242:7775
##############################


## Below is the setup code that runs once when the shiny app is started

# Load packages
library(quantmod)
library(shiny)
library(shinythemes)
library(highcharter)

## Load ETF symbols
# symbolv <- rutils::etfenv$symbolv
# symbol <- "VTI"

## Setup for Polygon
startd <- as.Date("1990-01-01")
# startd <- as.Date("2022-03-01")
endd <- Sys.Date()
tspan <- "day"
# tspan <- "minute"
apikey <- "UJcr9ctoMBXEBK1Mqu_KQAkUuBxLvEtE"

## Load S&P500 symbols
sp500table <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv")
symbolv <- sp500table$Ticker
symbol <- "AAPL"

lookb <- 1e2

# Setup end

## Create elements of the user interface
interface <- fluidPage(
  theme=shinytheme("paper"),
  
  # Title
  h3("HighCharts Candlestick Plot of OHLC Stock Prices"),
  
  # Widgets
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol:", choices=symbolv, selected=symbol)),
    # Input time span
    column(width=2, selectInput("tspan", label="Time span:", choices=c("day", "minute"), selected="day")),
    # Input lambda decay parameter
    column(width=3, sliderInput("lambda", label="lambda:", min=0.01, max=0.99, value=0.2, step=0.01))
  ),  # end fluidRow

  # Output plot panel
  highchartOutput("hchart", width="80%", height="600px")

)  # end fluidPage interface

## Define the server function
server <- function(input, output) {
  
  # Load the data
  ohlc <- shiny::reactive({
    
    symbol <- input$symbol
    tspan <- input$tspan
    # Set lookb to 30 days if time span = minute
    if (tspan == "minute")
      lookb <- 30
    startd <- Sys.Date() - lookb
    
    cat("Loading data for ", symbol, "\n")
    
    # Load ETF prices
    # ohlc <- get(input$symbol, rutils::etfenv)
    # Load stock prices from Polygon
    ohlc <- rutils::getpoly(symbol=symbol, startd=startd, tspan=tspan, apikey=apikey)
    ohlc[, c("Open", "High", "Low", "Close", "VWAP")] <- log(ohlc[, c("Open", "High", "Low", "Close", "VWAP")])
    ohlc
    
  })  # end Load the data
  
  # Return the Highchart plot to output argument
  output$hchart <- renderHighchart({
    
    ohlc <- ohlc()
    
    # Copy shiny parameters
    lambda <- input$lambda
    # lookb <- input$lookb
    
    # Calculate the running mean prices
    closep <- quantmod::Cl(ohlc)
    means <- HighFreq::run_mean(closep, lambda=lambda)
    means <- xts::xts(means, zoo::index(ohlc))
    # Plot highchart
    highcharter::highchart(type="stock") %>% 
      hc_add_series(ohlc, type="candlestick") %>% 
      hc_add_series(means, yAxis=0, name="MA", color="red")

  })
  
}  # end server code


## Run the Shiny app
shinyApp(ui=interface, server=server)

