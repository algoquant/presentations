##############################
# This is a shiny app for a HighCharts candlestick plot of OHLC stock prices.
# Just press the "Run App" button on the upper right of RStudio.
# Or run the app in MacBook terminal:  
#   Rscript -e "shiny::runApp("~/Develop/Presentations/app_highcharts.R", port=7775)"
# On mini6 run: 
#   Rscript -e "shiny::runApp("~/Develop/R/app_highcharts.R", host="0.0.0.0", port=7775)"
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

sp500table <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv")
symbolv <- sp500table$Ticker
symbol <- "AAPL"


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
    # Input the look-back interval
    column(width=2, numericInput("look_back", label="Look-back days:", value=1000, step=1))
  ),  # end fluidRow

  # Output plot panel
  highchartOutput("hchart", width="80%", height="600px")

)  # end fluidPage interface

## Define the server function
server <- function(input, output) {
  
  # Return the Highchart plot to output argument
  output$hchart <- renderHighchart({
    
    # Copy shiny parameters
    tspan <- input$tspan
    look_back <- input$look_back
    
    # Set look_back to 30 days if time span = minute
    if (tspan == "minute")
      look_back <- 30
    
    startd <- Sys.Date() - look_back
    
    # Load ETF prices
    # ohlc <- get(input$symbol, rutils::etfenv)
    # Load stock prices from Polygon
    ohlc <- rutils::getpoly(symbol=input$symbol, startd=startd, tspan=tspan, apikey=apikey)
    # Plot highchart
    highcharter::highchart(type="stock") %>% hc_add_series(ohlc, type="candlestick")

  })
  
}  # end server code


## Run the Shiny app
shinyApp(ui=interface, server=server)

