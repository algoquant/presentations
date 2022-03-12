##############################
# This is a shiny app for simulating a VWAP moving 
# average crossover strategy, with dygraphs plot.
# Just press the "Run App" button on the upper right of RStudio.
# Or run the app in terminal:  Rscript app_highcharts.R
##############################


## Below is the setup code that runs once when the shiny app is started

# Load packages
library(rutils)
library(shiny)
library(shinythemes)
library(highcharter)

## Get ETFs
# symbolv <- rutils::etfenv$symbolv
# symbol <- "VTI"

## Get stocks from Polygon
# startd <- as.Date("1990-01-01")
startd <- as.Date("2022-03-01")
endd <- Sys.Date()
# tspan <- "day"
tspan <- "minute"
apikey <- "UJcr9ctoMBXEBK1Mqu_KQAkUuBxLvEtE"

sp500table <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv")
symbolv <- sp500table$Ticker
symbol <- "AAPL"


# Setup end

## Create elements of the user interface
interface <- fluidPage(
  theme=shinytheme("paper"),
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol", choices=symbolv, selected=symbol))
  ),  # end fluidRow

  # Create output plot panel
  highchartOutput("hchart", width="90%", height="600px")

)  # end fluidPage interface

## Define the server function
server <- function(input, output) {
  
  # Return the Highchart plot to output argument
  output$hchart <- renderHighchart({
    
    # ohlc <- get(input$symbol, rutils::etfenv)
    ohlc <- rutils::getpoly(symbol=input$symbol, startd=startd, tspan=tspan, apikey=apikey)
    highcharter::highchart(type="stock") %>% hc_add_series(ohlc, type="candlestick")

  })
  
}  # end server code


## Run the Shiny app
shinyApp(ui=interface, server=server)

