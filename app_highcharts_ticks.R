##############################
# This is a shiny app for a HighCharts plot of SPY tick prices.
# Just press the "Run App" button on the upper right of RStudio.
# Or run the app in MacBook terminal:  
#   Rscript -e "shiny::runApp("Users/jerzy/Develop/Presentations/app_highcharts_ticks.R", port=7775)"
# On mini6 run: 
#   Rscript -e "shiny::runApp("Users/jerzy/Develop/R/app_highcharts_ticks.R", host="0.0.0.0", port=7775)"
# And in browser load the page: http://206.72.197.242:7775
##############################


## Below is the setup code that runs once when the shiny app is started

# Load packages
library(quantmod)
library(shiny)
library(shinythemes)
library(highcharter)

## Load SPY ticks

load(file="/Users/jerzy/Develop/data/spy_ticks.RData")
# load(file="/Users/danielsavage/Public/dans_public/spyticks.RData")

# Setup end

## Create elements of the user interface
uifun <- shiny::fluidPage(
  theme=shinythemes::shinytheme("paper"),
  
  # Title
  h3("HighCharts Plot of SPY Tick Prices"),
  
  # Widgets
  # fluidRow(
  #   # Input stock symbol
  #   column(width=2, selectInput("symbol", label="Symbol:", choices=symbolv, selected=symbol)),
  #   # Input time span
  #   column(width=2, selectInput("tspan", label="Time span:", choices=c("day", "minute"), selected="day")),
  #   # Input the look-back interval
  #   column(width=2, numericInput("lookb", label="Look-back days:", value=1000, step=1))
  # ),  # end fluidRow

  # Output plot panel
  highchartOutput("hchart", width="80%", height="600px")

)  # end fluidPage interface

## Define the server function
servfun <- function(input, output) {
  
  # Return the Highchart plot to output argument
  output$hchart <- renderHighchart({
    
    # Copy shiny parameters
    # tspan <- input$tspan
    # lookb <- input$lookb
    
    # Set lookb to 30 days if time span = minute
    # if (tspan == "minute")
    #   lookb <- 30
    # 
    # startd <- Sys.Date() - lookb
    
    # Load ETF prices
    # ohlc <- get(input$symbol, rutils::etfenv)
    # Load stock prices from Polygon
    # ohlc <- rutils::getpoly(symbol=input$symbol, startd=startd, tspan=tspan, apikey=apikey)
    # Plot highchart
    highcharter::hchart(spyticks$price)
    # highcharter::highchart(type="stock") %>% hc_add_series(ohlc, type="candlestick")

  })
  
}  # end server code


## Run the Shiny app
shiny::shinyApp(ui=uifun, server=servfun)

