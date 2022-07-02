##############################
# This is a shiny app for a Dygraph plot of SPY tick prices.
# Just press the "Run App" button on the upper right of RStudio.
# Or run the app in MacBook terminal:  
#   Rscript -e "shiny::runApp("Users/jerzy/Develop/Presentations/app_dygraphs_ticks.R", port=7775)"
# On mini6 run: 
#   Rscript -e "shiny::runApp("Users/jerzy/Develop/R/app_dygraphs_ticks.R", host="0.0.0.0", port=7775)"
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

maxsize <- quantile(spyticks$size, 0.99)
maxsizem <- median(spyticks$size)

# Setup end

## Create elements of the user interface
uifun <- shiny::fluidPage(
  theme=shinythemes::shinytheme("paper"),
  
  # Title
  h3("Dygraph Plot of SPY Tick Prices"),
  
  # Widgets
  fluidRow(
    # Input minimum trade size
    column(width=2, sliderInput("minsize", label="Minimum trade size:",
                                min=1, max=maxsizem, value=1, step=1)),
    # Input maximum trade size
    column(width=2, sliderInput("maxsize", label="Maximum trade size:",
                                min=100, max=maxsize, value=(maxsize %/% 2), step=1))
    #   # Input stock symbol
    #   column(width=2, selectInput("symbol", label="Symbol:", choices=symbolv, selected=symbol)),
    #   # Input time span
    #   column(width=2, selectInput("tspan", label="Time span:", choices=c("day", "minute"), selected="day")),
    #   # Input the look-back interval
    #   column(width=2, numericInput("minsize", label="Look-back days:", value=1000, step=1))
  ),  # end fluidRow
  
  # Output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="550px")
  # highchartOutput("hchart", width="80%", height="600px")
  
)  # end fluidPage interface

## Define the server function
servfun <- function(input, output) {
  
  # Recalculate the prices
  pricev <- shiny::reactive({
    cat("Calculating prices \n")
    # Get model parameters from input argument
    minsize <- input$minsize
    maxsize <- input$maxsize

    pricev <- spyticks
    # Remove prices with very small size
    pricev <- pricev[(pricev$size >= minsize), ]
    # Remove prices with very large size
    pricev <- pricev[(pricev$size <= maxsize), ]
    
    pricev
    
  })  # end reactive code
  
  
  # Return the Dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    
    # Copy shiny parameters
    # tspan <- input$tspan
    # minsize <- input$minsize
    
    # Set minsize to 30 days if time span = minute
    # if (tspan == "minute")
    #   minsize <- 30
    # 
    # startd <- Sys.Date() - minsize
    
    # Load ETF prices
    # ohlc <- get(input$symbol, rutils::etfenv)
    # Load stock prices from Polygon
    # ohlc <- rutils::getpoly(symbol=input$symbol, startd=startd, tspan=tspan, apikey=apikey)
    # Plot Dygraph
    dygraphs::dygraph(pricev()$price, main="SPY Tick Prices") %>%
      dyLegend(width=500)
    # highcharter::hchart(spyticks$price)
    # highcharter::highchart(type="stock") %>% hc_add_series(ohlc, type="candlestick")
    
  })
  
}  # end server code


## Run the Shiny app
shiny::shinyApp(ui=uifun, server=servfun)

