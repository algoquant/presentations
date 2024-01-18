##############################
# This is a shiny app for cointegration.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

# Load S&P500 constituent stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
symbolv <- sort(names(sp500env))
symbolstock <- "MSFT"
symboletf <- "VTI"

captiont <- "Cointegration of Stocks vs ETFs"

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  # fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
    # column(width=12,
    #        h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
    #        actionButton("recalcb", "Recalculate the Model"))
  # ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbolstock", label="Stock Symbol:", choices=symbolv, selected=symbolstock)),
    # Input ETF symbol
    column(width=2, selectInput("symboletf", label="ETF Symbol:", choices=rutils::etfenv$symbolv, selected=symboletf)),
    # Input look-back interval
    column(width=2, sliderInput("betav", label="Beta", min=0.5, max=3.0, value=1.5, step=0.1))
    # Input exponent for variance
    # column(width=2, sliderInput("exponent", label="Std Dev exponent:",
    #                             min=0.25, max=2.5, value=1.0, step=0.05)),
    # Input weights
    # column(width=2, sliderInput("weights", label="VTI weight:", min=0.4, max=0.6, value=0.5, step=0.01))
    # Input lag trade parameter
    # column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1)),
    # Input threshold interval
    # column(width=2, sliderInput("threshold", label="threshold", min=0.5, max=3.0, value=1.0, step=0.1))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="80%", height="600px")
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Load the prices
  pricev <- shiny::reactive({
    
    # Get model parameters from input argument
    symbolstock <- input$symbolstock
    symboletf <- input$symboletf
    cat("Loading data for ", symbolstock, " and ", symboletf, "\n")

    # Load stock prices from sp500env
    ohlc <- get(symbolstock, sp500env)
    closep <- log(quantmod::Cl(ohlc))
    # Load ETF prices from rutils::etfenv
    ohlc <- get(symboletf, rutils::etfenv)
    closetf <- log(quantmod::Cl(ohlc))
    pricev <- na.omit(cbind(closep, closetf))
    colnames(pricev) <- c(symbolstock, symboletf)
    pricev
    
  })  # end Load the data
  
  # Calculate the portfolio residuals
  residuals <- shiny::reactive({
    cat("Calculating the portfolio residuals\n")
    
    # Get model parameters from input argument
    betav <- input$betav

    # Calculate the prices
    closep <- pricev()[, 1]
    closetf <- pricev()[, 2]
    # Calculate regression coefficients of XLB ~ XLE
    # Calculate regression residuals
    alpha <- (mean(closep) - betav*mean(closetf))
    residuals <- (closep - alpha - betav*closetf)
    colnames(residuals) <- paste0(symbolstock, " vs ", symboletf)
    residuals

  })  # end reactive code
  
  
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    residuals <- residuals()
    
    # Perform ADF test on residuals
    adftest <- tseries::adf.test(residuals, k=1)

    captiont <- paste0("Cointegration: ", colnames(residuals), " / ADF p-value = ", round(adftest$p.value, 3))

    # Plot log wealths
    dygraphs::dygraph(residuals, main=captiont) %>%
      dyOptions(colors=c("blue"), strokeWidth=2) %>%
      dyLegend(show="always", width=500)
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
