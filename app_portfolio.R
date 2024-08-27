##############################
# This is a shiny app for a portfolio of stocks
# with fixed betas.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

## Load SPY minute prices
# pricev <- lapply(4:7, function(x) {
#   load(paste0("/Users/jerzy/Develop/data/SPY_minute_20240", x, ".RData"))
#   do.call(rbind, pricel)
# }) # end lapply
# pricev <- do.call(rbind, pricev)
# pricespy <- pricev
## Load SVXY minute prices
pricev <- lapply(4:7, function(x) {
  load(paste0("/Users/jerzy/Develop/data/SVXY_minute_20240", x, ".RData"))
  do.call(rbind, pricel)
}) # end lapply
pricev <- do.call(rbind, pricev)
price1 <- pricev
# Load VXX minute prices
pricev <- lapply(4:7, function(x) {
  load(paste0("/Users/jerzy/Develop/data/VXX_minute_20240", x, ".RData"))
  do.call(rbind, pricel)
}) # end lapply
pricev <- do.call(rbind, pricev)
price2 <- pricev

## Load daily stock prices
# retp <- na.omit(rutils::etfenv$returns[, c("SVXY", "VXX")])
# # Cumulative returns means daily rebalancing of the portfolio
# price1 <- cumsum(retp$SVXY)
# price2 <- cumsum(retp$VXX)

# Load S&P500 constituent stock prices
# symbolv <- rutils::etfenv$symbolv
symbol1 <- colnames(price1)
symbol2 <- colnames(price2)

label1 <- paste("Beta", symbol1)
label2 <- paste("Beta", symbol2)


captiont <- paste("Portfolio of", symbol1, "and", symbol2)

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
    # column(width=2, selectInput("symbol1", label="Stock Symbol:", choices=symbolv, selected=symbol1)),
    # Input ETF symbol
    # column(width=2, selectInput("symbol2", label="ETF Symbol:", choices=rutils::etfenv$symbolv, selected=symbol2)),
    # Input look-back interval
    column(width=2, sliderInput("beta1", label=label1, min=0.0, max=10.0, value=5.0, step=0.1)),
    # Input look-back interval
    column(width=2, sliderInput("beta2", label=label2, min=0.0, max=10.0, value=5.0, step=0.1)),
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
  
  # Calculate the portfolio priceport
  priceport <- shiny::reactive({
    cat("Calculating the portfolio prices\n")
    
    # Get model parameters from input argument
    # beta1 <- input$beta1

    # Calculate the prices
    # Calculate regression coefficients of XLB ~ XLE
    # Calculate regression residuals
    # alpha <- (mean(price1) + beta1*mean(price2))
    # priceport <- (pricespy - input$beta1*price1 - input$beta2*price2)
    # priceport <- (-input$beta1*price1 - input$beta2*price2)
    # colnames(priceport) <- paste0(symbol1, " vs ", symbol2)
    priceport <- (-input$beta1*price1 - input$beta2*price2)
    colnames(priceport) <- "Portfolio"
    priceport

  })  # end reactive code
  
  
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    priceport <- priceport()
    # symbol1 <- colnames(pricespy)
    symbol1 <- "SPY"
    symbol2 <- colnames(price1)
    symbol3 <- colnames(price2)
    
    # Perform ADF test on priceport
    # adftest <- tseries::adf.test(priceport, k=1)

    # captiont <- paste0("Cointegration: ", colnames(priceport), " / ADF p-value = ", round(adftest$p.value, 3))
    captiont <- paste0("Portfolio of ", symbol1, " vs ", symbol2, " and ", symbol3)
    
    # Plot log wealths
    dygraphs::dygraph(priceport, main=captiont) %>%
      dyOptions(colors=c("blue"), strokeWidth=1) %>%
      dyLegend(show="always", width=500)
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
