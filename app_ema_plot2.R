##############################
# This is a shiny app for plotting an EMA moving average.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)


## Load daily ETF prices

envv <- rutils::etfenv
symbolv <- sort(get("symbolv", envv))
symboln <- "QQQ"
captiont <- "Exponential Moving Average (EMA) Prices"

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symboln", label="Symbol", choices=symbolv, selected=symboln)),
    # Input look-back interval
    column(width=2, sliderInput("lambdaf", label="Decay factor", min=0.5, max=0.99, value=0.9, step=0.01))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="700px")

)  # end fluidPage interface


## Define the server function
servfun <- shiny::shinyServer(function(input, output) {
  
  # Get the close prices in a reactive environment
  pricev <- shiny::reactive({
    cat("Getting the close prices\n")
    # Get the data
    ohlc <- get(input$symboln, envv)
    pricev <- log(quantmod::Cl(ohlc["2020-01/2020-05"]))
    # Return the prices
    return(pricev)
  })  # end reactive code
  
  # Calculate the EMA indicator in a reactive environment
  pricema <- shiny::reactive({
    cat("Calculating the EMA indicator\n")
    # Get model parameters from input argument
    lambdaf <- input$lambdaf
    # Calculate EMA prices recursively using RcppArmadillo
    pricev <- pricev()
    pricema <- HighFreq::run_mean(pricev, lambdaf=lambdaf)
    pricema[is.na(pricema)] <- 0
    # Return the plot data
    pricema <- cbind(pricev, pricema)
    colnames(pricema) <- c(input$symboln, "EMA Prices")
    pricema
  })  # end reactive code
  
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    cat("Plotting the dygraph\n")
    colnamev <- colnames(pricema())
    ##  Create dygraph plot
    dyplot <- dygraph(pricema(), main=paste(colnamev, collapse=" ")) %>%
      dySeries(name=colnamev[1], strokeWidth=2, color="blue") %>%
      dySeries(name=colnamev[2], strokeWidth=2, color="red")
    
    ##  Return the dygraph object
    return(dyplot)

  })  # end output plot
  
})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
