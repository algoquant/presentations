##############################
# This is a shiny app for plotting an EMA moving average.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)


## Set up ETF data

datenv <- rutils::etfenv
symbolv <- get("symbolv", datenv)
symboln <- "VTI"

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("VTI EMA Prices"),
  
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symboln", label="Symbol",
                                choices=symbolv, selected=symboln)),
    # Input look-back interval
    column(width=2, sliderInput("lambda", label="Lambda decay factor",
                                min=0.5, max=0.99, value=0.9, step=0.01))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")

)  # end fluidPage interface


## Define the server function
servfun <- shiny::shinyServer(function(input, output) {
  
  # Get the close prices in a reactive environment
  pricev <- shiny::reactive({
    cat("Getting the close prices\n")
    # Get the data
    ohlc <- get(input$symboln, datenv)
    pricev <- log(quantmod::Cl(ohlc["2008/2009"]))
    # Return the data
    pricev
  })  # end reactive code
  
  # Calculate the EMA indicator in a reactive environment
  pricema <- shiny::reactive({
    cat("Calculating the EMA indicator\n")
    # Get model parameters from input argument
    lambda <- input$lambda
    # Calculate EMA prices recursively using RcppArmadillo
    pricev <- pricev()
    pricema <- HighFreq::run_mean(pricev, lambda=lambda)
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
    dygraph(pricema(), main=paste(colnamev, collapse=" ")) %>%
      dySeries(name=colnamev[1], strokeWidth=2, color="blue") %>%
      dySeries(name=colnamev[2], strokeWidth=2, color="red")
  })  # end output plot
})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
