##############################
# This is a shiny app for plotting an EWMA moving average.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)


## Set up ETF data

data_env <- rutils::etfenv
symbolv <- get("symbolv", data_env)
symbol <- "VTI"

## End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel("EWMA Moving Average"),
  
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol",
                                choices=symbolv, selected=symbol)),
    # Input look-back interval
    column(width=2, sliderInput("lambda", label="Decay parameter",
                                min=0.1, max=0.99, value=0.9, step=0.01))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")

)  # end fluidPage interface


## Define the server function
servfun <- shiny::shinyServer(function(input, output) {
  
  # Get the close prices in a reactive environment
  closep <- shiny::reactive({
    cat("Getting the close prices\n")
    # Get the data
    ohlc <- get(input$symbol, data_env)
    closep <- log(quantmod::Cl(ohlc))
    # Return the data
    closep
  })  # end reactive code
  
  # Calculate the EWMA indicator in a reactive environment
  ewmap <- shiny::reactive({
    cat("Calculating the EWMA indicator\n")
    # Get model parameters from input argument
    lambda <- input$lambda
    # Calculate EWMA prices recursively using RcppArmadillo
    closep <- closep()
    ewmap <- HighFreq::run_mean(closep, lambda=lambda)
    ewmap[is.na(ewmap)] <- 0
    # Return the plot data
    datav <- cbind(closep, ewmap)
    colnames(datav) <- c(input$symbol, "EWMA")
    datav
  })  # end reactive code
  
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    cat("Plotting the dygraph\n")
    colnamev <- colnames(ewmap())
    dygraphs::dygraph(ewmap(), main=paste(colnamev[1], "EWMA")) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")
  })  # end output plot
})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
