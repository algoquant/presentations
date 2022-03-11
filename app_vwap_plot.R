##############################
# This is a shiny app for plotting a VWAP moving average.
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
  titlePanel("VWAP Moving Average"),
  
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol",
                                choices=symbolv, selected=symbol)),
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Lookback interval",
                                min=1, max=150, value=11, step=1))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
  
)  # end fluidPage interface


## Define the server function
servfunc <- shiny::shinyServer(function(input, output) {
  
  # Get the close and volume data in a reactive environment
  closep <- shiny::reactive({
    cat("Getting the close and volume data\n")
    # Get the data
    ohlc <- get(input$symbol, data_env)
    closep <- log(quantmod::Cl(ohlc))
    volumes <- quantmod::Vo(ohlc)
    # Return the data
    cbind(closep, volumes)
  })  # end reactive code
  
  # Calculate the VWAP indicator in a reactive environment
  vwapv <- shiny::reactive({
    cat("Calculating the VWAP indicator\n")
    # Get model parameters from input argument
    look_back <- input$look_back
    # Calculate the VWAP indicator
    closep <- closep()[, 1]
    volumes <- closep()[, 2]
    vwapv <- HighFreq::roll_sum(tseries=closep*volumes, look_back=look_back)
    volume_rolling <- HighFreq::roll_sum(tseries=volumes, look_back=look_back)
    vwapv <- vwapv/volume_rolling
    vwapv[is.na(vwapv)] <- 0
    # Return the plot data
    datav <- cbind(closep, vwapv)
    colnames(datav) <- c(input$symbol, "VWAP")
    datav
  })  # end reactive code
  
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    cat("Plotting the dygraph\n")
    colnamev <- colnames(vwapv())
    dygraphs::dygraph(vwapv(), main=paste(colnamev[1], "VWAP")) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")
  })  # end output plot
})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfunc)
