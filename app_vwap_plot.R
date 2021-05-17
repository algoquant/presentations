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

data_env <- rutils::etf_env
sym_bols <- get("sym_bols", data_env)
sym_bol <- "VTI"

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("VWAP Moving Average"),
  
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("sym_bol", label="Symbol",
                                choices=sym_bols, selected=sym_bol)),
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Lookback interval",
                                min=1, max=150, value=11, step=1))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph"), width=12)
  
)  # end fluidPage interface


## Define the server function
ser_ver <- shiny::shinyServer(function(input, output) {
  
  # Get the close and volume data in a reactive environment
  clos_e <- shiny::reactive({
    cat("Getting the close and volume data\n")
    # Get the data
    oh_lc <- get(input$sym_bol, data_env)
    clos_e <- log(quantmod::Cl(oh_lc))
    vol_ume <- quantmod::Vo(oh_lc)
    # Return the data
    cbind(clos_e, vol_ume)
  })  # end reactive code
  
  # Calculate the VWAP indicator in a reactive environment
  v_wap <- shiny::reactive({
    cat("Calculating the VWAP indicator\n")
    # Get model parameters from input argument
    look_back <- input$look_back
    # Calculate the VWAP indicator
    clos_e <- clos_e()[, 1]
    vol_ume <- clos_e()[, 2]
    v_wap <- HighFreq::roll_sum(se_ries=clos_e*vol_ume, look_back=look_back)
    volume_rolling <- HighFreq::roll_sum(se_ries=vol_ume, look_back=look_back)
    v_wap <- v_wap/volume_rolling
    v_wap[is.na(v_wap)] <- 0
    # Return the plot data
    da_ta <- cbind(clos_e, v_wap)
    colnames(da_ta) <- c(input$sym_bol, "VWAP")
    da_ta
  })  # end reactive code
  
  # Return the dygraph plot to output argument
  output$dy_graph <- dygraphs::renderDygraph({
    cat("Plotting the dygraph\n")
    col_names <- colnames(v_wap())
    dygraphs::dygraph(v_wap(), main=paste(col_names[1], "VWAP")) %>%
      dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
      dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
      dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
  })  # end output plot
})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
