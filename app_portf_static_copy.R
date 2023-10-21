##############################
# This is a shiny app for calculating the returns of 
# a static portfolio of ETFs defined by the weights input
# by the user.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

# Select ETFs
# retv <- rutils::etfenv$returns
# symbolv <- colnames(retv)
# symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "IEF"))]
symbolv <- c("VTI", "VXX", "SVXY")
retv <- na.omit(rutils::etfenv$returns[, symbolv])


# End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(paste("Static Portfolio of ETFs")),
  
  # Create single row with two slider inputs
  fluidRow(
    # Input weights
    column(width=2, sliderInput("weight1", label=paste0("Weight for ", symbolv[1], ":"),
                                min=-2, max=2, value=1, step=0.1)),
    column(width=2, sliderInput("weight2", label=paste0("Weight for ", symbolv[2], ":"),
                                min=-2, max=2, value=-2, step=0.1)),
    column(width=2, sliderInput("weight3", label=paste0("Weight for ", symbolv[3], ":"),
                                min=-2, max=2, value=-2, step=0.1))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Recalculate the data and rerun the model
  pnls <- shiny::reactive({
    # get model parameters from input argument
    weight1 <- input$weight1
    weight2 <- input$weight2
    weight3 <- input$weight3
    
    weights <- c(weight1, weight2, weight3)
    (retv %*% weights)
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    pnls <- pnls()
    # Variance ratio
    # tre_nd <- HighFreq::calcvar_ag(pnls, lagg)/HighFreq::calcvar_ag(pnls)/lagg
    pnls <- xts::xts(cumsum(pnls), zoo::index(retv))
    dygraphs::dygraph(pnls, main="Static Portfolio of ETFs")
    # colnamev <- colnames(pnls())
    # dygraphs::dygraph(pnls(), main="ETF Portfolio Optimization") %>%
    #   dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
    #   dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
    #   dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="red") %>%
    #   dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="blue")
  })  # end output plot
  
  # output$dygraph <- dygraphs::renderDygraph({
  #   dygraph(pnls(), main="Rolling Portfolio Optimization Strategy") %>%
  #     dySeries("strategy", label="strategy", strokeWidth=1, color="red")
  # })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
