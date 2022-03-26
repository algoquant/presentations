##############################
# This is a shiny app for simulating a rolling portfolio 
# optimization strategy, which produces an interactive 
# dygraphs plot.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# load packages
library(shiny)
library(dygraphs)
library(rutils)

# Model and data setup
# source the model function
source("C:/Develop/lecture_slides/scripts/roll_portf.R")
max_eigen <- 2
symbolv <- colnames(rutils::etfenv$returns)
symbolv <- symbolv[!((symbolv=="VXX")|(symbolv=="SVXY"))]
nweights <- NROW(symbolv)
returns <- rutils::etfenv$returns[, symbolv]
returns <- zoo::na.locf(returns)
returns <- na.omit(returns)
riskf <- 0.03/260
excess <- returns - riskf
# calculate equal weight portfolio
indeks <- cumsum(returns %*% rep(1/sqrt(NCOL(returns)), NCOL(returns)))

# Define endpoints
endpoints <- rutils::calc_endpoints(returns, interval="months")
endpoints <- endpoints[endpoints > (nweights+1)]
nrows <- NROW(endpoints)

# End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel("Rolling Portfolio Optimization Strategy for 19 ETFs"),
  
  # create single row with two slider inputs
  fluidRow(
    # input look_back interval
    column(width=5, sliderInput("look_back", label="lookback interval (months):",
                                min=6, max=30, value=12, step=1)),
    # input the shrinkage intensity
    column(width=5, sliderInput("alpha", label="shrinkage intensity alpha:",
                                min=0.01, max=0.99, value=0.1, step=0.05))
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface


## Define the server code
servfunc <- function(input, output) {

  # Recalculate the data and rerun the model
  datav <- reactive({
    # get model parameters from input argument
    look_back <- input$look_back
    alpha <- input$alpha
    # define startpoints
    startpoints <- c(rep_len(1, look_back-1), endpoints[1:(nrows-look_back+1)])
    # rerun the model
    pnls <- cbind(
      roll_portf_r(excess, returns, startpoints, endpoints, alpha, max_eigen), 
      indeks)  # end cbind
    colnames(pnls) <- c("strategy", "equal weight")
    pnls
  })  # end reactive code
  
  # return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    dygraph(datav(), main="Rolling Portfolio Optimization Strategy") %>%
      dySeries("strategy", label="strategy", strokeWidth=1, color="red")
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfunc)
