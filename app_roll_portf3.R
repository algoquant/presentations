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
# max_eigen <- 2
symbolv <- colnames(rutils::etfenv$returns)
symbolv <- symbolv[-match(c("VXX", "SVXY", "MTUM"), symbolv)]
nweights <- NROW(symbolv)
returns <- rutils::etfenv$returns[, symbolv]
returns <- zoo::na.locf(returns)
returns <- na.omit(returns)
riskf <- 0.03/260
excess <- (returns - riskf)
# calculate returns on equal weight portfolio
indeks <- cumsum(returns %*% rep(1/sqrt(nweights), nweights))

# End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel("Rolling Portfolio Optimization Strategy for 19 ETFs"),
  
  # create single row with two slider inputs
  fluidRow(
    # Input number of eigenvalues for regularized matrix inverse
    column(width=4, numericInput("max_eigen", "Number of eigenvalues:", value=round(nweights/2))),
    # Input end points interval
    column(width=4, selectInput("interval", label="End points Interval",
                choices=c("weeks", "months", "years"), selected="months")),
    # Input look-back interval
    column(width=4, sliderInput("look_back", label="Lookback interval:",
                                min=1, max=30, value=12, step=1)),
    # Input the shrinkage intensity
    column(width=4, sliderInput("alpha", label="Shrinkage intensity:",
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
    interval <- input$interval
    max_eigen <- input$max_eigen
    look_back <- input$look_back
    alpha <- input$alpha
    
    # Define end points
    endpoints <- rutils::calc_endpoints(returns, interval=interval)
    # endpoints <- ifelse(endpoints<(nweights+1), nweights+1, endpoints)
    endpoints <- endpoints[endpoints > (nweights+1)]
   .n_rows <- NROW(endpoints)
    # Define startpoints
    startpoints <- c(rep_len(1, look_back-1), endpoints[1:.n_rows-look_back+1)])
    # rerun the model
    pnls <- roll_portf_r(excess, returns, startpoints, endpoints, alpha, max_eigen)
    # pnls <- sd(rutils::diffit(indeks))*pnls/sd(rutils::diffit(pnls))
    pnls <- cbind(pnls, indeks)
    colnames(pnls) <- c("strategy", "equal weight")
    pnls
  })  # end reactive code
  
  # return to output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    colnamev <- colnames(datav())
    dygraphs::dygraph(datav(), main="Rolling Portfolio Optimization Strategy") %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue")
  })  # end output plot
  
  # output$dygraph <- dygraphs::renderDygraph({
  #   dygraph(datav(), main="Rolling Portfolio Optimization Strategy") %>%
  #     dySeries("strategy", label="strategy", strokeWidth=1, color="red")
  # })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfunc)
