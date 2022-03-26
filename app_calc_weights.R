##############################
# This is a shiny app for visualizing calc_weights
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# load packages
library(HighFreq)
library(shiny)
library(dygraphs)
# Rcpp::sourceCpp(file="C:/Develop/lecture_slides/assignments/rcpp_strat.cpp")
# Model and data setup
# source the model function
# source("C:/Develop/lecture_slides/scripts/roll_portf_new.R")
# max_eigen <- 2
load("/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# returns <- returns100
returns <- returns[, !is.na(returns[NROW(returns), ])]
returns <- returns[, !is.na(returns[NROW(returns)-1000, ])]
returns <- na.omit(returns)
nweights <- NCOL(returns)
# riskf <- 0.03/260
# excess <- (returns - riskf)
# calculate returns on equal weight portfolio
# indeks <- xts(cumsum(returns %*% rep(1/sqrt(nweights), nweights)), index(returns))


# End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel("Visualize Weights for S&P500 Portfolio"),
  
  # create single row with two slider inputs
  fluidRow(
    # Input number of eigenvalues for regularized matrix inverse
    column(width=4, numericInput("max_eigen", "Number of eigenvalues:", value=5)),
    # Input end points interval
    # column(width=4, selectInput("interval", label="End points Interval",
    #             choices=c("weeks", "months", "years"), selected="months")),
    # Input look-back interval
    # column(width=4, sliderInput("look_back", label="Lookback interval:",
    #                             min=1, max=30, value=12, step=1)),
    # Input end_stub interval
    # column(width=4, sliderInput("end_stub", label="End_stub interval:",
    #                             min=1, max=90, value=30, step=1)),
    # Input the shrinkage intensity
    # column(width=4, sliderInput("alpha", label="Shrinkage intensity:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05))
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(plotOutput("plotobj"), width=12)
)  # end fluidPage interface


## Define the server code
servfunc <- function(input, output) {

  # Recalculate the data and rerun the model
  datav <- reactive({
    # get model parameters from input argument
    # interval <- input$interval
    max_eigen <- input$max_eigen
    # look_back <- input$look_back
    # end_stub <- input$end_stub
    # alpha <- input$alpha
    
    # Define end points
    # endpoints <- rutils::calc_endpoints(returns, interval=interval)
    # endpoints <- ifelse(endpoints<(nweights+1), nweights+1, endpoints)
    # endpoints <- endpoints[endpoints > (nweights+1)]
    # nrows <- NROW(endpoints)
    # Define startpoints
    # startpoints <- c(rep_len(1, look_back-1), endpoints[1:(nrows-look_back+1)])
    # rerun the model
    weights = HighFreq::calc_weights(returns, max_eigen=max_eigen);
    
    # pnls <- roll_portf_n(excess=returns, 
    #                               returns=returns,
    #                               startpoints=startpoints-1,
    #                               endpoints=endpoints-1,
    #                       max_eigen=max_eigen, 
    #                       alpha=alpha,
    #                       min_var=FALSE)
    # pnls[which(is.na(pnls)), ] <- 0
    # pnls <- roll_portf_r(excess, returns, startpoints, endpoints, alpha, max_eigen, end_stub)
    # pnls <- sd(rutils::diffit(indeks))*pnls/sd(rutils::diffit(pnls))
    # pnls <- cumsum(pnls)
    # pnls <- cbind(pnls, indeks)
    # colnames(pnls) <- c("Strategy", "Index")
    # pnls[c(1, endpoints), ]
    sort(weights)
  })  # end reactive code
  
  # return to output argument a dygraph plot with two y-axes
  output$plotobj <- shiny::renderPlot({
    plot(datav())
  })  # end renderPlot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfunc)
