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
load("C:/Develop/lecture_slides/data/sp500_prices.RData")
# re_turns <- returns_100
re_turns <- re_turns[, !is.na(re_turns[NROW(re_turns), ])]
re_turns <- re_turns[, !is.na(re_turns[NROW(re_turns)-1000, ])]
re_turns <- na.omit(re_turns)
n_weights <- NCOL(re_turns)
# risk_free <- 0.03/260
# ex_cess <- (re_turns - risk_free)
# calculate returns on equal weight portfolio
# in_dex <- xts(cumsum(re_turns %*% rep(1/sqrt(n_weights), n_weights)), index(re_turns))


# End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("Visualize Weights for S&P500 Portfolio"),
  
  # create single row with two slider inputs
  fluidRow(
    # Input number of eigenvalues for regularized matrix inverse
    column(width=4, numericInput("max_eigen", "Number of eigenvalues:", value=5)),
    # Input end points interval
    # column(width=4, selectInput("inter_val", label="End points Interval",
    #             choices=c("weeks", "months", "years"), selected="months")),
    # Input look-back interval
    # column(width=4, sliderInput("look_back", label="Lookback interval:",
    #                             min=1, max=30, value=12, step=1)),
    # Input end_stub interval
    # column(width=4, sliderInput("end_stub", label="End_stub interval:",
    #                             min=1, max=90, value=30, step=1)),
    # Input the shrinkage intensity
    # column(width=4, sliderInput("al_pha", label="Shrinkage intensity:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05))
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(plotOutput("plo_t"), width=12)
)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {

  # re-calculate the data and rerun the model
  da_ta <- reactive({
    # get model parameters from input argument
    # inter_val <- input$inter_val
    max_eigen <- input$max_eigen
    # look_back <- input$look_back
    # end_stub <- input$end_stub
    # al_pha <- input$al_pha
    
    # Define end points
    # end_points <- rutils::calc_endpoints(re_turns, inter_val=inter_val)
    # end_points <- ifelse(end_points<(n_weights+1), n_weights+1, end_points)
    # end_points <- end_points[end_points > (n_weights+1)]
    # len_gth <- NROW(end_points)
    # Define start_points
    # start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
    # rerun the model
    weight_s = HighFreq::calc_weights(re_turns, max_eigen=max_eigen);
    
    # pnl_s <- roll_portf_n(ex_cess=re_turns, 
    #                               re_turns=re_turns,
    #                               start_points=start_points-1,
    #                               end_points=end_points-1,
    #                       max_eigen=max_eigen, 
    #                       al_pha=al_pha,
    #                       min_var=FALSE)
    # pnl_s[which(is.na(pnl_s)), ] <- 0
    # pnl_s <- roll_portf_r(ex_cess, re_turns, start_points, end_points, al_pha, max_eigen, end_stub)
    # pnl_s <- sd(rutils::diff_it(in_dex))*pnl_s/sd(rutils::diff_it(pnl_s))
    # pnl_s <- cumsum(pnl_s)
    # pnl_s <- cbind(pnl_s, in_dex)
    # colnames(pnl_s) <- c("Strategy", "Index")
    # pnl_s[c(1, end_points), ]
    sort(weight_s)
  })  # end reactive code
  
  # return to output argument a dygraph plot with two y-axes
  output$plo_t <- renderPlot({
    plot(da_ta())
  })  # end renderPlot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
