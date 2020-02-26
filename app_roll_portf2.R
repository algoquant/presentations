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
sym_bols <- colnames(rutils::etf_env$re_turns)
sym_bols <- sym_bols[!((sym_bols=="VXX")|(sym_bols=="SVXY"))]
n_weights <- NROW(sym_bols)
re_turns <- rutils::etf_env$re_turns[, sym_bols]
re_turns <- zoo::na.locf(re_turns)
re_turns <- na.omit(re_turns)
risk_free <- 0.03/260
ex_cess <- re_turns - risk_free
# calculate equal weight portfolio
in_dex <- cumsum(re_turns %*% rep(1/sqrt(NCOL(re_turns)), NCOL(re_turns)))

# Define end_points
end_points <- rutils::calc_endpoints(re_turns, inter_val="months")
end_points <- end_points[end_points > (n_weights+1)]
len_gth <- NROW(end_points)

# End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("Rolling Portfolio Optimization Strategy for 19 ETFs"),
  
  # create single row with two slider inputs
  fluidRow(
    # input look_back interval
    column(width=5, sliderInput("look_back", label="lookback interval (months):",
                                min=6, max=30, value=12, step=1)),
    # input the shrinkage intensity
    column(width=5, sliderInput("al_pha", label="shrinkage intensity alpha:",
                                min=0.01, max=0.99, value=0.1, step=0.05))
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(dygraphOutput("dy_graph"), width=12)
)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {

  # re-calculate the data and rerun the model
  da_ta <- reactive({
    # get model parameters from input argument
    look_back <- input$look_back
    al_pha <- input$al_pha
    # define start_points
    start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
    # rerun the model
    pnl_s <- cbind(
      roll_portf_r(ex_cess, re_turns, start_points, end_points, al_pha, max_eigen), 
      in_dex)  # end cbind
    colnames(pnl_s) <- c("strategy", "equal weight")
    pnl_s
  })  # end reactive code
  
  # return the dygraph plot to output argument
  output$dy_graph <- renderDygraph({
    dygraph(da_ta(), main="Rolling Portfolio Optimization Strategy") %>%
      dySeries("strategy", label="strategy", strokeWidth=1, color="red")
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
