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
# re_turns <- rutils::etf_env$re_turns
# sym_bols <- colnames(re_turns)
# sym_bols <- sym_bols[!(sym_bols %in% c("VXX", "SVXY", "MTUM", "IEF"))]
sym_bols <- c("VTI", "VXX", "SVXY")
re_turns <- na.omit(rutils::etf_env$re_turns[, sym_bols])


# End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(paste("Static Portfolio of ETFs")),
  
  # Create single row with two slider inputs
  fluidRow(
    # Input weights
    column(width=2, sliderInput("weight1", label=paste0("Weight for ", sym_bols[1], ":"),
                                min=-2, max=2, value=1, step=0.1)),
    column(width=2, sliderInput("weight2", label=paste0("Weight for ", sym_bols[2], ":"),
                                min=-2, max=2, value=-2, step=0.1)),
    column(width=2, sliderInput("weight3", label=paste0("Weight for ", sym_bols[3], ":"),
                                min=-2, max=2, value=-2, step=0.1))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph"), width=12)
)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {

  # Recalculate the data and rerun the model
  pnl_s <- reactive({
    # get model parameters from input argument
    weight1 <- input$weight1
    weight2 <- input$weight2
    weight3 <- input$weight3
    
    weight_s <- c(weight1, weight2, weight3)
    (re_turns %*% weight_s)
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph({
    pnl_s <- pnl_s()
    # Variance ratio
    # tre_nd <- HighFreq::calc_var_ag(pnl_s, lagg)/HighFreq::calc_var_ag(pnl_s)/lagg
    pnl_s <- xts::xts(cumsum(pnl_s), zoo::index(re_turns))
    dygraphs::dygraph(pnl_s, main="Static Portfolio of ETFs")
    # col_names <- colnames(pnl_s())
    # dygraphs::dygraph(pnl_s(), main="ETF Portfolio Optimization") %>%
    #   dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
    #   dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
    #   dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
    #   dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue")
  })  # end output plot
  
  # output$dygraph <- dygraphs::renderDygraph({
  #   dygraph(pnl_s(), main="Rolling Portfolio Optimization Strategy") %>%
  #     dySeries("strategy", label="strategy", strokeWidth=1, color="red")
  # })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
