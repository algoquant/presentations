##############################
# This is a shiny app for backtesting a market making strategy.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load packages
library(rutils)
library(dygraphs)

## Model and data setup
# Load the trading function
source("C:/Develop/R/scripts/market_making.R")

data_dir <- "C:/Develop/data/ib_data"
setwd(dir=data_dir)
# for SPY
# oh_lc <- HighFreq::SPY
load("QM_ohlc.RData")
n_rows <- NROW(oh_lc)
ohlc_data <- coredata(oh_lc)
ohlc_lag <- rutils::lag_it(ohlc_data)

# calculate EWMA variance using filter()
look_back <- 11
weight_s <- exp(-0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
std_dev <- stats::filter((ohlc_data[, 2]-ohlc_data[, 3])^2, filter=weight_s, sides=1)
std_dev[1:(look_back-1)] <- std_dev[look_back]
std_dev <- sqrt(std_dev)


# End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("Market Making Strategy"),

  # create single row with two slider inputs
  fluidRow(
    column(width=3, sliderInput("buy_spread", label="buy spread:",
                                min=0.0, max=0.5, value=0.05, step=0.025)),
    column(width=3, sliderInput("sell_spread", label="sell spread:",
                                min=0.0, max=0.5, value=0.05, step=0.025)),
    column(width=3, sliderInput("lagg", label="lag:",
                                min=0.0, max=10, value=3, step=1))
    # for SPY
    # column(width=3, sliderInput("buy_spread", label="buy spread:",
    #                             min=0.0, max=0.1, value=0.001, step=0.001)),
    # column(width=3, sliderInput("sell_spread", label="sell spread:",
    #                             min=0.0, max=0.1, value=0.001, step=0.001))
  ),  # end fluidRow

  # Render plot in panel
  mainPanel(dygraphOutput("dy_graph"), width=12)
  # plotOutput("plo_t")
)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {

  # re-calculate the data and rerun the model
  da_ta <- reactive({
    # get model parameters from input argument
    buy_spread <- input$buy_spread
    sell_spread <- input$sell_spread
    lagg <- input$lagg

    # Run the trading model (strategy):
    pnl_s <- make_market(oh_lc=ohlc_data, ohlc_lag=rutils::lag_it(ohlc_data, lagg=lagg),
                         buy_spread=buy_spread, sell_spread=sell_spread)
    end_points <- c(1, rutils::calc_endpoints(oh_lc, inter_val="minutes"))
    xts::xts(pnl_s[end_points, c(4, 5)], index(oh_lc[end_points]))
  })  # end reactive code

  output$dy_graph <- renderDygraph({
    col_names <- colnames(da_ta())
    dygraphs::dygraph(da_ta(), main="Market Making Strategy") %>%
      dyAxis("y", label=col_names[2], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[1], independentTicks=TRUE) %>%
      dySeries(name=col_names[2], axis="y", label=col_names[2], strokeWidth=1, col="red") %>%
      dySeries(name=col_names[1], axis="y2", label=col_names[1], strokeWidth=1, col="blue")
  })  # end output plot
  # output$plo_t <- renderPlot({
  #   plot(da_ta(), t="l", main="Market Making Strategy")
  # })  # end renderPlot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
