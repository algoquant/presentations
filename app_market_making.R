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
sym_bol <- "ES"
tick_size <- 0.25
load(paste0(sym_bol, "_ohlc.RData"))
n_rows <- NROW(oh_lc)
ohlc_data <- coredata(oh_lc)
re_turns <- rutils::diff_it(ohlc_data[, 4])
ohlc_lag <- rutils::lag_it(ohlc_data)
col_names <- c("Strategy PnL", "Inventory", "Realized PnL", "Unrealized PnL", "EWMA")
# calculate EWMA variance using filter()
# look_back <- 11
# weight_s <- exp(-0.1*1:look_back)
# weight_s <- weight_s/sum(weight_s)
# std_dev <- stats::filter((ohlc_data[, 2]-ohlc_data[, 3])^2, filter=weight_s, sides=1)
# std_dev[1:(look_back-1)] <- std_dev[look_back]
# std_dev <- sqrt(std_dev)


# End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(paste0("Market Making Strategy for ", sym_bol)),

  # create single row with two slider inputs
  fluidRow(
    column(width=2, sliderInput("buy_spread", label="spread:",
                                min=0.0, max=10*tick_size, value=3*tick_size, step=tick_size)),
    # column(width=2, sliderInput("sell_spread", label="sell spread:",
    #                             min=0.0, max=10*tick_size, value=3*tick_size, step=tick_size)),
    column(width=2, sliderInput("look_back", label="look_back:",
                                min=3, max=211, value=111, step=1)),
    column(width=2, sliderInput("thresh_old", label="threshold:",
                                min=0.0, max=4.0, value=1.0, step=0.01)),
    column(width=2, sliderInput("lagg", label="lag:",
                                min=0, max=10, value=2, step=1)),
    column(width=2, sliderInput("lamb_da", label="lambda:",
                                min=0.0, max=0.9, value=0.05, step=0.01)),
    column(width=2, sliderInput("invent_limit", label="inventory limit:",
                                min=5, max=100, value=50, step=1)),
    # Select output series
    column(width=2, selectInput("out_put", label="output series:",
                                choices=col_names, selected=col_names[1]))
    # for SPY
    # column(width=2, sliderInput("buy_spread", label="buy spread:",
    #                             min=0.0, max=0.1, value=0.001, step=0.001)),
    # column(width=2, sliderInput("sell_spread", label="sell spread:",
    #                             min=0.0, max=0.1, value=0.001, step=0.001))
  ),  # end fluidRow

  # Render plot in panel
  mainPanel(dygraphs::dygraphOutput("dy_graph"), width=12)
  # plotOutput("plo_t")
)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {

  # Recalculate the data and rerun the model
  da_ta <- reactive({
    # Get model parameters from input argument
    # buy_spread <- input$buy_spread
    # sell_spread <- buy_spread
    # lagg <- input$lagg

    # Run the trading model (strategy):
    # pnl_s <- make_market(ohlc=ohlc_data,
    #                      ohlc_lag=rutils::lag_it(ohlc_data, lagg=input$lagg),
    #                      buy_spread=input$buy_spread,
    #                      sell_spread=input$buy_spread,
    #                      lamb_da=input$lamb_da,
    #                      invent_limit=input$invent_limit,
    #                      warm_up=100)

    # pnl_s <- make_market_ewma(ohlc=ohlc_data,
    #                           ohlc_lag=rutils::lag_it(ohlc_data, lagg=input$lagg),
    #                           buy_spread=input$buy_spread,
    #                           sell_spread=input$buy_spread,
    #                           lamb_da=input$lamb_da,
    #                           invent_limit=input$invent_limit,
    #                           lagg=input$lagg,
    #                           warm_up=100)

    pnl_s <- trade_median(re_turns=re_turns, ohlc=ohlc_data,
                          ohlc_lag=rutils::lag_it(ohlc_data, lagg=input$lagg),
                          look_back=input$look_back, 
                          thresh_old=input$thresh_old, 
                          buy_spread=input$buy_spread,
                          sell_spread=input$buy_spread,
                          lamb_da=input$lamb_da,
                          invent_limit=input$invent_limit)
    
    # Output
    end_points <- c(1, rutils::calc_endpoints(oh_lc, inter_val="minutes"))
    col_names <- c(colnames(oh_lc)[4], input$out_put)
    xts::xts(pnl_s[end_points, col_names], index(oh_lc[end_points]))
  })  # end reactive code

  output$dy_graph <- dygraphs::renderDygraph({
    col_names <- colnames(da_ta())
    dygraphs::dygraph(da_ta(), main=paste0("Market Making Strategy for ", sym_bol)) %>%
      dyAxis("y", label=col_names[2], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[1], independentTicks=TRUE) %>%
      dySeries(name=col_names[2], axis="y", label=col_names[2], strokeWidth=1, col="red") %>%
      dySeries(name=col_names[1], axis="y2", label=col_names[1], strokeWidth=1, col="blue")
  })  # end output plot
  # output$plo_t <- shiny::renderPlot({
  #   plot(da_ta(), t="l", main="Market Making Strategy")
  # })  # end renderPlot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
