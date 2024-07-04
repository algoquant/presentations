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
source("/Users/jerzy/Develop/R/scripts/market_making.R")

data_dir <- "/Users/jerzy/Develop/data/ib_data"
setwd(dir=data_dir)
# for SPY
# ohlc <- HighFreq::SPY
symbol <- "ES"
tick_size <- 0.25
load(paste0(symbol, "_ohlc.RData"))
nrows <- NROW(ohlc)
ohlc_data <- coredata(ohlc)
retv <- rutils::diffit(ohlc_data[, 4])
ohlc_lag <- rutils::lagit(ohlc_data)
colnamev <- c("Strategy PnL", "Inventory", "Realized PnL", "Unrealized PnL", "EWMA")
# calculate EWMA variance using filter()
# lookb <- 11
# weights <- exp(-0.1*1:lookb)
# weights <- weights/sum(weights)
# stdev <- stats::filter((ohlc_data[, 2]-ohlc_data[, 3])^2, filter=weights, sides=1)
# stdev[1:(lookb-1)] <- stdev[lookb]
# stdev <- sqrt(stdev)


# End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(paste0("Market Making Strategy for ", symbol)),

  # create single row with two slider inputs
  fluidRow(
    column(width=2, sliderInput("buy_spread", label="spread:",
                                min=0.0, max=10*tick_size, value=3*tick_size, step=tick_size)),
    # column(width=2, sliderInput("sell_spread", label="sell spread:",
    #                             min=0.0, max=10*tick_size, value=3*tick_size, step=tick_size)),
    column(width=2, sliderInput("lookb", label="lookb:",
                                min=3, max=211, value=111, step=1)),
    column(width=2, sliderInput("threshold", label="threshold:",
                                min=0.0, max=4.0, value=1.0, step=0.01)),
    column(width=2, sliderInput("lagg", label="lag:",
                                min=0, max=10, value=2, step=1)),
    column(width=2, sliderInput("lambda", label="lambda:",
                                min=0.0, max=0.9, value=0.05, step=0.01)),
    column(width=2, sliderInput("invent_limit", label="inventory limit:",
                                min=5, max=100, value=50, step=1)),
    # Select output series
    column(width=2, selectInput("output", label="output series:",
                                choices=colnamev, selected=colnamev[1]))
    # for SPY
    # column(width=2, sliderInput("buy_spread", label="buy spread:",
    #                             min=0.0, max=0.1, value=0.001, step=0.001)),
    # column(width=2, sliderInput("sell_spread", label="sell spread:",
    #                             min=0.0, max=0.1, value=0.001, step=0.001))
  ),  # end fluidRow

  # Render plot in panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
  # plotOutput("plotobj")
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Recalculate the data and rerun the model
  datav <- shiny::reactive({
    # Get model parameters from input argument
    # buy_spread <- input$buy_spread
    # sell_spread <- buy_spread
    # lagg <- input$lagg

    # Run the trading model (strategy):
    # pnls <- make_market(ohlc=ohlc_data,
    #                      ohlc_lag=rutils::lagit(ohlc_data, lagg=input$lagg),
    #                      buy_spread=input$buy_spread,
    #                      sell_spread=input$buy_spread,
    #                      lambda=input$lambda,
    #                      invent_limit=input$invent_limit,
    #                      warm_up=100)

    # pnls <- make_market_ewma(ohlc=ohlc_data,
    #                           ohlc_lag=rutils::lagit(ohlc_data, lagg=input$lagg),
    #                           buy_spread=input$buy_spread,
    #                           sell_spread=input$buy_spread,
    #                           lambda=input$lambda,
    #                           invent_limit=input$invent_limit,
    #                           lagg=input$lagg,
    #                           warm_up=100)

    pnls <- trade_median(retv=retv, ohlc=ohlc_data,
                          ohlc_lag=rutils::lagit(ohlc_data, lagg=input$lagg),
                          lookb=input$lookb, 
                          threshold=input$threshold, 
                          buy_spread=input$buy_spread,
                          sell_spread=input$buy_spread,
                          lambda=input$lambda,
                          invent_limit=input$invent_limit)
    
    # Output
    endp <- c(1, rutils::calc_endpoints(ohlc, interval="minutes"))
    colnamev <- c(colnames(ohlc)[4], input$output)
    xts::xts(pnls[endp, colnamev], index(ohlc[endp]))
  })  # end reactive code

  output$dyplot <- dygraphs::renderDygraph({
    colnamev <- colnames(datav())
    dygraphs::dygraph(datav(), main=paste0("Market Making Strategy for ", symbol)) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[2], axis="y", strokeWidth=1, col="red") %>%
      dySeries(name=colnamev[1], axis="y2", strokeWidth=1, col="blue")
  })  # end output plot
  # output$plotobj <- shiny::renderPlot({
  #   plot(datav(), t="l", main="Market Making Strategy")
  # })  # end renderPlot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
