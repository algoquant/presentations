##############################
# This is a shiny app for backtesting a market making strategy.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# load packages
library(rutils)
library(dygraphs)

# Model and data setup
# source the model function
# source("C:/Develop/R/lecture_slides/scripts/roll_portf_new.R")
# max_eigen <- 2

# Load the trading function written as an eWrapper:
# source("C:/Develop/R/IBrokers2/R/trade_wrapper.R")

# data_dir <- "C:/Develop/data/ib_data"
# setwd(dir=data_dir)
# load("oh_lc.RData")
# oh_lc <- rutils::etf_env$VTI
oh_lc <- HighFreq::SPY
n_rows <- NROW(oh_lc)
ohlc_lag <- rutils::lag_it(oh_lc)
pric_e <- as.numeric(Cl(oh_lc))
std_dev <- sqrt(HighFreq::roll_variance(oh_lc=oh_lc, look_back=5))
std_dev <- rutils::lag_it(as.numeric(std_dev))


# End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("Market Making Strategy"),

  # create single row with two slider inputs
  fluidRow(
    column(width=3, sliderInput("buy_spread", label="buy spread:",
                                min=0.0, max=0.1, value=0.001, step=0.001)),
    column(width=3, sliderInput("sell_spread", label="sell spread:",
                                min=0.0, max=0.1, value=0.001, step=0.001))
  ),  # end fluidRow

  # Render plot in panel
  mainPanel(dygraphOutput("dy_graph"), width=12)
)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {

  # re-calculate the data and rerun the model
  da_ta <- reactive({
    # get model parameters from input argument
    buy_spread <- input$buy_spread
    sell_spread <- input$sell_spread

    # Run the trading model (strategy):
    buy_price <- (1 - buy_spread*std_dev)*as.numeric(Lo(ohlc_lag))
    sell_price <- (1 + sell_spread*std_dev)*as.numeric(Hi(ohlc_lag))

    buy_ind <- (as.numeric(Lo(oh_lc)) < buy_price)
    n_buy <- cumsum(buy_ind)
    sell_ind <- (as.numeric(Hi(oh_lc)) > sell_price)
    n_sell <- cumsum(sell_ind)

    buy_s <- numeric(n_rows)
    buy_s[buy_ind] <- buy_price[buy_ind]
    buy_s <- cumsum(buy_s)
    sell_s <- numeric(n_rows)
    sell_s[sell_ind] <- sell_price[sell_ind]
    sell_s <- cumsum(sell_s)

    pnl_s <- ((sell_s-buy_s) - pric_e*(n_sell-n_buy))
    pnl_s <- cbind(pnl_s, Cl(oh_lc))
    colnames(pnl_s) <- c("Strategy", "Index")
    pnl_s[c(1, rutils::calc_endpoints(oh_lc, inter_val="days")), ]
    # xts::xts(pnl_s, index(oh_lc))
  })  # end reactive code

  output$dy_graph <- renderDygraph({
    col_names <- colnames(da_ta())
    dygraphs::dygraph(da_ta(), main="Market Making Strategy") %>%
      dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
      dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
      dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue")
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
