##############################
# This is a shiny app for simulating a VWAP moving 
# average crossover strategy, with a dygraphs plot.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Source the backtest functions
source("C:/Develop/R/scripts/backtest_functions.R")

## Set up ETF data
# if (!("etf_env" %in% search()))
#   attach(etf_env)
# if (!("etf_env" %in% ls()))
#   load(file="C:/Develop/lecture_slides/data/etf_data.RData")
# data_env <- "etf_env"
# sym_bols <- etf_env$sym_bols
# sym_bol <- "SVXY"
# re_turns <- etf_env$re_turns

data_env <- rutils::etf_env
sym_bols <- get("sym_bols", data_env)
sym_bol <- "SVXY"
# sym_bols <- rutils::etf_env$sym_bols


## Set up S&P500 data
# if (!("sp500_env" %in% search()))
#   attach(sp500_env)
# if (!("sp500_env" %in% ls())) {
#   load(file="C:/Develop/lecture_slides/data/sp500.RData")
# }  # end if
# data_env <- sp500_env
# sym_bols <- names(data_env)
# # sym_bols <- c("PG", "CDNS", "YUM", "YUMC", "KHC", "SNPS", "ODFL", "CHRW", "AWK", "SO", "EA", "FIS", "DG", "BAX", "HRL", "MSFT", "XOM", "BSX", "JNJ", "CLX", "CL", "MCD", "WMT", "SBUX", "LLY", "ADM", "BIO", "XLNX", "ATVI", "DISH", "K", "SHW", "SIG", "CSCO", "INTU", "VRTX", "FB", "ORCL", "DUK", "KSS", "ROP", "AKAM", "MXIM", "TXN", "NEM", "COST", "EL", "JWN", "ACN", "FISV", "KLAC", "PFE", "TYL", "BIIB", "MCHP", "BBBY", "DRE", "PEP", "LIN", "NKE", "TROW", "LEN", "HOLX", "NVR", "UDR", "WEC", "DHI", "NI")
# sym_bol <- "YUM"


## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("VWAP Moving Average Crossover Strategy"),
  
  # create single row with four slider inputs
  fluidRow(
    # Input stock symbol
    column(width=3, selectInput("sym_bol", label="Symbol",
                                choices=sym_bols, selected=sym_bol)),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback interval",
                                min=1, max=150, value=4, step=1)),
    # Input lag trade parameter
    column(width=3, sliderInput("lagg", label="Confirmation signals", min=1, max=5, value=2, step=1)),
    # Input lag trade parameter
    column(width=3, sliderInput("thresh_old", label="Threshold", min=0.01, max=1.5, value=0.05, step=0.01)),
    # Input trending or reverting (contrarian) strategy
    column(width=3, selectInput("co_eff", label="Trend (1) Revert (-1)",
                                choices=c(1, -1), selected=(1)))
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(dygraphOutput("dy_graph"), width=12)
)  # end fluidPage interface


## Define the server code
ser_ver <- shiny::shinyServer(function(input, output) {

  # re-calculate the data and rerun the model
  da_ta <- reactive({
    # Get model parameters from input argument
    sym_bol <- input$sym_bol
    look_back <- input$look_back
    lagg <- input$lagg
    thresh_old <- input$thresh_old
    co_eff <- as.numeric(input$co_eff)

    # Prepare data
    oh_lc <- get(sym_bol, data_env)
    clo_se <- log(quantmod::Cl(oh_lc))
    star_t <- as.numeric(clo_se[1, ])
    # Run model and calculate strategy profits and losses
    pnl_s <- backtest_ewma(oh_lc, look_back=look_back, lagg=lagg, thresh_old=thresh_old, co_eff=co_eff)
    # position_s <- pnl_s[ ,"positions"]
    v_wap <- pnl_s[ ,"vwap"]
    pnl_s <- star_t + cumsum(pnl_s[ ,"pnls"])
    pnl_s <- cbind(clo_se, pnl_s, v_wap)
    colnames(pnl_s) <- c(sym_bol, "strategy", "vwap")
    pnl_s
  })  # end reactive code
  
  # return the dygraph plot to output argument
  output$dy_graph <- renderDygraph({
    col_names <- colnames(da_ta())
    dygraphs::dygraph(da_ta(), main=paste(col_names[1], "Strategy")) %>%
      dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
      dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
      dySeries(name=col_names[3], axis="y", label=col_names[3], strokeWidth=2, col="green") %>%
      dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
