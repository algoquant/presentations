##############################
# This is a shiny app for backtesting a strategy based on the Z-scores
# of the residuals of running regressions.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Source the backtest functions
source("/Users/jerzy/Develop/R/backtest_functions.R")


## Set up ETF data
# if (!("etfenv" %in% search()))
#   attach(etfenv)
# if (!("etfenv" %in% ls()))
#   load(file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")
# data_env <- "etfenv"
# symbolv <- etfenv$symbolv
# symbol <- "SVXY"
# retv <- etfenv$returns

data_env <- rutils::etfenv
symbolv <- get("symbolv", data_env)
symbol <- "XLK"
# symbolv <- rutils::etfenv$symbolv


## Set up S&P500 data
# if (!("sp500env" %in% search()))
#   attach(sp500env)
# if (!("sp500env" %in% ls())) {
#   load(file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
# }  # end if
# data_env <- sp500env
# symbolv <- names(data_env)
# # symbolv <- c("PG", "CDNS", "YUM", "YUMC", "KHC", "SNPS", "ODFL", "CHRW", "AWK", "SO", "EA", "FIS", "DG", "BAX", "HRL", "MSFT", "XOM", "BSX", "JNJ", "CLX", "CL", "MCD", "WMT", "SBUX", "LLY", "ADM", "BIO", "XLNX", "ATVI", "DISH", "K", "SHW", "SIG", "CSCO", "INTU", "VRTX", "FB", "ORCL", "DUK", "KSS", "ROP", "AKAM", "MXIM", "TXN", "NEM", "COST", "EL", "JWN", "ACN", "FISV", "KLAC", "PFE", "TYL", "BIIB", "MCHP", "BBBY", "DRE", "PEP", "LIN", "NKE", "TROW", "LEN", "HOLX", "NVR", "UDR", "WEC", "DHI", "NI")
# symbol <- "YUM"


## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Strategy Based on the Z-scores of Running Regressions"),
  
  # create single row with four slider inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol", choices=symbolv, selected=symbol)),
    # Input lambda decay parameter
    column(width=3, sliderInput("lambda", label="lambda:", min=0.01, max=0.99, value=0.2, step=0.01)),
    # Input lag trade parameter
    column(width=2, sliderInput("lagg", label="Confirmation signals", min=1, max=5, value=1, step=1)),
    # Input lag trade parameter
    column(width=2, sliderInput("threshold", label="Threshold", min=0.01, max=1.5, value=1.1, step=0.1)),
    # Input trending or reverting (contrarian) strategy
    column(width=2, selectInput("coeff", label="Trend (1) Revert (-1)", choices=c(1, -1), selected=(-1)))
  ),  # end fluidRow
  
  # create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  # mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface


## Define the server code
servfun <- shiny::shinyServer(function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Recalculate the data and rerun the model
  datav <- shiny::reactive({
    # Get model parameters from input argument
    symbol <- input$symbol
    lambda <- input$lambda
    lagg <- input$lagg
    threshold <- input$threshold
    coeff <- as.numeric(input$coeff)

    # Prepare data
    ohlc <- get(symbol, data_env)
    closep <- log(quantmod::Cl(ohlc))
    # startd <- as.numeric(closep[1, ])
    # rangev <- (log(quantmod::Hi(ohlc)) - log(quantmod::Lo(ohlc)))
    # Run model from /Users/jerzy/Develop/R/backtest_functions.R
    pnls <- backtest_zscores(ohlc, lambda=lambda, lagg=lagg, threshold=threshold, coeff=coeff)
    posit <- pnls[ ,"positions"]
    
    # Calculate number of trades
    values$ntrades <- sum(abs(rutils::diffit(posit)) > 0)
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(cbind(rutils::diffit(closep), pnls[ ,"pnls"]), function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    
    pnls <- cumsum(pnls[ ,"pnls"])
    # pnls <- cbind(closep, pnls, closep + coeff*posit*rangev)
    # colnames(pnls) <- c(symbol, "Strategy", "Positions")
    pnls <- cbind(closep, pnls)
    colnames(pnls) <- c(symbol, "Strategy")
    pnls
  })  # end reactive code
  
  # return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    pnls <- datav()
    colnamev <- colnames(pnls)
    
    captiont <- paste("Strategy for", input$symbol, "Regression Z-score / \n", 
                      paste0(c("Index SR=", "Strategy SR="), values$sharper, collapse=" / "), "/ \n",
                      "Number of trades=", values$ntrades)
    
    dygraphs::dygraph(pnls, main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
      # dySeries(name=colnamev[3], axis="y", label=colnamev[3], strokeWidth=2, col="green") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
      dyLegend(show="always", width=500)
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
