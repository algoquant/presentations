##############################
# This is a shiny app for simulating a VWAP moving 
# average crossover strategy, with dygraphs plot.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)


## Set up ETF data
# if (!("etfenv" %in% search()))
#   attach(etfenv)
# if (!("etfenv" %in% ls()))
#   load(file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")
# datenv <- "etfenv"
# symbolv <- etfenv$symbolv
# symbol <- "SVXY"
# retv <- etfenv$returns

datenv <- rutils::etfenv
symbolv <- sort(get("symbolv", datenv))
symbol <- "SVXY"
# symbolv <- rutils::etfenv$symbolv


## Set up S&P500 data
# if (!("sp500env" %in% search()))
#   attach(sp500env)
# if (!("sp500env" %in% ls())) {
#   load(file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
# }  # end if
# datenv <- sp500env
# symbolv <- names(datenv)
# symbolv <- c("PG", "CDNS", "YUM", "YUMC", "KHC", "SNPS", "ODFL", "CHRW", "AWK", "SO", "EA", "FIS", "DG", "BAX", "HRL", "MSFT", "XOM", "BSX", "JNJ", "CLX", "CL", "MCD", "WMT", "SBUX", "LLY", "ADM", "BIO", "XLNX", "ATVI", "DISH", "K", "SHW", "SIG", "CSCO", "INTU", "VRTX", "FB", "ORCL", "DUK", "KSS", "ROP", "AKAM", "MXIM", "TXN", "NEM", "COST", "EL", "JWN", "ACN", "FISV", "KLAC", "PFE", "TYL", "BIIB", "MCHP", "BBBY", "DRE", "PEP", "LIN", "NKE", "TROW", "LEN", "HOLX", "NVR", "UDR", "WEC", "DHI", "NI")
# symbol <- "YUM"


## End setup code


## Create elements of the user interface
interface <- shiny::fluidPage(
  titlePanel("VWAP Crossover Strategy app_vwap_strat.R"),
  
  # Create single row with four slider inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol",
                                choices=symbolv, selected=symbol)),
    # Input look-back interval
    # column(width=2, sliderInput("lookb", label="Lookback interval", min=1, max=150, value=4, step=1)),
    # Input lambda parameter
    column(width=3, sliderInput("lambda", label="lambda:", min=0.01, max=0.99, value=0.37, step=0.01)),
    # Input lag trade parameter
    column(width=2, sliderInput("lagg", label="Confirmation signals", min=1, max=5, value=2, step=1)),
    # Input trend or revert
    column(width=2, selectInput("coeff", label="Trend (1) Revert (-1)",
                                choices=c(1, -1), selected=(1)))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="95%", height="600px")
  
)  # end fluidPage interface


## Define the server function
server <- shiny::shinyServer(function(input, output) {

  ## Create an empty list of reactive values.
  globals <- reactiveValues()
  
  # Recalculate the data and rerun the model
  datav <- shiny::reactive({
    # Get model parameters from input argument
    symbol <- input$symbol
    # lookb <- input$lookb
    lambda <- input$lambda
    lagg <- input$lagg
    coeff <- as.numeric(input$coeff)

    # Prepare data
    # symbol <- "SVXY"
    # symbol2 <- "VXX"
    ohlc <- get(symbol, datenv)
    # ohlc2 <- get(symbol2, datenv)
    pricev <- log(quantmod::Cl(ohlc))
    startd <- as.numeric(prices[1])
    # retv <- na.omit(get(symbol, retv))
    retv <- rutils::diffit(pricev)
    # returns2 <- na.omit(get(symbol2, retv))
    # com_bined <- cbind(retv, -na.omit(retv2))
    # which_na <- which(is.na(com_bined$VXX))
    # com_bined$VXX[which_na] <- com_bined$SVXY[which_na]
    # closep <- cumprod(1+rowMeans(com_bined))
    # closep <- quantmod::Cl(ohlc)
    
    # sum(is.na(com_bined))
    # head(com_bined)
    # volumes <- cbind(quantmod::Vo(ohlc), quantmod::Vo(etfenv$VXX))
    # volumes$VXX.Volume[which_na] <- volumes$SVXY.Volume[which_na]
    # volumes <- rowMeans(volumes)
    volumes <- quantmod::Vo(ohlc)
    
    # Simulate strategy
    vwapv <- HighFreq::run_mean(pricev, lambda=lambda, weightv=volumes)

    # Calculate VWAP indicator
    indic <- sign(pricev - vwapv)
    # indic_lag <- rutils::lagit(indic, lagg=1)
    # Flip position only if the indic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # posv <- ifelse(indic == indic_lag, indic, posv)
    indics <- HighFreq::roll_sum(tseries=matrix(indic), lookb=lagg)
    indics[1:lagg] <- 0
    posv <- rep(NA_integer_, NROW(pricev))
    posv[1] <- 0
    posv <- ifelse(indics == lagg, 1, posv)
    posv <- ifelse(indics == (-lagg), -1, posv)
    posv <- zoo::na.locf(posv, na.rm=FALSE)
    # posv[1:lagg] <- 0
    # Calculate indicator of flipping the positions
    indic <- rutils::diffit(posv)
    # Calculate number of trades
    globals$ntrades <- sum(abs(indic)>0)
    
    # Add buy/sell indicators for annotations
    longi <- (indic > 0)
    shorti <- (indic < 0)
    # Lag the positions to trade in next period
    posv <- rutils::lagit(posv, lagg=1)
    # Calculate log strategy returns
    # retv <- rutils::diffit(pricev)
    # Calculate strategy profits and losses
    pnls <- coeff*returns*posv
    # Scale the pnls so they have same SD as returns
    pnls <- pnls*sd(retv[returns<0])/sd(pnls[pnls<0])
    
    pnls <- cbind(retv, pnls)
    colnames(pnls) <- c(symbol, "Strategy")
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    globals$sharper <- round(sharper, 3)

    pnls <- cumsum(pnls)
    # pnls <- cbind(pnls, vwapv)
    # colnames(pnls)[3] <- "VWAP"
    
    # pnls <- cumprod(1+pnls)
    pnls
  })  # end reactive code
  
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    
    colnamev <- colnames(datav())
    
    # Get Sharpe ratios
    sharper <- globals$sharper
    # Get number of trades
    ntrades <- globals$ntrades
    
    captiont <- paste(input$symbol, "VWAP strategy / \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
                      "Number of trades=", ntrades)
    
    dygraphs::dygraph(datav(), main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
      # dySeries(name=colnamev[3], axis="y", strokeWidth=2, col="green") %>%
      dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red")
  })  # end output plot

})  # end server code


## Run the Shiny app
shiny::shinyApp(ui=interface, server=server)

