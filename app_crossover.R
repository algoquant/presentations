##############################
# This is a shiny app for backtesting an EWMA crossover strategy.
# The stock position is equal to minus the sign of the excess price.
# If the excess price is positive then the position is -$1 of stock, 
# and vice versa.
# Version for dual EWMA:
#   The excess price is equal to the fast EWMA price minus the slow EWMA.
#   The model depends on two lambda decay parameters used to calculate the 
#   trailing average prices - fast lambda (small) and slow lambda (large).
# Version for single EWMA:
#   The excess price is equal to the current price minus the trailing mean price.
#   The model depends on a single lambda decay parameter used to calculate 
#   the trailing average prices.
# 
# Just press the "Run App" button on the upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Load daily ETF prices
# symbolns <- sort(colnames(rutils::etfenv$prices))
# symboln <- "XLK"
# pricev <- log(na.omit(get(symboln, rutils::etfenv$prices)))

## Load daily stock prices
# symbolns <- sort(colnames(prices))
# symboln <- "AAPL"
# pricev <- log(na.omit(rutils::etfenv$prices$VTI))

## Calculate the intraday VTI returns
# ohlc <- log(rutils::etfenv$VTI)
# openp <- quantmod::Op(ohlc)
# highp <- quantmod::Hi(ohlc)
# lowp <- quantmod::Lo(ohlc)
# closep <- quantmod::Cl(ohlc)
# retd <- (closep - openp)
# colnames(retd) <- "VTI"
# pricev <- cumsum(retd)

# dataf <- "Days"


## Load intraday tick stock prices

load(file="/Users/jerzy/Develop/data/SPY_minute_2023.RData")
# load(file="/Users/jerzy/Develop/data/SPY_minute.RData")
# load(file="/Users/jerzy/Develop/data/AAPL_minute.RData")
# load(file="/Users/jerzy/Develop/data/MSFT_minute.RData")
# load(file="/Users/jerzy/Develop/data/AAPL_minute_market.RData")
# load(file="/Users/jerzy/Develop/data/SVXY_minute_bars_2022_2023.RData")
# pricev <- log(quantmod::Cl(ohlc))
# load(file="/Users/jerzy/Develop/data/SPY_minute_bars_2019_2023.RData")
# pricev <- log(quantmod::Cl(ohlc))
# Load SPY OHLC minute bars
# pricev <- log(quantmod::Cl(HighFreq::SPY))
# colnames(pricev) <- "SPY"
dataf <- "Minutes"

# load(file="/Users/jerzy/Develop/data/SPY_second_2023.RData")
# load(file="/Users/jerzy/Downloads/temp/SPY_20230922.RData")
# dataf <- "Seconds"
# load(file="/Users/jerzy/Develop/data/SPY_5second_202306.RData")
# dataf <- "5-Seconds"
# load(file="/Users/jerzy/Develop/data/SPY_10second_202306.RData")
# dataf <- "10-Seconds"
# load(file="/Users/jerzy/Develop/data/QQQ_bigticks_202305.RData")

pricev <- pricev[, 1]
pricev <- pricev["T09:30:00/T16:00:00"]
# pricev <- pricev["T09:30:00/T16:00:00"]
# pricev <- pricev["T09:30:00/T11:00:00"]
# pricev <- pricev["T10:00:00/T16:00:00"]
# pricev <- pricev["T11:00:00/T16:00:00"]

symboln <- rutils::get_name(colnames(pricev))
nrows <- NROW(pricev)


# Calculate the returns
retp <- rutils::diffit(pricev)

# Calculate the end points for the start of every day
datev <- .index(retp)
datev <- rutils::diffit(datev)
datev <- which(datev > 1000)
# Set large overnight returns to zero
# retp[abs(retp) > 10*sd(retp)] <- 0
# retp[abs(retp) > 3*sd(retp)] <- 0
retp[datev] <- 0
pricev <- cumsum(retp)

captiont <- paste("EWMA Crossover Strategy For", symboln, dataf)


## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  # create single row with four slider inputs
  fluidRow(
    # Input lambda decay parameter
    # column(width=2, sliderInput("lambda", label="lambda:", min=0.8, max=0.999, value=0.99, step=0.001)),
    # Input slow lambda decay parameter
    # column(width=2, sliderInput("lambdas", label="lambda slow:", min=0.98, max=0.999, value=0.99, step=0.001)),
    column(width=2, sliderInput("lambdas", label="lambda slow:", min=0.5, max=0.999, value=0.95, step=0.001)),
    # Input fast lambda decay parameter
    # column(width=2, sliderInput("lambdaf", label="lambda fast:", min=0.5, max=0.99, value=0.9, step=0.01)),
    column(width=2, sliderInput("lambdaf", label="lambda fast:", min=0.1, max=0.99, value=0.9, step=0.01)),
    # Input lag parameter
    column(width=1, sliderInput("lagg", label="lag", min=1, max=3, value=1, step=1)),
    # Input trending or reverting (contrarian) strategy
    column(width=1, selectInput("coeff", label="Trend (1) Revert (-1)", choices=c(1, -1), selected=(-1))),
    # Input the Bid-ask spread
    column(width=1, numericInput("bidask", label="Bid-ask:", value=0.0, step=0.01))
  ),  # end fluidRow
  
  # create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  # mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface


## Define the server code
servfun <- shiny::shinyServer(function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Recalculate the difference of EWMA prices
  priced <- shiny::reactive({

    cat("Recalculating the EWMA prices", "\n")
    ## Calculate the fast and slow EWMA prices
    # pricem <- HighFreq::run_mean(pricev, lambda=input$lambda)
    # pricesl <- HighFreq::run_mean(pricev, lambda=input$lambdas)
    # pricef <- HighFreq::run_mean(pricev, lambda=input$lambdaf)
    # sign(zoo::coredata(pricef - pricesl))

    sign(zoo::coredata(HighFreq::run_mean(retp, lambda=input$lambdas)))
    
  })  # end reactive code

  
  # Recalculate the trailing variance of prices
  # vars <- shiny::reactive({
  #   
  #   cat("Recalculating the trailing variance", "\n")
  #   # Get model parameters from input argument
  #   lambda <- input$lambda
  #   
  #   # Recalculate the trailing variance of residuals
  #   sqrt(HighFreq::run_var(pricev, lambda=lambda))
  #   
  # })  # end reactive code
  # 
  
  # Rerun the strategy
  wealthv <- shiny::reactive({
    cat("Recalculating the strategy", "\n")
    # Get model parameters from input argument
    # symboln <- input$symboln
    # symboletf <- input$symboletf
    # lambda <- input$lambda
    # lagg <- input$lagg
    # threshd <- input$threshd
    coeff <- as.numeric(input$coeff)
    
    # betas <- betas()
    # nrows <- NROW(pricev)
    
    # threshd <- input$threshd*rutils::lagit(vars())
    
    # threshd <- input$threshd*vars()
    
    # priced <- rutils::diffit(zoo::coredata(pricem))
    posv <- coeff*priced()
    posv <- rutils::lagit(posv, lagg=input$lagg)

    
    ## Calculate the positions from the excess prices
    # lambdav <- seq(from=input$lambdaf, to=input$lambdas, by=0.001)
    # Perform lapply() loop over lambdav
    # posv <- lapply(lambdav, function(lambda) {
    #   pricem <- HighFreq::run_mean(pricev, lambda=lambda)
    #   priced <- zoo::coredata(pricev - pricem)
    #   # priced <- rutils::diffit(zoo::coredata(pricem))
    #   posv <- -sign(priced)
    #   rutils::lagit(posv, lagg=1)
    # })  # end lapply
    # posv <- do.call(cbind, posv)
    # posv <- rowMeans(posv)
    
    # Calculate the number of trades
    tradez <- abs(rutils::diffit(posv))
    values$ntrades <- sum(tradez > 0)

    # Calculate the PnLs
    # reti <- (retp$Stock - rutils::lagit(betas)*retp$ETF)
    pnls <- retp*posv
    # Calculate the transaction costs
    costs <- 0.5*input$bidask*tradez
    pnls <- (pnls - costs)
    
    # pnls <- pnls*sd(retp)/sd(pnls)
    wealthv <- cbind(retp, pnls)
    colnames(wealthv) <- c(symboln, "Strategy")
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    
    wealthv
    
  })  # end reactive code
  
  
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    
    # symboln <- input$symboln
    # symboletf <- input$symboletf
    wealthv <- wealthv()
    colnamev <- colnames(wealthv)
    
    captiont <- paste(paste0(paste(colnamev[1:2], "Sharpe =", values$sharper), collapse=" / "), "/ \n",
                      "Number of trades=", values$ntrades)
    
    # endd <- rutils::calc_endpoints(wealthv, interval="days")
    endd <- rutils::calc_endpoints(wealthv, interval="minutes")
    # dygraphs::dygraph(cumsum(wealthv), main=captiont) %>%
    dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
      dyLegend(show="always", width=300)
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
