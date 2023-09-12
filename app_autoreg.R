##############################
# This is a shiny app for backtesting four different autoregressive 
# models.  The code for the different models must be uncommented in 
# several places. 
# - The EWMA model calculates the positions equal to minus the 
#   trailing EWMA of the unscaled returns. 
# - The fixed MA model is similar but it calculates the MA of the 
#   unscaled returns over a look-back interval. 
# - The in-sample AR model calculates the positions equal to the AR forecasts, 
#   with the AR coefficients calibrated in-sample.
# - The running AR model calculates the positions equal to the AR 
#   forecasts, with the AR coefficients calibrated recursively 
#   out-of-sample using HighFreq::run_reg().
# 
# Just press the "Run App" button on the upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Load daily stock prices

# Load daily OHLC bars

ohlc <- rutils::etfenv$VTI
openp <- quantmod::Op(ohlc)
# highp <- quantmod::Hi(ohlc)
# lowp <- quantmod::Lo(ohlc)
pricev <- quantmod::Cl(ohlc)
retd <- (pricev - openp)
pricev <- cumsum(retd)
# colnames(retd) <- "intraday"
# reton <- (openp - rutils::lagit(pricev, lagg=1, pad_zeros=FALSE))
# colnames(reton) <- "overnight"
volumv <- quantmod::Vo(ohlc)
dataf <- "Intraday"
# dataf <- "Days"

# Load daily ETF prices
# symbolns <- sort(colnames(rutils::etfenv$prices))
# symboln <- "XLK"
# pricev <- log(na.omit(get(symboln, rutils::etfenv$prices)))
# dataf <- "Days"

# Load daily stock prices
# symbolns <- sort(colnames(prices))
# symboln <- "AAPL"
# pricev <- log(na.omit(rutils::etfenv$prices$VTI))


## Load intraday stock prices

# Load SPY OHLC minute bars
# pricev <- log(quantmod::Cl(HighFreq::SPY))

# load(file="/Users/jerzy/Develop/data/SPY_second_202306.RData")
# load(file="/Users/jerzy/Develop/data/VXX_second_202306.RData")
# load(file="/Users/jerzy/Develop/data/SVXY_second_202306.RData")
# dataf <- "Seconds"
# load(file="/Users/jerzy/Develop/data/SPY_5second_202306.RData")
# dataf <- "5-Seconds"
# load(file="/Users/jerzy/Develop/data/SPY_10second_2023.RData")
# dataf <- "10-Seconds"
# load(file="/Users/jerzy/Develop/data/SPY_minute_2023.RData")
# dataf <- "Minutes"
# load(file="/Users/jerzy/Develop/data/QQQ_bigticks_202305.RData")
# load(file="/Users/jerzy/Develop/data/SPY_minute.RData")
# load(file="/Users/jerzy/Develop/data/AAPL_minute.RData")
# load(file="/Users/jerzy/Develop/data/MSFT_minute.RData")
# load(file="/Users/jerzy/Develop/data/AAPL_minute_market.RData")
# load(file="/Users/jerzy/Develop/data/SVXY_minute_market.RData")
# load(file="/Users/jerzy/Develop/data/VXX_minute.RData")
# symboln <- "VXX"
# load(file="/Users/jerzy/Develop/data/SVXY_minute.RData")

# volumv <- pricev[, 2]
pricev <- pricev[, 1]

# Calculate the returns
symboln <- rutils::get_name(colnames(pricev))
retp <- rutils::diffit(pricev)

# Select returns during market hours
# retp <- retp["T09:30:00/T16:00:00"]
# retp <- retp["T09:30:00/T11:00:00"]
# retp <- retp["T10:30:00/T16:00:00"]

## Uncomment for intraday returns
# Set overnight returns to zero
# datev <- .index(retp)
# datev <- rutils::diffit(datev)
# datev <- which(datev > 1000)
# retp[datev] <- 0
nrows <- NROW(retp)
# pricev <- cumsum(retp)

# Define the response and predictor matrices
orderm <- 5
# predm <- lapply(1:orderm, rutils::lagit, input=retp)
# predm <- rutils::do_call(cbind, predm)
# predm <- cbind(rep(1, nrows), predm)

# captiont <- paste("Autoregressive Strategy For", symboln, dataf)
# captiont <- paste("Autoregressive Strategy Using the Lagged Fitted Values For", symboln, dataf)
captiont <- paste("Strategy Using Autoregressive Forecasts For", symboln, dataf)

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  # Create single row with four slider inputs
  fluidRow(
    # Input look-back parameter
    # column(width=2, sliderInput("look_back", label="Look-back", min=2, max=10, value=2, step=1)),
    # Input AR order parameter
    # column(width=2, sliderInput("orderp", label="orderp", min=1, max=orderm, value=3, step=1)),
    # Input lambda decay parameter
    column(width=2, sliderInput("lambda", label="lambda:", min=0.1, max=0.99, value=0.9, step=0.01)),
    # Input lag parameter
    column(width=2, sliderInput("lagg", label="lag", min=0, max=5, value=1, step=1)),
    # Input the bid-ask spread
    column(width=2, numericInput("bidask", label="Bid-ask:", value=0.01, step=0.01)),
    # Input trending or reverting (contrarian) strategy
    column(width=1, selectInput("coeff", label="Trend (1) Revert (-1)", choices=c(1, -1), selected=(-1)))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  # mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface


## Define the server code
servfun <- shiny::shinyServer(function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Recalculate the AR forecasts
  fcasts <- shiny::reactive({

    cat("Recalculating the AR forecasts", "\n")

    lambda <- input$lambda
    orderp <- input$orderp

    ## Uncomment for daily returns
    # Calculate the returns scaled by the trailing average volume
    # volumr <- HighFreq::run_mean(volumv, lambda=lambda)
    # retsc <- ifelse(volumv > 0, volumr*retp/volumv, 0)
    ## Uncomment for intraday returns
    retsc <- retp
    
    ## Uncomment for the EWMA model
    # Calculate the trailing mean returns
    rutils::lagit(sign(HighFreq::run_mean(retsc, lambda=lambda)), lagg=input$lagg)
    # retp
    
    ## Uncomment for the in-sample and running AR models
    # Calculate the predictor matrix
    # predm <- lapply(1:orderp, rutils::lagit, input=retsc)
    # predm <- rutils::do_call(cbind, predm)
    # Make the first rows of the predictor matrix random - to avoid collinearity
    # predm[1:orderm, ] <- matrix(rnorm(orderm*orderp, sd=0.01), nc=orderp)
    # predm <- cbind(rep(1, nrows), predm)
    # colnames(predm) <- c("phi0", paste0("lag", 1:orderp))
    
    # Uncomment for the in-sample AR model
    # Calculate the in-sample AR forecasts
    # predinv <- MASS::ginv(predm)
    # coeff <- predinv %*% retsc
    # rutils::lagit(predm %*% coeff, lagg=input$lagg)
    
    ## Uncomment for the running AR model
    # Calculate the trailing forecasts
    # controlv <- HighFreq::param_reg()
    # regs <- HighFreq::run_reg(respv=retsc, predm=predm, lambda=lambda, controlv=controlv)
    # Forecasts equal to the AR forecasts
    # fcasts <- regs[, NCOL(regs)]
    # Forecasts equal to the AR residuals
    # fcasts <- rutils::lagit(regs[, NCOL(regs)-1], lagg=input$lagg)
    # fcasts[1:10] <- 0
    # fcasts

  })  # end reactive code

  
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
    
    posv <- coeff*fcasts()

    # Calculate the number of trades
    tradez <- abs(rutils::diffit(posv))
    values$ntrades <- sum(tradez > 0)

    # Calculate the PnLs
    pnls <- retp*posv
    # Calculate the transaction costs
    costs <- 0.5*input$bidask*tradez
    pnls <- (pnls - costs)
    
    pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
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
    dygraphs::dygraph(cumsum(wealthv), main=captiont) %>%
    # dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
      dyLegend(show="always", width=300)
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
