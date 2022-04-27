##############################
# This is a shiny app for simulating a rolling portfolio 
# optimization strategy with filtering of returns.
# It uses HighFreq::back_test()
# 
# Just press the "Run App" button on upper right of this panel.
##############################

# Best parameters - these are stale and don't work
# typev interval look_back max_eigen alpha
# max_sharpe  days 15-35  6-7
# min_var  days 15-35  6-7
# max_sharpe  weeks 3-6  5-9
# min_var  weeks 3-5  6-9
# max_sharpe  weeks 16  6-9
# min_var  weeks 16  6-9
# Long weekly look_backs work very well because of IEF long position !
# max_sharpe  weeks 100  8-9
# min_var  weeks 100  8-9
# max_sharpe  months 7  6
# min_var  months 2-3  9
# Long monthly look_backs work very well because of IEF long position !
# max_sharpe  months 21-100  9
# min_var  months 21-100  9



## Setup code that runs once when the shiny app is started

# load packages
library(HighFreq)
library(shiny)
library(dygraphs)
# Model and data setup

# Source the model function
# source("C:/Develop/lecture_slides/scripts/roll_portf.R")


# Load ETF data
# symbolv <- rutils::etfenv$symbolv
# ETFs with smallest Hurst
# symbolv <- c("XLP", "XLU", "VNQ", "XLV", "XLF", "XLB", "XLE", "XLY", "XLI", "XLK")
# ETFs with largest Hurst
# symbolv <- c("DBC", "IEF", "VTI", "XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY")
# symbolv <- c("VYM", "VEU", "DBC", "IEF", "VTI", "IWF", "IWD", "IWB")
# symbolv <- c("XLU", "XLE", "XLK", "IWD", "VYM", "IWF", "XLI", "IEF", "VNQ", "DBC")
# symbolv <- c("VYM", "VEU", "DBC", "IEF", "VTI", "IWF", "IWD", "IWB", "XLU", "XLE", "XLK", "XLI", "VNQ")
# symbolv <- c("IVW", "VTI", "IWF", "IWD", "IWB", "VYM", "DBC", "IEF", "VEU", "SVXY", "VXX")
# symbolv <- c("IVW", "VTI", "IWF", "IWD", "IWB", "VYM", "DBC", "IEF", "VEU")
# symbolv <- c("VEU", "GLD", "EEM", "DBC", "VYM", "USO", "IWB", "IWD", "VTI")
# symbolv <- c("TLT", "IEF", "USO", "GLD", "DBC", "XLY", "XLI", "XLB", "XLV", "XLE", "XLU", "XLK", "XLP", "IWD")

symbolv <- c("IVW", "VTI", "IWF", "IWD", "IWB", "VYM", "DBC", "VEU", "SVXY", "VXX")

nweights <- NROW(symbolv)
returns <- rutils::etfenv$returns[, symbolv]
# Select rows with IEF data
# returns <- returns[index(rutils::etfenv$IEF)]
# 
# Or
# Calculate the first non-NA values and their positions.
first_non_na <- sapply(returns, function(xtes) {
  match(TRUE, !is.na(xtes))
})  # end sapply
# Find first row containing at least 3 non-NA values.
sort(first_non_na)[3]
# Select rows containing at least 3 non-NA values.
returns <- returns[(sort(first_non_na)[3]):NROW(returns)]
# returns <- returns[-(1:(sort(first_non_na)[7]-1))]


# Calculate the volumes
# volume_s <- lapply(symbolv, function(symbol) {
#   quantmod::Vo(get(x=symbol, envir=rutils::etfenv))
# })  # end lapply
# volume_s <- rutils::do_call(cbind, volume_s)
# colnames(volume_s) <- symbolv
# volume_s <- volume_s[index(returns)]
# volume_s[volume_s == 0] <- NA
# volume_s <- zoo::na.locf(volume_s, na.rm=FALSE)
# volume_s <- zoo::na.locf(volume_s, fromLast=TRUE)
# Calculate the row ranks


############
# S&P100
# load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# returns <- returns["2000-01-01/"]
# symbolv <- colnames(returns)
# nweights <- NROW(symbolv)


# Copy over NA values with zeros
returns[1, is.na(returns[1, ])] <- 0
returns <- zoo::na.locf(returns, na.rm=FALSE)
# sum(is.na(returns))
# excess <- matrixStats::rowRanks(returns)
# excess <- (excess - rowMeans(excess))
# Scale returns by the volumes
# excess <- returns/sqrt(volume_s)
excess <- returns

# Benchmark index
# indeks <- xts(cumsum(rowMeans(returns)), index(returns))
# indeks <- Cl(rutils::etfenv$VTI)[index(returns)]
indeks <- rutils::etfenv$returns[index(returns), "VTI"]
indeks[1] <- 0
indeks <- zoo::na.locf(indeks, na.rm=FALSE)
indeks <- cumsum(indeks)


# Portfolio with largest Hurst
# weights <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/etf_hurst_weights.csv", stringsAsFactors=FALSE)
# weights <- structure(as.numeric(weights$x), names=weights$X)
# portf_hurst <- -drop(returns %*% weights)
# portf_hurst <- sd(excess$VTI)/sd(portf_hurst)*portf_hurst


# End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel("Rolling Portfolio Optimization Strategy for ETFs"),
  
  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the re_calculate variable is updated
    column(width=12, 
           h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
           actionButton("re_calculate", "Recalculate the Model"))
  ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input end points interval
    column(width=2, selectInput("interval", label="End points Interval",
                choices=c("days", "weeks", "months", "years"), selected="weeks")),
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Lookback interval",
                                min=1, max=100, value=18, step=1)),
    column(width=2, sliderInput("lambda", label="Weight decay:",
                                min=0.01, max=0.99, value=0.01, step=0.05)),
    # Input end points interval
    column(width=2, selectInput("model_type", label="Weights type",
                                choices=c("max_sharpe", "max_sharpe_median", "min_var", "min_varpca", "rank", "rankrob", "quantilev"), selected="max_sharpe")),
    # Input number of eigenvalues for regularized matrix inverse
    column(width=2, numericInput("max_eigen", "Number of eigenvalues", value=6)),
    # Input the shrinkage intensity
    column(width=2, sliderInput("alpha", label="Shrinkage intensity",
                                min=0.01, max=0.99, value=0.01, step=0.05)),
    # Input the quantile
    column(width=2, sliderInput("probv", label="Confidence level",
                                min=0.01, max=0.49, value=0.25, step=0.01)),
    # If coeff=1 then trending, If coeff=(-1) then contrarian
    # column(width=2, numericInput("coeff", "Trend coefficient:", value=1)),
    column(width=2, selectInput("coeff", label="Trend coefficient",
                                choices=c(1, -1), selected=1))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Recalculate the data and rerun the model
  datav <- shiny::reactive({
    # get model parameters from input argument
    interval <- isolate(input$interval)
    max_eigen <- isolate(input$max_eigen)
    look_back <- isolate(input$look_back)
    lambda <- isolate(input$lambda)
    model_type <- isolate(input$model_type)
    alpha <- isolate(input$alpha)
    probv <- isolate(input$probv)
    coeff <- as.numeric(isolate(input$coeff))
    # Model is recalculated when the re_calculate variable is updated
    input$re_calculate

    # Define end points
    endp <- rutils::calc_endpoints(returns, interval=interval)
    # endp <- ifelse(endp<(nweights+1), nweights+1, endp)
    endp <- endp[endp > 2*nweights]
    nrows <- NROW(endp)
    # Define start points
    startp <- c(rep_len(1, look_back-1), endp[1:(nrows-look_back+1)])
    
    # Calculate the weights
    # weights <- exp(-lambda*(1:look_back))
    # weights <- weights/sum(weights)
    # weights <- matrix(weights, nc=1)
    # Calculate smoothed excess returns
    # excess <- HighFreq::roll_conv(excess, weights=weights)
    # excess <- HighFreq::lagit(excess, lagg=1)
    
    # needs more work: Shrink to portfolio with largest Hurst
    # excess <- (1-alpha)*excess + alpha*portf_hurst
    
    
    # Rerun the model
    pnls <- HighFreq::back_test(excess=excess, 
                                 returns=returns,
                                 startpoints=startp-1,
                                 endpoints=endp-1,
                                 probv=probv,
                                 max_eigen=max_eigen, 
                                 alpha=alpha, 
                                 model_type=model_type,
                                 coeff=coeff)
    pnls[which(is.na(pnls)), ] <- 0
    pnls <- cumsum(pnls)
    # pnls <- cumprod(1 + pnls)
    pnls <- cbind(pnls, indeks)
    colnames(pnls) <- c("Strategy", "Index")
    pnls[c(1, endp), ]
  })  # end reactive code
  
  # return to output argument a dygraph plot with two y-axes

    output$dyplot <- dygraphs::renderDygraph({
    colnamev <- colnames(datav())
    dygraphs::dygraph(datav(), main="Rolling Portfolio Optimization Strategy") %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue")
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
