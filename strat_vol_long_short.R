##############################
# This is a shiny app for simulating a Long-Short volatility strategy 
# for stocks.
# The strategy buys the low volatility stock portfolio and shorts the 
# high volatility portfolio.
# The low volatility portfolio consists of stocks with trailing 
# volatilities less than the median, and the high portfolio with 
# trailing volatilities greater than the median.
# It uses the function HighFreq::run_var() to calculate the trailing 
# volatilities. 
# The low volatility portfolio has higher risk-adjusted returns than 
# the high volatility portfolio, which contradicts the CAPM model.
# The Long-Short Volatility strategy has higher risk-adjusted returns 
# than the price-weighted portfolio, but it has much lower absolute 
# returns.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(rutils)
library(shiny)
library(dygraphs)

## Model and data setup

captiont <- paste("Long-Short Volatility Strategy For Stocks")

# Load daily S&P500 log percentage stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Subset (select) the stock returns after the start date of VTI
retvti <- na.omit(rutils::etfenv$returns$VTI)
colnames(retvti) <- "VTI"
retp <- retv[zoo::index(retvti)]
datev <- zoo::index(retp)
retvti <- retvti[datev]
nrows <- NROW(retp)
nstocks <- NCOL(retp)
# Replace NA returns with zeros
retsna <- retp
retsna[is.na(retsna)] <- 0

# Wealth of price-weighted (fixed shares) portfolio
pricev <- cumsum(retsna)
wealthpw <- rowMeans(exp(pricev))

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input lambda decay parameter
    column(width=3, sliderInput("lambda", label="lambda:", min=0.01, max=0.99, value=0.65, step=0.01))
  ),  # end fluidRow

  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="650px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Recalculate the strategy
  wealthv <- shiny::reactive({
    
    # Get model parameters from input argument
    lambda <- input$lambda
    cat("Recalculating strategy", "\n")

    # Calculate the volatilities
    volat <- HighFreq::run_var(retsna, lambda=lambda)
    volat <- sqrt(volat)
    # Calculate the median volatilities
    medianv <- matrixStats::rowMedians(volat)
    # Calculate wealth of low volatility stocks
    alloc <- matrix(integer(nrows*nstocks), ncol=nstocks)
    alloc[volat <= medianv] <- 1
    alloc <- rutils::lagit(alloc)
    retlow <- rowSums(alloc*retp, na.rm=TRUE)
    wealth_lovol <- exp(cumsum(retlow))
    # Calculate wealth of high volatility stocks
    alloc <- matrix(integer(nrows*nstocks), ncol=nstocks)
    alloc[volat > medianv] <- 1
    alloc <- rutils::lagit(alloc)
    rethigh <- rowSums(alloc*retp, na.rm=TRUE)
    wealth_hivol <- exp(cumsum(rethigh))
    
    # Calculate the volatilities of the low and high volatility stocks
    volat <- HighFreq::run_var(cbind(retlow, rethigh), lambda=lambda)
    volat <- sqrt(volat)
    volat[1:2, ] <- 1
    colnames(volat) <- c("Low Volatility", "High Volatility")
    # Multiply the high volatility portfolio returns by a factor
    factv <- volat[, 1]/volat[, 2]
    factv <- rutils::lagit(factv)
    # Calculate the long-short volatility returns
    retls <- (retlow - factv*rethigh)
    wealthls <- exp(cumsum(retls))
    
    # Combined wealth
    wealthv <- cbind(wealthpw, wealthls)
    wealthv <- xts::xts(wealthv, datev)
    colnamev <- c("Price-weighted", "Long-Short Vol")
    colnames(wealthv) <- colnamev
    log(wealthv)

  })  # end Recalculate the strategy
  

  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    cat("Plotting for ", input$symbol, "\n")
    wealthv <- wealthv()
    colnamev <- colnames(wealthv)

    # Calculate the Sharpe and Sortino ratios
    sharper <- sqrt(252)*sapply(rutils::diffit(wealthv),
                     function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))[1, ]
    sharper <- round(sharper, 3)

    captiont <- paste("Wealth of Price-weighted and Long-Short Vol Portfolios / \n",
                      paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "))
    
    # Plot the log wealth
    endd <- rutils::calc_endpoints(wealthv, interval="months")
    dygraphs::dygraph(wealthv[endd], main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="blue") %>%
      dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="red") %>%
      dyLegend(show="always", width=500)
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
