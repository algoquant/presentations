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

# Load the S&P500 stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Subset (select) the prices after the start date of VTI
retvti <- na.omit(rutils::etfenv$returns$VTI)
colnames(retvti) <- "VTI"
pricev <- prices[zoo::index(retvti)]
# Select columns with non-NA prices at start
pricev <- prices[, !is.na(prices[1, ])]
# Copy over NA prices using the function zoo::na.locf()
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
datev <- zoo::index(pricev)
retvti <- retvti[datev]
nrows <- NROW(pricev)
nstocks <- NCOL(pricev)

# Calculate log stock returns
retp <- rutils::diffit(log(pricev))
# Wealth of price-weighted (fixed shares) portfolio
pricen <- exp(cumsum(retp))
wealth_pw <- rowMeans(pricen)

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input lambda decay parameter
    column(width=3, sliderInput("lambda", label="lambda:", min=0.01, max=0.99, value=0.87, step=0.01)),
    # Input the volatility level
    column(width=2, sliderInput("volevel", label="volevel", min=0.001, max=0.1, value=0.016, step=0.001))
  ),  # end fluidRow

  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="550px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Recalculate the strategy
  wealthv <- shiny::reactive({
    
    # Get model parameters from input argument
    lambda <- input$lambda
    volevel <- input$volevel
    cat("Recalculating strategy", "\n")

    # Calculate the volatilities
    volat <- HighFreq::run_var(retp, lambda=lambda)
    volat <- sqrt(volat)
    # medianv <- matrixStats::rowMedians(volat)
    
    # Calculate wealth of low volatility stocks
    alloc <- matrix(integer(nrows*nstocks), ncol=nstocks)
    alloc[volat <= volevel] <- 1
    alloc <- rutils::lagit(alloc)
    retlow <- rowSums(alloc*retp)
    # Calculate wealth of high volatility stocks
    alloc <- matrix(integer(nrows*nstocks), ncol=nstocks)
    alloc[volat > volevel] <- 1
    alloc <- rutils::lagit(alloc)
    rethigh <- rowSums(alloc*retp)

    # Calculate the volatilities of the low and high volatility stocks
    retvol <- cbind(retlow, rethigh)
    retvol <- xts::xts(retvol, datev)
    volat <- HighFreq::run_var(retvol, lambda=lambda)
    volat <- sqrt(volat)
    volat[1:2, ] <- 1
    colnames(volat) <- c("Low Volatility", "High Volatility")
    # Multiply the high volatility portfolio returns by a factor
    factv <- volat[, 1]/volat[, 2]
    factv <- rutils::lagit(factv)
    # Calculate the long-short volatility returns
    retls <- (retlow - factv*rethigh)
    wealth_ls <- exp(cumsum(retls))
    
    # Combined wealth
    wealthv <- cbind(wealth_pw, wealth_ls)
    wealthv <- xts::xts(wealthv, datev)
    colnamev <- c("Price-weighted", "Long-Short Vol")
    colnames(wealthv) <- colnamev
    log(wealthv)

  })  # end Recalculate the strategy
  

  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    cat("Plotting", "\n")
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
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
      dyLegend(show="always", width=500)
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
