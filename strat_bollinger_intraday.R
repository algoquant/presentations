##############################
# This is a shiny app for simulating an intraday Bollinger 
# strategy for a set of stocks.
# The strategy uses z-scores of prices to determine the 
# position.
# 
# It sells the stock short if the z-score is above a threshold 
# and buys the stock long if the z-score is below a negative 
# threshold.
# It exits the position when the z-score changes sign.
# The strategy is path dependent and requires performing 
# a loop over time.
# 
# The z-scores are calculated by the C++ function run_zscorex()
# using an exponential moving maximum EMM volatility and an 
# exponential moving average EMA of the prices.
# 
# Compile the function run_zscorex() by running this command:
# Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/run_zscores.cpp")
# 
# Just press the "Run App" button on upper right of this panel.
##############################


## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Model and data setup

# Load the intraday stock prices
if (!exists("run_zscorex")) {
  cat("Compiling run_zscorex()\n")
  Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/run_zscores.cpp")
} # end if

# Load the intraday stock prices
if (!exists("pricestock")) {
  cat("Loading the stock prices\n")
  load("/Users/jerzy/Develop/data/minutes/chained/stock_prices.RData")
  symbolv <- colnames(pricestock)
  retstock <- rutils::diffit(pricestock)
} # end if
symbolv <- colnames(pricestock)

captiont <- paste("Intraday Bollinger Strategy")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  # Create single row with inputs
  fluidRow(
    # Input stock symbol
    # column(width=1, selectInput("symboln", label="Symbol", choices=c(symboletf, symbolv), selected="QQQ")),
    # Input lambda decay parameter
    column(width=2, sliderInput("lambdaf", label="lambda:", min=0.1, max=0.99, value=0.82, step=0.01)),
    # Input threshold level
    column(width=2, sliderInput("threshv", label="Threshold:", min=0.1, max=3.9, value=0.3, step=0.1)),
    # If trend=1 then trending, If trend=(-1) then contrarian
    column(width=1, selectInput("trendind", label="Trend coefficient", choices=c(1, -1), selected=(-1))),
    # Input the Bid-ask spread
    column(width=1, numericInput("bidask", label="Bid-ask:", value=0.04, step=0.01)),
    
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="650px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Calculate the z-scores
  zscores <- shiny::reactive({
    # Get model parameters from input argument
    # symboln <- input$symboln
    lambdaf <- input$lambdaf

    zscores <- lapply(pricestock, function(pricex) {
      # Calculate the z-scores
      # zscores <- HighFreq::run_zscores(pricex, lambdaf=lambdaf)
      zscores <- run_zscorex(pricex, lambdaf=lambdaf)
      zscores <- zscores[, 3]
      zscores[1:11] <- 0
      return(zscores)
    }) # end lapply)
    zscores <- do.call(cbind, zscores)
    colnames(zscores) <- symbolv
    return(zscores)
    
  })  # end z-scores


  # Calculate pnls
  pnls <- shiny::reactive({
    cat("Calculating pnls\n")
    # symboln <- input$symboln
    threshv <- input$threshv
    trendind <- as.numeric(input$trendind)

    zscores <- zscores()
    nrows <- NROW(zscores)
    ncols <- NCOL(zscores)
    # Calculate the positions
    posv <- matrix(NA_integer_, nrow=nrows, ncol=ncols)
    posv[1, ] <- 0 ##  Initial position
    posv <- ifelse(zscores > threshv, trendind, posv)
    posv <- ifelse(zscores < -threshv, -trendind, posv)
    posv <- zoo::na.locf(posv)
    posv <- rutils::lagit(posv, lagg=1)
    # Simulate the patient Bollinger Strategy
    # posv <- integer(nrows) ##  Stock positions
    # posv[1] <- 0 ##  Initial position
    # for (it in 2:nrows) {
    #   if ((abs(zscores[it-1]) > abs(posv[it-1])) || ((zscores[it-1]*posv[it-1]) > 0)) {
    #     # Increase position size
    #     posv[it] <- trendind*zscores[it-1]
    #   } else {
    #     ##  Do nothing
    #     posv[it] <- posv[it-1]
    #   }  ##  end if
    # }  ##  end for
    
    # Calculate the number of trades
    flipi <- abs(rutils::diffit(posv))
    values$ntrades <- sum(flipi > 0)
    # Calculate the PnLs
    pnls <- posv*retstock
    # Calculate the transaction costs
    costs <- 0.5*input$bidask*flipi
    pnls <- (pnls - costs)
    # colnames(pnls) <- symbolv
    # Scale the PnL volatility to that of the index
    # pnls <- pnls*sd(retstock[retstock<0])/sd(pnls[pnls<0])
    pnls <- xts::xts(rowSums(pnls), order.by=index(retstock))
    retstockm <- rowSums(retstock)
    pnls <- cbind(retstockm, pnls, 0.5*(retstockm+pnls))
    colnames(pnls) <- c("Stocks", "Bollinger", "Combined")
    return(pnls)

  })  # end Calculate pnls
  

  # Plot dygraph
  output$dyplot <- dygraphs::renderDygraph({
    cat("Plotting pnls\n")
    
    pnls <- pnls()
    colv <- colnames(pnls)

    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 3)
    ntrades <- values$ntrades

    captiont <- paste("Sharpe", paste0(paste(colv, " =", sharper), collapse=" / "), "/ \n",
                      "Number trades =", ntrades)
    # endw <- rutils::calc_endpoints(pnls, interval="weeks")
    dygraphs::dygraph(cumsum(pnls), main=captiont) %>%
      dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
      dyLegend(show="always", width=300)
    
  })  # end reactive


}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
