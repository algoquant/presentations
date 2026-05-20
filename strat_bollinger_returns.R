##############################
# This is a shiny app for simulating a Bollinger strategy
# for daily returns of ETFs and stocks.
# The strategy uses the z-scores of the returns to determine 
# oversold and overbought conditions.
# It goes long $1 of the stock when the z-score is below 
# the lower threshold, and goes short $1 when the z-score 
# is above the upper threshold.  
# The lower and upper thresholds have opposite signs, but
# the same absolute value, and are set by the user.
# So the strategy is contrarian and mean-reversion.
# The strategy is patient because it holds its position 
# until the z-score crosses the opposite threshold.
# The strategy is path dependent and requires performing 
# a loop over time.
# 
# Just press the "Run App" button on upper right of this panel.
##############################


## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Model and data setup

# symbolv <- names(datenv)
symboletf <- rutils::etfenv$symbolv
symboletf <- symboletf[!(symboletf %in% c("MTUM", "QUAL", "VLUE", "USMV", "AIEQ"))]
# retp <- rutils::etfenv$returns[, symboletf]

load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
symbolstock <- sort(colnames(retstock))


captiont <- paste("Bollinger Strategy")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  ## Create single row with inputs
  fluidRow(
    # Input stock symbol
    column(width=1, selectInput("symboln", label="Symbol", choices=c(symboletf, symbolstock), selected="QQQ")),
    # Input lambda decay parameter
    column(width=2, sliderInput("lambdaf", label="lambda:", min=0.1, max=0.99, value=0.4, step=0.01)),
    # threshv
    # Input threshold parameter
    column(width=2, sliderInput("threshv", label="Threshold:", min=0.1, max=2.0, value=0.7, step=0.1)),
    # If trend=1 then trending, If trend=(-1) then contrarian
    # column(width=1, selectInput("trendind", label="Trend coefficient", choices=c(1, -1), selected=(-1))),
  
  ),  ## end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")

)  ## end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Calculate the returns
  retp <- shiny::reactive({
    # Get model parameters from input argument
    symboln <- input$symboln

    if (symboln %in% symboletf) {
      cat("Loading ETF prices \n")
      # Get ETF returns from rutils::etfenv
      retp <- na.omit(get(symboln, rutils::etfenv$returns))
    } else if (symboln %in% symbolstock) {
      cat("Loading stock prices \n")
      # Get stock returns from retstock
      retp <- na.omit(get(symboln, retstock))
      retp <- retp[retp != 0]  # Remove zero returns
    }  # end if
    
    colnames(retp) <- symboln
    return(retp)
    
  })  # end returns
  
  
  # Calculate the z-scores
  zscores <- shiny::reactive({
    # Get model parameters from input argument
    symboln <- input$symboln
    lambdaf <- input$lambdaf

    retp <- retp()
    
    # Calculate the EMA returns and variance
    volp <- HighFreq::run_var(retp, lambdaf=lambdaf)
    retm <- volp[, 1]
    volp <- sqrt(volp[, 2])
    # Calculate the z-scores
    zscores <- retm/volp
    zscores[1:11] <- 0
    return(zscores)
    
  })  ## end z-scores
  

  ## Calculate pnls
  pnls <- shiny::reactive({
    cat("Calculating pnls\n")
    symboln <- input$symboln
    # trendind <- as.numeric(input$trendind)
    threshv <- input$threshv

    retp <- retp()
    zscores <- zscores()
    # Simulate the patient Bollinger Strategy
    posv <- rep(NA_integer_, NROW(retp)) ##  Stock positions
    posv[1] <- 0
    posv <- ifelse(zscores > threshv, -1, posv)
    posv <- ifelse(zscores < -threshv, 1, posv)
    posv <- zoo::na.locf(posv)
    # Lag the positions to trade in the next period
    posv <- rutils::lagit(posv, lagg=1)
    # Calculate the number of trades and the PnLs
    values$ntrades <- sum(abs(rutils::diffit(posv)) > 0)
    pnls <- posv*retp
    # Scale the PnL volatility to that of the index
    pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
    pnls <- cbind(retp, pnls, 0.5*(retp+pnls))
    colnames(pnls) <- c(symboln, "Bollinger", "Combined")
    return(pnls)

  })  ## end Calculate pnls
  

  ## Plot dygraph
  dyplot <- shiny::reactive({
    cat("Plotting pnls\n")
    
    symboln <- input$symboln
    pnls <- pnls()
    colv <- colnames(pnls)

    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 3)
    ntrades <- values$ntrades

    captiont <- paste(paste0(paste(colv, " =", sharper), collapse=" / "), "/ \n",
                      "Number trades =", ntrades)
    endw <- rutils::calc_endpoints(pnls, interval="weeks")
    dygraphs::dygraph(cumsum(pnls)[endw], main=captiont) %>%
      dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
      dyLegend(show="always", width=300)
    
  })  ## end reactive

  ## Render the dyplot object
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph(dyplot())
    
}  ## end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
