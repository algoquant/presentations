##############################
# This is a shiny app for simulating a Bollinger strategy.
# The strategy uses z-scores of prices to determine the 
# position.
# It doubles down and adds to the position when the z-score 
# increases in the same direction.
# The strategy holds its position as long as the z-score 
# doesn't change sign.
# It flips the position when the z-score changes sign.
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

# Compile the file in R by running this command:
# Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/sim_boll.cpp")

# Model and data setup


symboln <- "SPY"
# ohlc <- xts::as.xts(zoo::read.zoo(file = "/Users/jerzy/Develop/data/minutes/SPY_20260107.csv",
#                                   header = TRUE, sep = ",",
#                                   FUN = as.POSIXct,
#                                   format = "%Y-%m-%d %H:%M:%S"))
# save(ohlc, file="/Users/jerzy/Develop/data/minutes/SPY_minutes.RData")
# ohlc <- ohlc["T9:30/T16:00"]
# save(ohlc, file="/Users/jerzy/Develop/data/minutes/SPY_minutes_markets.RData")
load(file=paste0("/Users/jerzy/Develop/data/minutes/", symboln, "_minutes_markets.RData"))

pricev <- log(Cl(ohlc))
# Calculate the EMA returns and variance
retp <- rutils::diffit(pricev)

captiont <- paste("Bollinger Strategy For", symboln, "Minutes")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  # Create single row with inputs
  fluidRow(
    # Input stock symbol
    # column(width=2, selectInput("symboln", label="Symbol", choices=c(symboletf, symbolstock), selected="QQQ")),
  # ),  # end fluidRow
  # 
  # # Create single row with inputs
  # fluidRow(
    # Input lambda decay parameter
    column(width=2, sliderInput("lambdaf", label="lambda:", min=0.1, max=0.99, value=0.37, step=0.01)),
    # Input Look back interval
    # column(width=2, sliderInput("lookb", label="Look back", min=3, max=250, value=100, step=1)),
    # If trend=1 then trending, If trend=(-1) then contrarian
    column(width=2, selectInput("trendind", label="Trend coefficient", choices=c(1, -1), selected=(-1))),
  
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")

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

    # if (symboln %in% symboletf) {
    #   cat("Loading ETF prices \n")
    #   # Get ETF returns from rutils::etfenv
    #   pricev <- log(na.omit(get(symboln, rutils::etfenv$prices)))
    # } else if (symboln %in% symbolstock) {
    #   cat("Loading stock prices \n")
    #   # Get stock returns from pricestock
    #   pricev <- log(na.omit(get(symboln, pricestock)))
    # }  # end if
    
    # volr <- HighFreq::run_var(retp, lambdaf=lambdaf)
    # retema <- volr[, 1]
    # volr <- sqrt(volr[, 2])
    # zscores <- trunc((retp - retema)/volr)
    # Calculate the EMA prices and variance
    volp <- HighFreq::run_var(pricev, lambdaf=lambdaf)
    pricema <- volp[, 1]
    volp <- sqrt(volp[, 2])
    # Calculate the z-scores
    zscores <- trunc((pricev - pricema)/volp)
    # zscores <- cbind(retp, zscores)
    # colnames(zscores) <- c(symboln, "zscores")
    return(zscores)
    
  })  # end z-scores
  

  # Calculate pnls
  pnls <- shiny::reactive({
    cat("Calculating pnls\n")
    # symboln <- input$symboln
    trendind <- as.numeric(input$trendind)

    # retp <- zscores()[, 1, drop=FALSE]
    zscores <- zscores()
    nrows <- NROW(zscores)
    # Simulate the patient Bollinger Strategy
    posv <- sim_boll(zscores) ##  Stock positions
    # Calculate the number of trades and the PnLs
    values$ntrades <- sum(abs(rutils::diffit(posv)) > 0)
    pnls <- posv*retp
    # Scale the PnL volatility to that of the index
    # pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
    pnls <- cbind(retp, pnls, 0.5*(retp+pnls))
    colnames(pnls) <- c(symboln, "Strategy", "Combined")
    return(pnls)

  })  # end Calculate pnls
  

  # Plot dygraph
  dyplot <- shiny::reactive({
    cat("Plotting pnls\n")
    
    symboln <- input$symboln
    pnls <- pnls()
    colv <- colnames(pnls)

    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 3)
    ntrades <- values$ntrades

    captiont <- paste("Bollinger Strategy Sharpe", "/ \n", 
                      paste0(paste(colv, " =", sharper), collapse=" / "), "/ \n",
                      "Number trades =", ntrades)
    endw <- rutils::calc_endpoints(pnls, interval="days")
    dygraphs::dygraph(cumsum(pnls)[endw], main=captiont) %>%
      dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
      dyLegend(show="always", width=300)
    
  })  # end reactive

  # Render the dyplot object
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph(dyplot())
    
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
