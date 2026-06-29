##############################
# Shiny app for simulating an intraday range strategy.
# 
# The strategy uses the range ratio equal to the 
# close-to-low range divided by the hi-low range of 
# intraday prices to determine the position in the 
# stock.
# 
# If the close price is near the top of the range, 
# the strategy takes a short position, and if the 
# close price is near the bottom of the range, the 
# strategy takes a long position. 
# It leaves the position unchanged if the close 
# price is in the middle of the range, determined 
# by the threshold parameter.
#
# Just press the "Run App" button on upper right of this panel.
##############################


## Model and data setup
## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Uncomment the below to simulate the strategy for ETFs
# Get the vector of ETF symbols from the environment
if (!("etfenv" %in% ls())) {
  cat("Loading the ETF OHLC prices.\n")
  load("/Users/jerzy/Develop/data/etf_ohlc.RData")
} # end if
symboletf <- sort(names(etfenv))
symboln <- "QQQ"

# Uncomment the below to simulate the strategy for S&P500 stocks
# Load the SP500 OHLC prices
if (!exists("sp500env")) {
  cat("Loading the S&P500 OHLC prices.\n")
  load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
} # end if
symbolstock <- sort(names(sp500env))
# symboln <- "AAPL"

# Vector of stock symbols in the environment
symbolv <- c(symboletf, symbolstock)

rangev <- "1990/"
volt <- 0.01 ##  Volatility target for scaling the strategy PnLs
varfloor <- 1e-8 ##  Variance floor to prevent division by zero in Kelly ratio calculations

captiont <- paste("Intraday Range Strategy")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    ##  Input stock symboln
    column(width=1, selectInput("symboln", label="Symbol", choices=symbolv, selected=symboln)),
    ##  Input volatility threshold parameter
    column(width=2, sliderInput("threshv", label="Threshold", min=0.1, max=0.6, value=0.3, step=0.01)),
  ),  ##  end fluidRow

  ##  Render the plot in a new row
  fluidRow(
    dygraphs::dygraphOutput("dyplot", width="90%", height="700px")
  ),  ##  end fluidRow
  
)  ##  end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  ##  Create an empty list of reactive values.
  values <- reactiveValues()

  ##  Load the OHLC prices
  ohlc <- shiny::reactive({
    
    symboln <- input$symboln
    cat("Loading data for", symboln, "\n")

    if (symboln %in% symboletf) {
      cat("Loading ETF prices \n")
      # Get ETF returns from rutils::etfenv
      ohlc <- log(get(symboln, etfenv))
    } else if (symboln %in% symbolstock) {
      cat("Loading stock prices \n")
      # Get stock returns from pricestock
      ohlc <- log(get(symboln, sp500env))
    }  # end if
    
    return(ohlc)
    
  })  ##  end Load the data
  
  ##  Calculate the returns
  retp <- shiny::reactive({
    
    # symboln <- input$symboln
    cat("Recalculating returns\n")
    
    ohlc <- ohlc()
    closep <- quantmod::Cl(ohlc)
    retp <- rutils::diffit(closep) ##  daily returns
    return(retp)
    
  })  ##  end Calculate the returns
  
  ##  Calculate the range
  rangev <- shiny::reactive({
    
    # symboln <- input$symboln
    cat("Recalculating returns\n")
    
    ohlc <- ohlc()
    openp <- quantmod::Op(ohlc)
    closep <- quantmod::Cl(ohlc)
    highp <- quantmod::Hi(ohlc)
    lowp <- quantmod::Lo(ohlc)
    hilo <- (highp - lowp) ##  range of daily prices
    rangev <- (closep - lowp)/hilo
    
    return(rangev)
    
  })  ##  end Calculate the returns
  
  ##  Recalculate the strategy
  pnls <- shiny::reactive({
    
    symboln <- input$symboln
    cat("Recalculating PnLs for", symboln, "\n")
    # volscale <- input$volscale
    # volthresh <- input$volthresh
    # volt <- input$volt
    threshv <- input$threshv
    
    ##  Calculate the strategy PnLs
    retp <- retp()
    rangev <- rangev()
    
    posv <- rep(NA_integer_, NROW(retp)) ##  Stock positions
    posv[1] <- 0
    posv <- ifelse(rangev > (1-threshv), -1, posv)
    posv <- ifelse(rangev < threshv, 1, posv)
    posv <- zoo::na.locf(posv)
    # Lag the positions to trade in the next period
    posv <- rutils::lagit(posv, lagg=1)
    # Calculate the number of trades and the PnLs
    values$ntrades <- sum(abs(rutils::diffit(posv)) > 0)
    pnls <- posv*retp
    
    ##  Bind together strategy pnls
    pnls <- cbind(retp, pnls)
    colnames(pnls) <- c(symboln, "Strategy")

    return(pnls)

  })  ##  end Recalculate the strategy
  

  ##  Plot the cumulative scaled returns
  ##  Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    ##  Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)

    ##  Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    # names(sharper) <- colnames(pnls)
    sharper <- round(sharper, 3)
    ntrades <- values$ntrades
    
    ##  Create the caption with Sharpe ratios
    captiont <- paste("Sharpe", paste0(paste(colnamev, " =", sharper), collapse=" / "), "/ \n",
                      "Number trades =", ntrades)
    
    ##  Plot dygraph of the cumulative PnLs
    dyplot <- dygraphs::dygraph(cumsum(pnls), main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
      dyLegend(show="always", width=300)
      
    ##  Return the dygraph object
    return(dyplot)


  })  ##  end output plot

}  ##  end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
