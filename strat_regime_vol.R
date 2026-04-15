##############################
# This is a shiny app for simulating a volatility regime 
# switching strategy.
# 
# The strategy combines regime switching with volatility 
# targeting.
# The strategy uses the level of the range volatility 
# compared to the threshold volatility level, to determine 
# high and low volatility regimes.
# When the volatility is low and below the threshold, the 
# strategy maintains a long position in the stock, equal 
# to the target volatility divided by the range volatility.  
# So the long position is larger when the volatility is 
# lower, and smaller when the volatility is higher.
# Using the target volatility improves the performance, 
# because it adds leverage when the volatility is low.
# As the volatility rises above the threshold, it gradually 
# reduces the stock position and increases the position in 
# a mean-reverting autoregressive strategy.
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
# envv <- rutils::etfenv
# symbolv <- get("symbolv", envir=envv)
# symboln <- "SPY"

# Uncomment the below to simulate the strategy for S&P500 stocks
# Load the SP500 OHLC prices
if (!exists("sp500env")) {
  cat("Loading the S&P500 OHLC prices.\n")
  load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
} # end if
envv <- sp500env
symbolv <- sort(names(envv))
symboln <- "AAPL"

volt <- 0.01 ##  Volatility target for scaling the strategy PnLs

captiont <- paste("Volatility Regime Switching Strategy")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    ##  Input stock symboln
    column(width=1, selectInput("symboln", label="Symbol", choices=symbolv, selected=symboln)),
    ##  Input lambda returns decay parameter
    column(width=2, sliderInput("lambdaf", label="Returns decay", min=0.1, max=0.9, value=0.3, step=0.1)),
    ##  Input lambda variance decay parameter
    column(width=2, sliderInput("lambdavol", label="Vol decay", min=0.1, max=0.9, value=0.7, step=0.1)),
    ##  Input volatility scale parameter
    column(width=2, sliderInput("volscale", label="Scale", min=0.01, max=0.1, value=0.01, step=0.01)),
    ##  Input volatility threshold parameter
    column(width=2, sliderInput("volthresh", label="Vol threshold", min=0.001, max=0.05, value=0.03, step=0.001)),
    ##  Input volatility target parameter
    column(width=2, sliderInput("volt", label="Vol target", min=0.01, max=0.05, value=0.01, step=0.01)),
  ),  ##  end fluidRow

  ##  Render the plot in a new row
  fluidRow(
    dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
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

    ohlc <- log(get(symboln, envv))
    return(ohlc)
    
  })  ##  end Load the data
  
  ##  Calculate the returns
  rets <- shiny::reactive({
    
    # symboln <- input$symboln
    cat("Recalculating returns\n")
    
    ohlc <- ohlc()
    openp <- quantmod::Op(ohlc)
    closep <- quantmod::Cl(ohlc)
    retp <- rutils::diffit(closep) ##  daily returns
    colnames(retp) <- "daily"
    retd <- (closep - openp) ##  daytime returns
    colnames(retd) <- "daytime"
    reton <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE)) ##  overnight returns
    colnames(reton) <- "overnight"
    # highp <- quantmod::Hi(ohlc)
    # lowp <- quantmod::Lo(ohlc)
    # hilo <- (highp - lowp) ##  range of daily prices
    
    return(cbind(retp, retd, reton))
    
  })  ##  end Calculate the returns
  
  ##  Calculate the EMA returns
  retm <- shiny::reactive({
    
    # symboln <- input$symboln
    cat("Recalculating EMA returns\n")
    lambdaf <- input$lambdaf
    
    rets <- rets()
    retp <- rets$daily
    retm <- HighFreq::run_mean(retp, lambda=lambdaf)

    return(retm)
    
  })  ##  end Calculate the EMA returns
  
  ##  Calculate the range variance
  varv <- shiny::reactive({
    
    ##  Get model parameters from input argument
    # symboln <- input$symboln
    cat("Recalculating variance\n")
    lambdavol <- input$lambdavol
    
    ##  Calculate the range variance
    ohlc <- ohlc()
    varv <- HighFreq::run_var_ohlc(ohlc, lambda=lambdavol)
    # varv[1:2] <- 1
    return(varv)
    
  })  ##  end Calculate the range variance
  
  ##  Recalculate the strategy
  pnls <- shiny::reactive({
    
    symboln <- input$symboln
    cat("Recalculating PnLs for", symboln, "\n")
    volscale <- input$volscale
    volthresh <- input$volthresh
    volt <- input$volt
    
    ##  Calculate the strategy PnLs
    varv <- varv()
    varv <- rutils::lagit(varv, lagg=1, pad_zeros=FALSE)
    volv <- sqrt(varv)
    ##  Scale the returns by the range variance
    rets <- rets()
    # reton <- rets$overnight
    retp <- rets$daily
    retm <- retm()
    posv <- -sign(retm)
    posv <- rutils::lagit(posv, lagg=1)
    pnls <- posv*retp
    
    # Calculate the probability of being in a trending regime 
    # or a mean-reverting regime based on the volatility
    probv <- (1 + tanh((volv-volthresh)/volscale))/2
    # Apply the probabilities to the strategy PnLs
    pnls <- pnls*probv + retp*volt/volv*(1-probv)


    ##  Bind together strategy pnls
    pnls <- cbind(retp, pnls)
    # pnls <- cbind(cumsum(retp), probv)
    colnames(pnls) <- c(symboln, "Strategy")
    
    ##  Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    names(sharper) <- colnames(pnls)
    values$sharper <- round(sharper, 3)
    
    pnls <- cumsum(pnls)
    return(pnls)

  })  ##  end Recalculate the strategy
  

  ##  Plot the cumulative scaled returns
  ##  Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    ##  Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    ##  Get Sharpe ratios
    sharper <- values$sharper

    ##  Create the caption with Sharpe ratios
    captiont <- paste0(paste0(names(sharper), " SR=", sharper), collapse=" / ")
    
    ##  Plot dygraph of the cumulative PnLs
    dyplot <- dygraphs::dygraph(pnls, main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
      dyLegend(show="always", width=300)
    
    ##  Plot dygraph with two y-axes
    # dyplot <- dygraphs::dygraph(pnls, main=captiont) %>%
    #   dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
    #   dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
    #   dySeries(axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
    #   dySeries(axis="y2", label=colnamev[2], strokeWidth=1, col="red") %>%
    #   dyLegend(show="always", width=300)
      
    ##  Return the dygraph object
    return(dyplot)


  })  ##  end output plot

}  ##  end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
