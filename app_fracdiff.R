##############################
# This is a shiny app for plotting the fractional returns.
# The user can select the difference fraction delta.
#
# Just press the "Run App" button on upper right of this panel.
##############################


## Model and data setup
## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

captiont <- paste("Fractional Returns")

# Define fractional difference function
fracdiff <- function(x, deltaf=0.9, lookb=100) {
  weightv <- (deltaf - 0:(lookb-2)) / 1:(lookb-1)
  weightv <- (-1)^(1:(lookb-1))*cumprod(weightv)
  weightv <- c(1, weightv)
  weightv <- (weightv - mean(weightv))
  retf <- HighFreq::roll_conv(x, weightv=weightv)
  return(retf)
}  # end fracdiff


# Uncomment the following lines to load the S&P500 OHLC prices
if (!("sp500env" %in% ls())) {
  cat("Loading the S&P500 OHLC prices.\n")
  load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
} # end if
# envv <- sp500env
symbolstock <- sort(ls(sp500env))
symboln <- "NVDA"


# Get the vector of ETF symbols from the environment
if (!("etfenv" %in% ls())) {
  cat("Loading the ETF OHLC prices.\n")
  load("/Users/jerzy/Develop/data/etf_ohlc.RData")
} # end if
symboletf <- sort(names(etfenv))
symboln <- "QQQ"

# Uncomment the below to simulate the strategy for S&P500 stocks
# Load the SP500 OHLC prices
# if (!exists("sp500env")) {
#   cat("Loading the S&P500 OHLC prices.\n")
#   load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# } # end if
# symbolstock <- sort(colnames(retstock))
# symboln <- "AAPL"

symboln <- "NVDA"

# Vector of stock symbols in the environment
symbolv <- c(symboletf, symbolstock)

# volt <- 0.01 ##  Volatility target for scaling the strategy datav
# varfloor <- 1e-6 ##  Variance floor to prevent division by zero in Kelly ratio calculations


## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    ##  Input stock symboln
    column(width=1, selectInput("symboln", label="Symbol", choices=c(symboletf, symbolstock), selected=symboln)),
    ##  Input return type
    # column(width=1, selectInput("return_type", label="Returns", 
    #   choices=c("Daily"="daily", "Overnight"="overnight"), selected="daily")),
    ##  Input variance type
    # column(width=1, selectInput("variance_type", label="Variance", 
    #   choices=c("EMA"="ema", "Downside"="downside", "Range"="range"), selected="range")),
    ##  Input position scaling method
    # column(width=1, selectInput("scaling_method", label="Scaling", 
    #   choices=c("Volatility"="inverse_volatility", "Variance"="inverse_variance"), selected="inverse_volatility")),
    ##  Input lambda decay parameter
    column(width=2, sliderInput("deltaf", label="Difference fraction", min=0.001, max=0.2, value=0.1, step=0.001)),
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
  
  ##  Calculate the prices
  pricev <- shiny::reactive({
    
    # symboln <- input$symboln
    cat("Recalculating prices\n")

    ohlc <- ohlc()
    pricev <- quantmod::Cl(ohlc)
    return(pricev)
    
  })  ##  end Calculate the returns
  
  ##  Calculate the returns
  retp <- shiny::reactive({
    
    # symboln <- input$symboln
    cat("Recalculating returns\n")

    pricev <- pricev()
    retp <- rutils::diffit(pricev) ##  daily returns
    return(retp)
    
  })  ##  end Calculate the returns
  
  ##  Calculate the fractional returns
  retfrac <- shiny::reactive({
    
    # symboln <- input$symboln
    cat("Recalculating fractional returns\n")

    pricev <- pricev()
    retfrac <- fracdiff(pricev, deltaf=input$deltaf)
    # retfrac <- rutils::lagit(retfrac)
    retfrac[1:131] <- 0
    return(retfrac)
    
  })  ##  end Calculate the returns
  
  ##  Recalculate the strategy
  datav <- shiny::reactive({
    
    symboln <- input$symboln
    cat("Recalculating datav for", symboln, "\n")
    # scaling_method <- input$scaling_method

    ##  Calculate the strategy datav
    # varv <- varv()
    # retp <- retp()
    pricev <- pricev()
    retfrac <- retfrac()
    # posv <- retfrac
    # datav <- posv*retp
    
    # Scale the strategy returns to have the same volatility as the underlying returns
    # datav <- datav*sd(retp[retp<0])/sd(datav[datav<0])
    
    ##  Bind together strategy datav
    datav <- cbind(pricev, retfrac)
    colnames(datav) <- c(symboln, "FracDiff")
    
    ##  Calculate Sharpe ratios
    # sharper <- sqrt(252)*sapply(datav, function(x) mean(x)/sd(x[x<0]))
    # values$sharper <- round(sharper, 3)
    
    # cat("sum(datav) = ", sum(datav), "\n")
    # datav <- cumsum(datav)
    # colnames(datav) <- c(symboln, "FracDiff")
    # datav <- cbind(cumsum(datav), posv)
    # colnames(datav) <- c(symboln, "Strategy", "Position")
    return(datav)

  })  ##  end Recalculate the strategy
  

  ##  Plot the cumulative scaled returns
  ##  Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    ##  Get the datav
    datav <- datav()

    ##  Create dygraph plot
    symboln <- input$symboln
    colv <- colnames(datav)
    captiont <- paste0(symboln, " Fractional Returns")
    dyplot <- dygraphs::dygraph(datav, main=captiont) %>%
      dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
      dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
      dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
      dyLegend(show="always", width=300)
    
    
    # dyplot <- dygraphs::dygraph(datav, main=captiont) %>%
    #   dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
    #   dyAxis("y2", label=colv[3], independentTicks=TRUE) %>%
    #   dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
    #   dySeries(name=colv[2], axis="y", strokeWidth=1, col="red") %>%
    #   dySeries(name=colv[3], axis="y2", strokeWidth=1, col="green") %>%
    #   dyLegend(show="always", width=300)
    
    ##  Return the dygraph object
    return(dyplot)


  })  ##  end output plot

}  ##  end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
