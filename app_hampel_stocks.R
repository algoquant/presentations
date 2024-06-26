##############################
# This is a shiny app for simulating a contrarian strategy 
# using the Hampel filter.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Model and data setup

## VTI ETF daily bars
# symbol <- "VTI"
# pricev <- Cl(rutils::etfenv$VTI)


## Set up S&P500 data
# if (!("sp500env" %in% search()))
#   attach(sp500env)
if (!("sp500env" %in% ls())) {
  load(file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
}  # end if
datenv <- sp500env
# symbolv <- names(datenv)
symbolv <- c("PG", "CDNS", "YUM", "YUMC", "KHC", "SNPS", "ODFL", "CHRW", "AWK", "SO", "EA", "FIS", "DG", "BAX", "HRL", "MSFT", "XOM", "BSX", "JNJ", "CLX", "CL", "MCD", "WMT", "SBUX", "LLY", "ADM", "BIO", "XLNX", "ATVI", "DISH", "K", "SHW", "SIG", "CSCO", "INTU", "VRTX", "FB", "ORCL", "DUK", "KSS", "ROP", "AKAM", "MXIM", "TXN", "NEM", "COST", "EL", "JWN", "ACN", "FISV", "KLAC", "PFE", "TYL", "BIIB", "MCHP", "BBBY", "DRE", "PEP", "LIN", "NKE", "TROW", "LEN", "HOLX", "NVR", "UDR", "WEC", "DHI", "NI")
symbol <- "YUM"

captiont <- "Contrarian Strategy for S&P500 Stocks Using the Hampel Filter Over Prices"

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
    # column(width=12, 
    #        h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
    #        actionButton("recalcb", "Recalculate the Model"))
  ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol",
                                choices=symbolv, selected=symbol)),
    # Input end points interval
    # column(width=2, selectInput("interval", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input look-back interval
    column(width=2, sliderInput("lookb", label="Lookback", min=3, max=30, value=9, step=1)),
    # Input lag trade parameter
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1)),
    # Input threshold interval
    column(width=2, sliderInput("threshold", label="threshold", min=1.0, max=10.0, value=1.8, step=0.2))
    # Input the weight decay parameter
    # column(width=2, sliderInput("lambda", label="Weight decay:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input model weights type
    # column(width=2, selectInput("typev", label="Portfolio weights type",
    #                             choices=c("max_sharpe", "min_var", "min_varpca", "rank"), selected="rank")),
    # Input number of eigenvalues for regularized matrix inverse
    # column(width=2, sliderInput("dimax", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
    # Input the shrinkage intensity
    # column(width=2, sliderInput("alpha", label="Shrinkage intensity",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input the percentile
    # column(width=2, sliderInput("quant", label="percentile:", min=0.01, max=0.45, value=0.1, step=0.01)),
    # Input the strategy coefficient: coeff=1 for momentum, and coeff=-1 for contrarian
    # column(width=2, selectInput("coeff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-ask spread
    # column(width=2, numericInput("bidask", label="bid-ask:", value=0.001, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # Load the prices
  pricev <- shiny::reactive({
    
    # Get model parameters from input argument
    symbol <- input$symbol
    cat("Loading the data for: ", symbol, "\n")
    
    log(quantmod::Cl(get(symbol, datenv)))

  })  # end reactive code
  
  
  # Recalculate the data and rerun the model
  zscores <- shiny::reactive({

    cat("Calculating the zscores\n")
    
    lookb <- input$lookb

    pricev <- prices()
    
    # Rerun the model
    medianv <- TTR::runMedian(pricev, n=lookb)
    medianv[1:lookb, ] <- 1
    # madv <- TTR::runMAD(pricev, n=lookb)
    # madv[1:lookb, ] <- 1
    # zscores <- ifelse(madv!=0, (pricev-medianv)/madv, 0)
    zscores <- (pricev-medianv)
    mad_zscores <- TTR::runMAD(zscores, n=lookb)
    mad_zscores[1:lookb, ] <- 0
    
    ifelse(mad_zscores > 0, zscores/mad_zscores, 0)

  })  # end reactive code
  
  
  # Recalculate the model
  datav <- shiny::reactive({
    cat("Recalculating the model\n")
    
    # Get model parameters from input argument
    lagg <- input$lagg
    threshold <- input$threshold

    zscores <- zscores()
    # Calculate posv and pnls from z-scores and rangev
    posv <- rep(NA_integer_, NROW(zscores))
    posv[1] <- 0
    posv <- ifelse(zscores > threshold, -1, posv)
    posv <- ifelse(zscores < (-threshold), 1, posv)
    posv <- zoo::na.locf(posv, na.rm=FALSE)
    posv <- rutils::lagit(posv, lagg=lagg)
    
    retv <- rutils::diffit(pricev())
    pnls <- cumsum(posv*retv)
    pnls <- cbind(pnls, cumsum(retv))
    colnames(pnls) <- c("Strategy", symbol)
    
    pnls
    
  })  # end reactive code
  
  
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    captiont <- paste("Contrarian Strategy for", input$symbol, "Using the Hampel Filter Over Prices")
    colnamev <- colnames(datav())
    dygraphs::dygraph(datav(), main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="blue")
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
