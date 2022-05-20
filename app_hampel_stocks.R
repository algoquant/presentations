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
# prices <- Cl(rutils::etfenv$VTI)


## Set up S&P500 data
# if (!("sp500env" %in% search()))
#   attach(sp500env)
if (!("sp500env" %in% ls())) {
  load(file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
}  # end if
data_env <- sp500env
# symbolv <- names(data_env)
symbolv <- c("PG", "CDNS", "YUM", "YUMC", "KHC", "SNPS", "ODFL", "CHRW", "AWK", "SO", "EA", "FIS", "DG", "BAX", "HRL", "MSFT", "XOM", "BSX", "JNJ", "CLX", "CL", "MCD", "WMT", "SBUX", "LLY", "ADM", "BIO", "XLNX", "ATVI", "DISH", "K", "SHW", "SIG", "CSCO", "INTU", "VRTX", "FB", "ORCL", "DUK", "KSS", "ROP", "AKAM", "MXIM", "TXN", "NEM", "COST", "EL", "JWN", "ACN", "FISV", "KLAC", "PFE", "TYL", "BIIB", "MCHP", "BBBY", "DRE", "PEP", "LIN", "NKE", "TROW", "LEN", "HOLX", "NVR", "UDR", "WEC", "DHI", "NI")
symbol <- "YUM"

captiont <- "Contrarian Strategy for S&P500 Stocks Using the Hampel Filter Over Prices"

## End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel(captiont),
  
  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the re_calculate variable is updated
    # column(width=12, 
    #        h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
    #        actionButton("re_calculate", "Recalculate the Model"))
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
    column(width=2, sliderInput("look_back", label="Lookback", min=3, max=30, value=9, step=1)),
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
    # column(width=2, sliderInput("eigen_max", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
    # Input the shrinkage intensity
    # column(width=2, sliderInput("alpha", label="Shrinkage intensity",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input the percentile
    # column(width=2, sliderInput("quant", label="percentile:", min=0.01, max=0.45, value=0.1, step=0.01)),
    # Input the strategy coefficient: coeff=1 for momentum, and coeff=-1 for contrarian
    # column(width=2, selectInput("coeff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    # column(width=2, numericInput("bid_offer", label="bid-offer:", value=0.001, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # Load the prices
  prices <- shiny::reactive({
    
    # Get model parameters from input argument
    symbol <- input$symbol
    cat("Loading the data for: ", symbol, "\n")
    
    log(quantmod::Cl(get(symbol, data_env)))

  })  # end reactive code
  
  
  # Recalculate the data and rerun the model
  zscores <- shiny::reactive({

    cat("Calculating the zscores\n")
    
    look_back <- input$look_back

    prices <- prices()
    
    # Rerun the model
    medi_an <- TTR::runMedian(prices, n=look_back)
    medi_an[1:look_back, ] <- 1
    # madv <- TTR::runMAD(prices, n=look_back)
    # madv[1:look_back, ] <- 1
    # zscores <- ifelse(madv!=0, (prices-medi_an)/madv, 0)
    zscores <- (prices-medi_an)
    mad_zscores <- TTR::runMAD(zscores, n=look_back)
    mad_zscores[1:look_back, ] <- 0
    
    ifelse(mad_zscores > 0, zscores/mad_zscores, 0)

  })  # end reactive code
  
  
  # Recalculate the model
  datav <- shiny::reactive({
    cat("Recalculating the model\n")
    
    # Get model parameters from input argument
    lagg <- input$lagg
    threshold <- input$threshold

    zscores <- zscores()
    # Calculate posit and pnls from z-scores and rangev
    posit <- rep(NA_integer_, NROW(zscores))
    posit[1] <- 0
    posit <- ifelse(zscores > threshold, -1, posit)
    posit <- ifelse(zscores < (-threshold), 1, posit)
    posit <- zoo::na.locf(posit, na.rm=FALSE)
    posit <- rutils::lagit(posit, lagg=lagg)
    
    returns <- rutils::diffit(prices())
    pnls <- cumsum(posit*returns)
    pnls <- cbind(pnls, cumsum(returns))
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
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue")
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
