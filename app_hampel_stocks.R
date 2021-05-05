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
# sym_bol <- "VTI"
# price_s <- Cl(rutils::etf_env$VTI)


## Set up S&P500 data
# if (!("sp500_env" %in% search()))
#   attach(sp500_env)
if (!("sp500_env" %in% ls())) {
  load(file="C:/Develop/lecture_slides/data/sp500.RData")
}  # end if
data_env <- sp500_env
# sym_bols <- names(data_env)
sym_bols <- c("PG", "CDNS", "YUM", "YUMC", "KHC", "SNPS", "ODFL", "CHRW", "AWK", "SO", "EA", "FIS", "DG", "BAX", "HRL", "MSFT", "XOM", "BSX", "JNJ", "CLX", "CL", "MCD", "WMT", "SBUX", "LLY", "ADM", "BIO", "XLNX", "ATVI", "DISH", "K", "SHW", "SIG", "CSCO", "INTU", "VRTX", "FB", "ORCL", "DUK", "KSS", "ROP", "AKAM", "MXIM", "TXN", "NEM", "COST", "EL", "JWN", "ACN", "FISV", "KLAC", "PFE", "TYL", "BIIB", "MCHP", "BBBY", "DRE", "PEP", "LIN", "NKE", "TROW", "LEN", "HOLX", "NVR", "UDR", "WEC", "DHI", "NI")
sym_bol <- "YUM"

cap_tion <- "Contrarian Strategy for S&P500 Stocks Using the Hampel Filter Over Prices"

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(cap_tion),
  
  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the re_calculate variable is updated
    # column(width=12, 
    #        h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
    #        actionButton("re_calculate", "Recalculate the Model"))
  ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=3, selectInput("sym_bol", label="Symbol",
                                choices=sym_bols, selected=sym_bol)),
    # Input end points interval
    # column(width=3, selectInput("inter_val", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback", min=3, max=30, value=9, step=1)),
    # Input lag trade parameter
    column(width=3, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1)),
    # Input threshold interval
    column(width=3, sliderInput("thresh_old", label="threshold", min=1.0, max=10.0, value=1.8, step=0.2))
    # Input the weight decay parameter
    # column(width=3, sliderInput("lamb_da", label="Weight decay:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input model weights type
    # column(width=3, selectInput("typ_e", label="Portfolio weights type",
    #                             choices=c("max_sharpe", "min_var", "min_varpca", "rank"), selected="rank")),
    # Input number of eigenvalues for regularized matrix inverse
    # column(width=3, sliderInput("max_eigen", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
    # Input the shrinkage intensity
    # column(width=3, sliderInput("al_pha", label="Shrinkage intensity",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input the percentile
    # column(width=3, sliderInput("percen_tile", label="percentile:", min=0.01, max=0.45, value=0.1, step=0.01)),
    # Input the strategy coefficient: co_eff=1 for momentum, and co_eff=-1 for contrarian
    # column(width=3, selectInput("co_eff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    # column(width=3, numericInput("bid_offer", label="bid-offer:", value=0.001, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph"), width=12)
  
)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {
  
  # Load the prices
  price_s <- reactive({
    
    # Get model parameters from input argument
    sym_bol <- input$sym_bol
    cat("Loading the data for: ", sym_bol, "\n")
    
    log(quantmod::Cl(get(sym_bol, data_env)))

  })  # end reactive code
  
  
  # Recalculate the data and rerun the model
  z_scores <- reactive({

    cat("Calculating the zscores\n")
    
    look_back <- input$look_back

    price_s <- price_s()
    
    # Rerun the model
    medi_an <- TTR::runMedian(price_s, n=look_back)
    medi_an[1:look_back, ] <- 1
    # ma_d <- TTR::runMAD(price_s, n=look_back)
    # ma_d[1:look_back, ] <- 1
    # z_scores <- ifelse(ma_d!=0, (price_s-medi_an)/ma_d, 0)
    z_scores <- (price_s-medi_an)
    mad_zscores <- TTR::runMAD(z_scores, n=look_back)
    mad_zscores[1:look_back, ] <- 0
    
    ifelse(mad_zscores > 0, z_scores/mad_zscores, 0)

  })  # end reactive code
  
  
  # Recalculate the model
  da_ta <- reactive({
    cat("Recalculating the model\n")
    
    # Get model parameters from input argument
    lagg <- input$lagg
    thresh_old <- input$thresh_old

    z_scores <- z_scores()
    # Calculate position_s and pnls from z-scores and ran_ge
    position_s <- rep(NA_integer_, NROW(z_scores))
    position_s[1] <- 0
    position_s <- ifelse(z_scores > thresh_old, -1, position_s)
    position_s <- ifelse(z_scores < (-thresh_old), 1, position_s)
    position_s <- zoo::na.locf(position_s, na.rm=FALSE)
    position_s <- rutils::lag_it(position_s, lagg=lagg)
    
    re_turns <- rutils::diff_it(price_s())
    pnl_s <- cumsum(position_s*re_turns)
    pnl_s <- cbind(pnl_s, cumsum(re_turns))
    colnames(pnl_s) <- c("Strategy", sym_bol)
    
    pnl_s
    
  })  # end reactive code
  
  
  # Return to the output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph({
    cap_tion <- paste("Contrarian Strategy for", input$sym_bol, "Using the Hampel Filter Over Prices")
    col_names <- colnames(da_ta())
    dygraphs::dygraph(da_ta(), main=cap_tion) %>%
      dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
      dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
      dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue")
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
