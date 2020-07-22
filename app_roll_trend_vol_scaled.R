##############################
# This is a shiny app for simulating a rolling portfolio 
# optimization strategy with filtering of returns.
# It uses HighFreq::back_test()
# 
# Just press the "Run App" button on upper right of this panel.
##############################

# Best parameters
# typ_e inter_val look_back max_eigen al_pha
# max_sharpe  days 15-35  6-7
# min_var  days 15-35  6-7
# max_sharpe  weeks 3-6  5-9
# min_var  weeks 3-5  6-9
# max_sharpe  weeks 16  6-9
# min_var  weeks 16  6-9
# Long weekly look_backs work very well because of IEF long position !
# max_sharpe  weeks 100  8-9
# min_var  weeks 100  8-9
# max_sharpe  months 7  6
# min_var  months 2-3  9
# Long monthly look_backs work very well because of IEF long position !
# max_sharpe  months 21-100  9
# min_var  months 21-100  9



## Setup code that runs once when the shiny app is started

# load packages
library(HighFreq)
library(shiny)
library(dygraphs)
# Model and data setup

# Source the model function
# source("C:/Develop/lecture_slides/scripts/roll_portf.R")


# Load ETF data
# sym_bols <- rutils::etf_env$sym_bols
# ETFs with smallest Hurst
# sym_bols <- c("XLP", "XLU", "VNQ", "XLV", "XLF", "XLB", "XLE", "XLY", "XLI", "XLK")
# ETFs with largest Hurst
# sym_bols <- c("DBC", "IEF", "VTI", "XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY")
# sym_bols <- c("VYM", "VEU", "DBC", "IEF", "VTI", "IWF", "IWD", "IWB")
# sym_bols <- c("XLU", "XLE", "XLK", "IWD", "VYM", "IWF", "XLI", "IEF", "VNQ", "DBC")
# sym_bols <- c("VYM", "VEU", "DBC", "IEF", "VTI", "IWF", "IWD", "IWB", "XLU", "XLE", "XLK", "XLI", "VNQ")
# sym_bols <- c("IVW", "VTI", "IWF", "IWD", "IWB", "VYM", "DBC", "IEF", "VEU", "SVXY", "VXX")
# sym_bols <- c("IVW", "VTI", "IWF", "IWD", "IWB", "VYM", "DBC", "IEF", "VEU")
# sym_bols <- c("VEU", "GLD", "EEM", "DBC", "VYM", "USO", "IWB", "IWD", "VTI")
# sym_bols <- c("TLT", "IEF", "USO", "GLD", "DBC", "XLY", "XLI", "XLB", "XLV", "XLE", "XLU", "XLK", "XLP", "IWD")

# sym_bols <- c("IVW", "VTI", "IWF", "IWD", "IWB", "VYM", "DBC", "VEU", "SVXY", "VXX")

# n_weights <- NROW(sym_bols)
# re_turns <- rutils::etf_env$re_turns[, sym_bols]
# Select rows with IEF data
# re_turns <- re_turns[index(rutils::etf_env$IEF)]
# 
# Or
# Calculate the first non-NA values and their positions.
# first_non_na <- sapply(re_turns, function(x_ts) {
#   match(TRUE, !is.na(x_ts))
# })  # end sapply
# Find first row containing at least 3 non-NA values.
# sort(first_non_na)[3]
# Select rows containing at least 3 non-NA values.
# re_turns <- re_turns[(sort(first_non_na)[3]):NROW(re_turns)]
# re_turns <- re_turns[-(1:(sort(first_non_na)[7]-1))]


# Calculate the volumes
# volume_s <- lapply(sym_bols, function(sym_bol) {
#   quantmod::Vo(get(x=sym_bol, envir=rutils::etf_env))
# })  # end lapply
# volume_s <- rutils::do_call(cbind, volume_s)
# colnames(volume_s) <- sym_bols
# volume_s <- volume_s[index(re_turns)]
# volume_s[volume_s == 0] <- NA
# volume_s <- zoo::na.locf(volume_s, na.rm=FALSE)
# volume_s <- zoo::na.locf(volume_s, fromLast=TRUE)
# Calculate the row ranks


############
# S&P100
load("C:/Develop/lecture_slides/data/sp500_returns.RData")
re_turns <- re_turns["2000-01-01/"]
sym_bols <- colnames(re_turns)
n_weights <- NROW(sym_bols)


# Copy over NA values with zeros
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
# sum(is.na(re_turns))
# ex_cess <- matrixStats::rowRanks(re_turns)
# ex_cess <- (ex_cess - rowMeans(ex_cess))
# Scale re_turns by the volumes
# ex_cess <- re_turns/sqrt(volume_s)
ex_cess <- re_turns

# Benchmark index
# in_dex <- xts(cumsum(rowMeans(re_turns)), index(re_turns))
# in_dex <- Cl(rutils::etf_env$VTI)[index(re_turns)]
in_dex <- rutils::etf_env$re_turns[index(re_turns), "VTI"]
in_dex[1] <- 0
in_dex <- zoo::na.locf(in_dex, na.rm=FALSE)
in_dex <- cumsum(in_dex)


# Portfolio with largest Hurst
# weight_s <- read.csv(file="C:/Develop/lecture_slides/data/etf_hurst_weights.csv", stringsAsFactors=FALSE)
# weight_s <- structure(as.numeric(weight_s$x), names=weight_s$X)
# portf_hurst <- -drop(re_turns %*% weight_s)
# portf_hurst <- sd(ex_cess$VTI)/sd(portf_hurst)*portf_hurst


# End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("Rolling Portfolio Optimization Strategy for ETFs"),
  
  # create single row with two slider inputs
  fluidRow(
    # Input end points interval
    column(width=3, selectInput("inter_val", label="End points Interval",
                choices=c("days", "weeks", "months", "years"), selected="weeks")),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback interval",
                                min=1, max=100, value=18, step=1)),
    column(width=3, sliderInput("lamb_da", label="Weight decay:",
                                min=0.01, max=0.99, value=0.01, step=0.05)),
    # Input end points interval
    column(width=3, selectInput("typ_e", label="Weights type",
                                choices=c("max_sharpe", "max_sharpe_median", "min_var", "min_varpca", "rank", "rankrob", "quan_tile"), selected="max_sharpe")),
    # Input number of eigenvalues for regularized matrix inverse
    column(width=3, numericInput("max_eigen", "Number of eigenvalues", value=6)),
    # Input the shrinkage intensity
    column(width=3, sliderInput("al_pha", label="Shrinkage intensity",
                                min=0.01, max=0.99, value=0.01, step=0.05)),
    # Input the quantile
    column(width=3, sliderInput("pro_b", label="Confidence level",
                                min=0.01, max=0.49, value=0.25, step=0.01)),
    # column(width=3, numericInput("co_eff", "Weight coefficient:", value=1)),
    column(width=3, selectInput("co_eff", label="Weight coefficient",
                                choices=c(1, -1), selected=1)),
    actionButton("re_calculate", "Recalculate the Model")
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(dygraphOutput("dy_graph"), width=12)
)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {

  # re-calculate the data and rerun the model
  da_ta <- reactive({
    # get model parameters from input argument
    inter_val <- isolate(input$inter_val)
    max_eigen <- isolate(input$max_eigen)
    look_back <- isolate(input$look_back)
    lamb_da <- isolate(input$lamb_da)
    typ_e <- isolate(input$typ_e)
    al_pha <- isolate(input$al_pha)
    pro_b <- isolate(input$pro_b)
    co_eff <- as.numeric(isolate(input$co_eff))
    # Model is re-calculated when the re_calculate variable is updated
    input$re_calculate

    # Define end points
    end_p <- rutils::calc_endpoints(re_turns, inter_val=inter_val)
    # end_p <- ifelse(end_p<(n_weights+1), n_weights+1, end_p)
    end_p <- end_p[end_p > 2*n_weights]
    n_rows <- NROW(end_p)
    # Define start points
    start_p <- c(rep_len(1, look_back-1), end_p[1:(n_rows-look_back+1)])
    
    # Calculate the weight_s
    # weight_s <- exp(-lamb_da*(1:look_back))
    # weight_s <- weight_s/sum(weight_s)
    # weight_s <- matrix(weight_s, nc=1)
    # Calculate smoothed ex_cess returns
    # ex_cess <- HighFreq::roll_conv(ex_cess, weight_s=weight_s)
    # ex_cess <- HighFreq::lag_it(ex_cess, lagg=1)
    
    # needs more work: Shrink to portfolio with largest Hurst
    # ex_cess <- (1-al_pha)*ex_cess + al_pha*portf_hurst
    
    
    # Rerun the model
    pnl_s <- HighFreq::back_test(ex_cess=ex_cess, 
                                 re_turns=re_turns,
                                 start_points=start_p-1,
                                 end_points=end_p-1,
                                 pro_b=pro_b,
                                 max_eigen=max_eigen, 
                                 al_pha=al_pha, 
                                 typ_e=typ_e,
                                 co_eff=co_eff)
    pnl_s[which(is.na(pnl_s)), ] <- 0
    pnl_s <- cumsum(pnl_s)
    # pnl_s <- cumprod(1 + pnl_s)
    pnl_s <- cbind(pnl_s, in_dex)
    colnames(pnl_s) <- c("Strategy", "Index")
    pnl_s[c(1, end_p), ]
  })  # end reactive code
  
  # return to output argument a dygraph plot with two y-axes

    output$dy_graph <- renderDygraph({
    col_names <- colnames(da_ta())
    dygraphs::dygraph(da_ta(), main="Rolling Portfolio Optimization Strategy") %>%
      dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
      dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
      dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue")
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
