##############################
# This is a shiny app for simulating a rolling portfolio 
# optimization strategy with filtering of returns.
# It uses HighFreq::back_test()
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# load packages
library(HighFreq)
library(shiny)
library(dygraphs)
# Model and data setup

# Source the model function
# source("C:/Develop/lecture_slides/scripts/roll_portf.R")


# Load PCA data
# load("C:/Develop/data/pca_rets.RData")
# re_turns <- pca_rets[, 1:9]


# Load ETF data
# load("C:/Develop/lecture_slides/data/etf_data.RData")
# re_turns <- rutils::etf_env$re_turns
# sym_bols <- c("DBC", "IEF", "VTI", "XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY")
# re_turns <- rutils::etf_env$re_turns[, sym_bols]


# Load S&P500 stocks
# load("C:/Develop/data/returns_100.RData")
load("C:/Develop/lecture_slides/data/sp500_prices.RData")
# # Select the columns with non-zero returns
# re_turns <- re_turns[, !(re_turns[NROW(re_turns) %/% 10, ] == 0)]
# # Select 100 columns to reduce computations
# set.seed(1121)  # reset random number generator
# sam_ple <- sample(1:NCOL(re_turns), 100)
# re_turns <- re_turns[, sam_ple]
re_turns <- returns_100
# re_turns <- cbind(re_turns, rutils::etf_env$re_turns$SVXY, rutils::etf_env$re_turns$VXX)

re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
n_weights <- NCOL(re_turns)

# Random data
# coredata(re_turns) <- matrix(rnorm(prod(dim(re_turns)))/100, nc=n_weights)


risk_free <- 0.03/260
ex_cess <- (re_turns - risk_free)
# ex_cess <- returns_100
# calculate returns on equal weight portfolio
# in_dex <- xts(cumprod(1 + rowMeans(re_turns)), index(re_turns))
in_dex <- xts(cumsum(rowMeans(re_turns)), index(re_turns))

# End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("Rolling Portfolio Optimization Strategy for S&P500 Stocks"),
  
  # create single row with two slider inputs
  fluidRow(
    # Input end points interval
    column(width=3, selectInput("inter_val", label="End points Interval",
                choices=c("days", "weeks", "months", "years"), selected="weeks")),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback interval",
                                min=1, max=350, value=55, step=1)),
    column(width=3, sliderInput("lamb_da", label="Weight decay:",
                                min=0.01, max=0.99, value=0.01, step=0.05)),
    # Input end points interval
    column(width=3, selectInput("typ_e", label="Weights type",
                                choices=c("max_sharpe", "max_sharpe_median", "min_var", "min_varpca", "rank", "rankrob", "quan_tile"), selected="max_sharpe")),
    # Input number of eigenvalues for regularized matrix inverse
    column(width=3, numericInput("max_eigen", "Number of eigenvalues", value=15)),
    # Input the shrinkage intensity
    column(width=3, sliderInput("al_pha", label="Shrinkage intensity",
                                min=0.01, max=0.99, value=0.7, step=0.05)),
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
    end_points <- rutils::calc_endpoints(re_turns, inter_val=inter_val)
    # end_points <- ifelse(end_points<(n_weights+1), n_weights+1, end_points)
    end_points <- end_points[end_points > (n_weights+1)]
    len_gth <- NROW(end_points)
    # Define start_points
    start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
    
    # Calculate the weight_s
    weight_s <- exp(-lamb_da*(1:look_back))
    weight_s <- weight_s/sum(weight_s)
    weight_s <- matrix(weight_s, nc=1)
    # calculate smoothed ex_cess returns
    ex_cess <- HighFreq::roll_conv(ex_cess, weight_s=weight_s)
    # ex_cess <- HighFreq::lag_it(ex_cess, lagg=1)
    
    # Rerun the model
    pnl_s <- HighFreq::back_test(ex_cess=ex_cess, 
                                 re_turns=re_turns,
                                 start_points=start_points-1,
                                 end_points=end_points-1,
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
    pnl_s[c(1, end_points), ]
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
