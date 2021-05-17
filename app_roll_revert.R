##############################
# This is a shiny app for simulating a rolling portfolio 
# optimization strategy with filtering of returns.
# It uses HighFreq::back_test()
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(shiny)
library(dygraphs)
library(HighFreq)

# Model and data setup
# Source the model function
# Source("C:/Develop/lecture_slides/scripts/roll_portf_new.R")
# max_eigen <- 2
load("C:/Develop/lecture_slides/data/sp500_prices.RData")
re_turns <- re_turns["2000/"]
# Random data
# re_turns <- xts(matrix(rnorm(NROW(returns_100)*NCOL(returns_100)), nc=NCOL(returns_100)), 
#                 index(returns_100))
n_cols <- NCOL(re_turns)
risk_free <- 0.03/260
ex_cess <- (re_turns - risk_free)
# Calculate returns on equal weight portfolio
in_dex <- xts(cumsum(rowMeans(re_turns)), index(re_turns))


## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("Rolling Portfolio Optimization Strategy for S&P500 Portfolio"),
  
  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the re_calculate variable is updated
    column(width=12, 
           h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
           actionButton("re_calculate", "Recalculate the Model"))
  ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input end points interval
    column(width=2, selectInput("inter_val", label="End points Interval",
                                choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Lookback interval",
                                min=2, max=125, value=6, step=1)),
    # Input look-back lag interval
    # column(width=2, sliderInput("look_lag", label="Lookback lag interval", min=1, max=10, value=2, step=1)),
    # Input the weight decay parameter
    # column(width=2, sliderInput("lamb_da", label="Weight decay:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input model weights type
    column(width=2, selectInput("typ_e", label="Portfolio weights type",
                                choices=c("max_sharpe", "min_var", "min_varpca", "rank"), selected="rank")),
    # Input number of eigenvalues for regularized matrix inverse
    column(width=2, sliderInput("max_eigen", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
    # Input the shrinkage intensity
    column(width=2, sliderInput("al_pha", label="Shrinkage intensity",
                                min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input the strategy coefficient: co_eff=1 for momentum, and co_eff=-1 for contrarian
    column(width=2, selectInput("co_eff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    column(width=2, numericInput("bid_offer", label="bid-offer:", value=0.0, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph"), width=12)
  
)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {
  
  # Recalculate the data and rerun the model
  da_ta <- reactive({
    # Get model parameters from input argument
    inter_val <- isolate(input$inter_val)
    max_eigen <- isolate(input$max_eigen)
    look_back <- isolate(input$look_back)
    # look_lag <- isolate(input$look_lag
    lamb_da <- isolate(input$lamb_da)
    typ_e <- isolate(input$typ_e)
    al_pha <- isolate(input$al_pha)
    co_eff <- as.numeric(isolate(input$co_eff))
    bid_offer <- isolate(input$bid_offer)
    # Model is recalculated when the re_calculate variable is updated
    input$re_calculate
    
    # Define end points
    end_points <- rutils::calc_endpoints(re_turns, inter_val=inter_val)
    # end_points <- ifelse(end_points<(n_cols+1), n_cols+1, end_points)
    end_points <- end_points[end_points > (n_cols+1)]
    n_rows <- NROW(end_points)
    # Define start_points
    start_points <- c(rep_len(1, look_back-1), end_points[1:(n_rows-look_back+1)])
    
    # Calculate the weight_s - commented out because it produces leak
    # weight_s <- exp(-lamb_da*1:look_back)
    # weight_s <- weight_s/sum(weight_s)
    # weight_s <- matrix(weight_s, nc=1)
    # ex_cess <- HighFreq::roll_conv(re_turns, weight_s=weight_s)
    # ex_cess <- rutils::lag_it(ex_cess, lagg=look_lag)
    
    # Rerun the model
    pnl_s <- HighFreq::back_test(ex_cess=ex_cess, 
                                 re_turns=re_turns,
                                 start_points=start_points-1,
                                 end_points=end_points-1,
                                 max_eigen=max_eigen, 
                                 al_pha=al_pha, 
                                 typ_e=typ_e,
                                 co_eff=co_eff,
                                 bid_offer=bid_offer)
    # pnl_s[which(is.na(pnl_s)), ] <- 0
    pnl_s <- cumsum(pnl_s)
    pnl_s <- cbind(pnl_s, in_dex)
    colnames(pnl_s) <- c("Strategy", "Index")
    pnl_s[c(1, end_points), ]
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph({
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
