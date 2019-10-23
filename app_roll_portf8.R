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
# Source("C:/Develop/R/lecture_slides/scripts/roll_portf_new.R")
# max_eigen <- 2
load("C:/Develop/R/lecture_slides/data/sp500_prices.RData")
re_turns <- returns_100["2000/"]
sym_bols <- colnames(re_turns)
n_cols <- NCOL(re_turns)
# Calculate returns on equal weight portfolio
index_rets <- xts(re_turns %*% rep(1/n_cols, n_cols), index(re_turns))
in_dex <- cumsum(index_rets)
# Calculate vector of monthly end points and start points
look_back <- 12
end_points <- rutils::calc_endpoints(re_turns, inter_val="months")
end_points[end_points<2*n_cols] <- 2*n_cols
n_rows <- NROW(end_points)
# sliding window
start_points <- c(rep_len(1, look_back-1), end_points[1:(n_rows-look_back+1)])
# OR expanding window
# start_points <- rep_len(1, NROW(end_points))
# risk_free is the daily risk-free rate
risk_free <- 0.03/252
# Calculate daily excess returns 
ex_cess <- re_turns - risk_free

percen_tile <- 0.1
quan_tile <- round(percen_tile*n_cols)

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("Rolling Portfolio Optimization Strategy for S&P500 Sub-portfolio"),
  
  fluidRow(
    # The Shiny App is re-calculated when the actionButton is clicked and the re_calculate variable is updated
    column(width=12, 
           h4("Click the button 'Recalculate the Model' to re-calculate the Shiny App."),
           actionButton("re_calculate", "Recalculate the Model"))
  ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input end points interval
    column(width=3, selectInput("inter_val", label="End points Interval",
                                choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback interval",
                                min=2, max=25, value=5, step=1)),
    # Input look-back lag interval
    # column(width=3, sliderInput("look_lag", label="Lookback lag interval", min=1, max=10, value=2, step=1)),
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
    column(width=3, sliderInput("percen_tile", label="percentile:", min=0.01, max=0.45, value=0.1, step=0.01)),
    # Input the strategy coefficient: co_eff=1 for momentum, and co_eff=-1 for contrarian
    column(width=3, selectInput("co_eff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    column(width=3, numericInput("bid_offer", label="bid-offer:", value=0.001, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphOutput("dy_graph"), width=12)
  
)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {
  
  # Re-calculate the data and rerun the model
  da_ta <- reactive({
    # Get model parameters from input argument
    inter_val <- isolate(input$inter_val)
    # max_eigen <- isolate(input$max_eigen)
    look_back <- isolate(input$look_back)
    # look_lag <- isolate(input$look_lag
    # lamb_da <- isolate(input$lamb_da)
    # typ_e <- isolate(input$typ_e)
    # al_pha <- isolate(input$al_pha)
    percen_tile <- isolate(input$percen_tile)
    co_eff <- as.numeric(isolate(input$co_eff))
    bid_offer <- isolate(input$bid_offer)
    # Model is re-calculated when the re_calculate variable is updated
    input$re_calculate
    
    # Define end points
    end_points <- rutils::calc_endpoints(re_turns, inter_val=inter_val)
    # end_points <- ifelse(end_points<(n_cols+1), n_cols+1, end_points)
    end_points <- end_points[end_points > (n_cols+1)]
    n_rows <- NROW(end_points)
    # Define start_points
    start_points <- c(rep_len(1, look_back-1), end_points[1:(n_rows-look_back+1)])
    
    # Define quantile
    quan_tile <- round(percen_tile*n_cols)
    
    # Calculate the weight_s - commented out because it produces leak
    # weight_s <- exp(-lamb_da*1:look_back)
    # weight_s <- weight_s/sum(weight_s)
    # weight_s <- matrix(weight_s, nc=1)
    # ex_cess <- HighFreq::roll_conv(re_turns, weight_s=weight_s)
    # ex_cess <- rutils::lag_it(ex_cess, lagg=look_lag)
    
    # Rerun the model
    pnl_s <- lapply(2:n_rows, function(it) {
      # Subset the ex_cess returns
      ex_cess <- ex_cess[start_points[it-1]:end_points[it-1], ]
      # Calculate the signal as volatility
      std_dev <- sapply(ex_cess, sd)
      # sig_nal <- std_dev
      # Calculate the signal as Sharpe ratio
      sig_nal <- ifelse((std_dev == 0), 0, colSums(ex_cess)/std_dev)
      # Calculate the signal as beta
      # index_rets <- index_rets[start_points[it-1]:end_points[it-1], ]
      # index_rets <- (index_rets - mean(index_rets))
      # ex_cess <- (ex_cess - colMeans(ex_cess))
      # sig_nal <- mean(drop(coredata(index_rets))*ex_cess)/sapply(ex_cess, var)
      ## Calculate the portfolio weights as ranks
      weight_s <- co_eff*sig_nal
      ## Calculate the portfolio weights as quantiles
      # weight_s <- numeric(n_cols)
      # names(weight_s) <- sym_bols
      # Calculate the signal order
      # or_der <- order(sig_nal)
      # weight_s[or_der[1:quan_tile]] <- (-co_eff)
      # weight_s[or_der[(n_cols-quan_tile+1):n_cols]] <- co_eff
      # Scale the weights
      weight_s <- weight_s/sum(abs(weight_s))
      # Subset the re_turns
      re_turns <- re_turns[(end_points[it-1]+1):end_points[it], ]
      # Calculate the out-of-sample portfolio returns
      xts(re_turns %*% weight_s, index(re_turns))
    }  # end anonymous function
    )  # end lapply
    
    # Calculate cumulative portfolio returns
    pnl_s <- rutils::do_call(rbind, pnl_s)
    pnl_s <- cumsum(pnl_s)
    
    pnl_s <- cbind(pnl_s, in_dex[index(pnl_s)])
    colnames(pnl_s) <- c("Strategy", "Index")
    # pnl_s[c(1, end_points), ]
    pnl_s
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
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
