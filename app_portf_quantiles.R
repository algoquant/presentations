##############################
# This is a shiny app for simulating a momentum strategy 
# using the percentiles of trailing returns.
# It's written in pure R and does not use HighFreq::back_test()
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
load("C:/Develop/lecture_slides/data/sp500_returns.RData")
ret_s <- returns_100["2000/"]
sym_bols <- colnames(ret_s)
n_cols <- NCOL(ret_s)
# Copy over NA values
ret_s[1, is.na(ret_s[1, ])] <- 0
ret_s <- zoo::na.locf(ret_s, na.rm=FALSE)
# Calculate returns on equal weight portfolio
in_dex <- rowMeans(ret_s)
std_dev <- sd(in_dex[in_dex<0])
# sharp_e <- sqrt(252)*mean(in_dex)/std_dev
in_dex <- xts(in_dex, index(ret_s))

# Calculate vector of monthly end points and start points
look_back <- 12
end_points <- rutils::calc_endpoints(ret_s, inter_val="months")
end_points[end_points<2*n_cols] <- 2*n_cols
n_rows <- NROW(end_points)
# sliding window
start_points <- c(rep_len(1, look_back-1), end_points[1:(n_rows-look_back+1)])
# OR expanding window
# start_points <- rep_len(1, NROW(end_points))
# risk_free is the daily risk-free rate
risk_free <- 0.03/252
# Calculate daily excess returns 
ex_cess <- ret_s - risk_free

percen_tile <- 0.1
quan_tile <- round(percen_tile*n_cols)

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("Rolling Portfolio Optimization Strategy for S&P500 Sub-portfolio"),
  
  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the re_calculate variable is updated
    column(width=12, 
           h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
           actionButton("re_calculate", "Recalculate the Model"))
  ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input end points interval
    column(width=3, selectInput("inter_val", label="End points Interval",
                                choices=c("days", "weeks", "months", "years"), selected="weeks")),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback interval",
                                min=2, max=70, value=5, step=1)),
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
    # Input the strategy factor: fac_tor=1 for momentum, and fac_tor=-1 for contrarian
    column(width=3, selectInput("fac_tor", "factor (1 momentum, -1 contrarian):", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    column(width=3, numericInput("bid_offer", label="bid-offer:", value=0.001, step=0.001))
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
    # max_eigen <- isolate(input$max_eigen)
    look_back <- isolate(input$look_back)
    # look_lag <- isolate(input$look_lag
    # lamb_da <- isolate(input$lamb_da)
    # typ_e <- isolate(input$typ_e)
    # al_pha <- isolate(input$al_pha)
    percen_tile <- isolate(input$percen_tile)
    fac_tor <- as.numeric(isolate(input$fac_tor))
    bid_offer <- isolate(input$bid_offer)
    # Model is recalculated when the re_calculate variable is updated
    input$re_calculate
    
    # Define end points
    end_points <- rutils::calc_endpoints(ret_s, inter_val=inter_val)
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
    # ex_cess <- HighFreq::roll_conv(ret_s, weight_s=weight_s)
    # ex_cess <- rutils::lag_it(ex_cess, lagg=look_lag)
    
    # Rerun the model
    pnl_s <- lapply(2:n_rows, function(it) {
      # Subset the ex_cess returns
      sub_excess <- ex_cess[start_points[it-1]:end_points[it-1], ]
      # Calculate the signal as volatility
      std_dev <- sapply(sub_excess, sd)
      # sig_nal <- std_dev
      # Calculate the signal as Sharpe ratio
      sig_nal <- ifelse(is.na(std_dev) | (std_dev == 0), 0, colSums(sub_excess)/std_dev)
      # Calculate the signal as beta
      # in_dex <- in_dex[start_points[it-1]:end_points[it-1], ]
      # in_dex <- (in_dex - mean(in_dex))
      # sub_excess <- (sub_excess - colMeans(sub_excess))
      # sig_nal <- mean(drop(coredata(in_dex))*sub_excess)/sapply(sub_excess, var)
      ## Calculate the portfolio weights as ranks
      # weight_s <- fac_tor*sig_nal
      ## Calculate the portfolio weights as quantiles
      weight_s <- numeric(n_cols)
      names(weight_s) <- sym_bols
      # Calculate the signal order
      or_der <- order(sig_nal)
      weight_s[or_der[1:quan_tile]] <- (-fac_tor)
      weight_s[or_der[(n_cols-quan_tile+1):n_cols]] <- fac_tor
      # Scale the weights
      weight_s <- weight_s/sum(abs(weight_s))
      # Subset the ret_s
      sub_returns <- ret_s[(end_points[it-1]+1):end_points[it], ]
      # Calculate the out-of-sample portfolio returns
      xts(sub_returns %*% weight_s, index(sub_returns))
    }  # end anonymous function
    )  # end lapply
    
    # Calculate cumulative portfolio returns
    pnl_s <- rutils::do_call(rbind, pnl_s)
    pnl_s <- std_dev*pnl_s/sd(pnl_s[pnl_s<0])
    pnl_s <- cbind(pnl_s, in_dex[index(pnl_s)])
    sharp_e <- sqrt(252)*sapply(pnl_s, function(x) mean(x)/sd(x[x<0]))
    sharp_e <- round(sharp_e, 3)
    pnl_s <- cumsum(pnl_s)
    colnames(pnl_s) <- paste0(c("Strategy SR=", "Index SR="), sharp_e)
    # pnl_s[c(1, end_points), ]
    pnl_s
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
