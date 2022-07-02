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
# Source("/Users/jerzy/Develop/lecture_slides/scripts/roll_portf_new.R")
# dimax <- 2
load("/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
returns <- returns["2000/"]
# Random data
# returns <- xts(matrix(rnorm(NROW(returns100)*NCOL(returns100)), nc=NCOL(returns100)), 
#                 index(returns100))
ncols <- NCOL(returns)
riskf <- 0.03/260
excess <- (returns - riskf)
# Calculate returns on equal weight portfolio
indeks <- xts(cumsum(rowMeans(returns)), index(returns))


## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Rolling Portfolio Optimization Strategy for S&P500 Portfolio"),
  
  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
    column(width=12, 
           h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
           actionButton("recalcb", "Recalculate the Model"))
  ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input end points interval
    column(width=2, selectInput("interval", label="End points Interval",
                                choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Lookback interval",
                                min=2, max=125, value=6, step=1)),
    # Input look-back lag interval
    # column(width=2, sliderInput("look_lag", label="Lookback lag interval", min=1, max=10, value=2, step=1)),
    # Input the weight decay parameter
    # column(width=2, sliderInput("lambda", label="Weight decay:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input model weights type
    column(width=2, selectInput("typev", label="Portfolio weights type",
                                choices=c("max_sharpe", "min_var", "min_varpca", "rank"), selected="rank")),
    # Input number of eigenvalues for regularized matrix inverse
    column(width=2, sliderInput("dimax", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
    # Input the shrinkage intensity
    column(width=2, sliderInput("alpha", label="Shrinkage intensity",
                                min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input the strategy coefficient: coeff=1 for momentum, and coeff=-1 for contrarian
    column(width=2, selectInput("coeff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    column(width=2, numericInput("bid_offer", label="bid-offer:", value=0.0, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # Recalculate the data and rerun the model
  datav <- shiny::reactive({
    # Get model parameters from input argument
    interval <- isolate(input$interval)
    dimax <- isolate(input$dimax)
    look_back <- isolate(input$look_back)
    # look_lag <- isolate(input$look_lag
    lambda <- isolate(input$lambda)
    typev <- isolate(input$typev)
    alpha <- isolate(input$alpha)
    coeff <- as.numeric(isolate(input$coeff))
    bid_offer <- isolate(input$bid_offer)
    # Model is recalculated when the recalcb variable is updated
    input$recalcb
    
    # Define end points
    endp <- rutils::calc_endpoints(returns, interval=interval)
    # endp <- ifelse(endp< ncols+1), ncols+1, endp)
    endp <- endp[endp > (ncols+1)]
    nrows <- NROW(endp)
    # Define startp
    startp <- c(rep_len(1, look_back-1), endp[1:(nrows-look_back+1)])
    
    # Calculate the weights - commented out because it produces leak
    # weights <- exp(-lambda*1:look_back)
    # weights <- weights/sum(weights)
    # weights <- matrix(weights, nc=1)
    # excess <- HighFreq::roll_conv(returns, weights=weights)
    # excess <- rutils::lagit(excess, lagg=look_lag)
    
    # Rerun the model
    pnls <- HighFreq::back_test(excess=excess, 
                                 returns=returns,
                                 startp=startp-1,
                                 endp=endp-1,
                                 dimax=dimax, 
                                 alpha=alpha, 
                                 typev=typev,
                                 coeff=coeff,
                                 bid_offer=bid_offer)
    # pnls[which(is.na(pnls)), ] <- 0
    pnls <- cumsum(pnls)
    pnls <- cbind(pnls, indeks)
    colnames(pnls) <- c("Strategy", "Index")
    pnls[c(1, endp), ]
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    colnamev <- colnames(datav())
    dygraphs::dygraph(datav(), main="Rolling Portfolio Optimization Strategy") %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue")
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
