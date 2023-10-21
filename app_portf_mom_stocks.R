##############################
# This is a shiny app for simulating a momentum strategy 
# using the quantiles of trailing returns.
# It's written in pure R and does not use HighFreq::back_test()
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(shiny)
library(dygraphs)
library(rutils)

# Model and data setup
# dimax <- 2
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
rets <- returns100["2000/"]
symbolv <- colnames(rets)
nstocks <- NCOL(rets)
# Copy over NA values
rets[1, is.na(rets[1, ])] <- 0
rets <- zoo::na.locf(rets, na.rm=FALSE)
# Calculate returns on equal weight portfolio
indeks <- rowMeans(rets)
stdev <- sd(indeks[indeks<0])
# sharper <- sqrt(252)*mean(indeks)/stdev
indeks <- xts(indeks, index(rets))

# Calculate vector of monthly end points and start points
look_back <- 12
endp <- rutils::calc_endpoints(rets, interval="months")
endp[endp<2*nstocks] <- 2*nstocks
nrows <- NROW(endp)
# sliding window
startp <- c(rep_len(1, look_back-1), endp[1:(nrows-look_back+1)])
# OR expanding window
# startp <- rep_len(1, NROW(endp))
# riskf is the daily risk-free rate
riskf <- 0.03/252
# Calculate daily excess returns 
excess <- rets - riskf

percent <- 0.1
quantilev <- round(percent*nstocks)

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Momentum Strategy for S&P500 Portfolio"),

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
                                choices=c("days", "weeks", "months", "years"), selected="weeks")),
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Lookback interval",
                                min=2, max=70, value=5, step=1)),
    # Input look-back lag interval
    # column(width=2, sliderInput("look_lag", label="Lookback lag interval", min=1, max=10, value=2, step=1)),
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
    column(width=2, sliderInput("percent", label="percentile:", min=0.01, max=0.45, value=0.1, step=0.01)),
    # Input the strategy factor: coeff=1 for momentum, and coeff=-1 for contrarian
    column(width=2, selectInput("coeff", "factor (1 momentum, -1 contrarian):", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    column(width=2, numericInput("bid_offer", label="bid-offer:", value=0.001, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # Recalculate the data and rerun the model
  datav <- shiny::reactive({
    # Get model parameters from input argument
    interval <- isolate(input$interval)
    # dimax <- isolate(input$dimax)
    look_back <- isolate(input$look_back)
    # look_lag <- isolate(input$look_lag
    # lambda <- isolate(input$lambda)
    # typev <- isolate(input$typev)
    # alpha <- isolate(input$alpha)
    percent <- isolate(input$percent)
    coeff <- as.numeric(isolate(input$coeff))
    bid_offer <- isolate(input$bid_offer)
    # Model is recalculated when the recalcb variable is updated
    input$recalcb
    
    # Define end points
    endp <- rutils::calc_endpoints(rets, interval=interval)
    # endp <- ifelse(endp< nstocks+1), nstocks+1, endp)
    endp <- endp[endp > (nstocks+1)]
    nrows <- NROW(endp)
    # Define startp
    startp <- c(rep_len(1, look_back-1), endp[1:(nrows-look_back+1)])
    
    # Define quantile
    quantilev <- round(percent*nstocks)
    
    # Calculate the weights - commented out because it produces leak
    # weights <- exp(-lambda*1:look_back)
    # weights <- weights/sum(weights)
    # weights <- matrix(weights, nc=1)
    # excess <- HighFreq::roll_conv(rets, weightv=weights)
    # excess <- rutils::lagit(excess, lagg=look_lag)
    
    # Rerun the model
    pnls <- lapply(2:nrows, function(it) {
      # Subset the excess returns
      sub_excess <- excess[startp[it-1]:endp[it-1], ]
      # Calculate the signal as volatility
      stdev <- sapply(sub_excess, sd)
      # score <- stdev
      # Calculate the signal as Sharpe ratio
      score <- ifelse(is.na(stdev) | (stdev == 0), 0, colSums(sub_excess)/stdev)
      # Calculate the signal as beta
      # indeks <- indeks[startp[it-1]:endp[it-1], ]
      # indeks <- (indeks - mean(indeks))
      # sub_excess <- (sub_excess - colMeans(sub_excess))
      # score <- mean(drop(coredata(indeks))*sub_excess)/sapply(sub_excess, var)
      ## Calculate the portfolio weights as ranks
      # weights <- coeff*score
      ## Calculate the portfolio weights as quantiles
      weights <- numeric(nstocks)
      names(weights) <- symbolv
      # Calculate the signal order
      ordern <- order(score)
      weights[ordern[1:quantilev]] <- (-coeff)
      weights[ordern[(nstocks-quantilev+1):nstocks]] <- coeff
      # Scale the weights
      weights <- weights/sum(abs(weights))
      # Subset the rets
      sub_retv <- rets[(endp[it-1]+1):endp[it], ]
      # Calculate the out-of-sample portfolio returns
      xts(sub_retv %*% weights, index(sub_retv))
    }  # end anonymous function
    )  # end lapply
    
    # Calculate cumulative portfolio returns
    pnls <- rutils::do_call(rbind, pnls)
    pnls <- stdev*pnls/sd(pnls[pnls<0])
    pnls <- cbind(pnls, indeks[index(pnls)])
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 3)
    pnls <- cumsum(pnls)
    colnames(pnls) <- paste0(c("Strategy SR=", "Index SR="), sharper)
    # pnls[c(1, endp), ]
    pnls
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    colnamev <- colnames(datav())
    dygraphs::dygraph(datav(), main="Momentum Strategy for S&P500 Portfolio") %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="blue") %>%
      dyLegend(show="always", width=500)
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
