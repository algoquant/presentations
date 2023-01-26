##############################
# This is a shiny app for plotting returns in trading time 
# scaled by the trading volumes.
# It uses reactive code to avoid unnecessary calculations.
# This is the best performing univariate strategy.
# 
# Just press the "Run App" button on upper right of this panel.
##############################


## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Model and data setup

# symbolv <- names(data_env)
# symbolv <- c("SPY", "LODE", "GME")
symbolv <- rutils::etfenv$symbolv
symbol <- "VTI"

captiont <- paste("Histogram of Returns Scaled by the Trading Volumes")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  # fluidRow(
  # The Shiny App is recalculated when the actionButton is clicked and the add_annotations variable is updated
  #   column(width=12,
  #          h4("Click the button 'Recalculate the Model' to recalculate the Shiny App."),
  #          actionButton("add_annotations", "Recalculate the Model"))
  # ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input end points interval
    # column(width=2, selectInput("interval", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol", choices=symbolv, selected=symbol)),
    # Input umber of histogram bins
    column(width=2, sliderInput("n_bins", "Number of bins:", min=1, max=50, value=100)),
    # Input short look-back interval
    column(width=2, sliderInput("short_back", label="Short lookback", min=3, max=300, value=150, step=1)),
    # Input long look-back interval
    # column(width=2, sliderInput("long_back", label="Long lookback", min=10, max=200, value=100, step=2)),
    # Input lag trade parameter
    # column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1)),
    # Input exponent of volume
    column(width=2, sliderInput("exponent", label="Exponent of Volume", min=0.25, max=2.0, value=1.0, step=0.1)),
    # column(width=2, sliderInput("threshold", label="threshold", min=0.5, max=3.0, value=0.9, step=0.1)),
    # Input add annotations Boolean
    # column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False"))
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
    # Input the bid-offer spread
    # column(width=2, numericInput("bid_offer", label="bid-offer:", value=0.001, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  plotOutput("histp", width="90%", height="550px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # recalculate the data and rerun the model
  # datav <- shiny::reactive({
  # Get model parameters from input argument
  # dimax <- isolate(input$dimax)
  # look_lag <- isolate(input$look_lag
  # lambda <- isolate(input$lambda)
  # typev <- isolate(input$typev)
  # alpha <- isolate(input$alpha)
  # quant <- isolate(input$quant)
  # coeff <- as.numeric(isolate(input$coeff))
  # bid_offer <- isolate(input$bid_offer)
  # Model is recalculated when the add_annotations variable is updated
  # input$add_annotations
  
  # Create an empty list of reactive values.
  # values <- reactiveValues()
  
  ## Calculate log returns
  retv <- shiny::reactive({
    symbol <- input$symbol
    cat("Loading data for ", symbol, "\n")
    ohlc <- rutils::etfenv[[symbol]]
    retv <- cbind(rutils::diffit(log(Cl(ohlc))),
                      Vo(ohlc))
    colnames(retv) <- c("returns", "volume")
    returns
  })  # end reactive
  
  ## Calculate scaled if there are new short_back and long_back values
  scaled <- shiny::reactive({
    cat("Calculating scaled returns in trading time \n")
    short_back <- input$short_back
    # long_back <- input$long_back
    
    # Calculate the rolling volume
    retv <- returns()$returns
    volumes <- (retv()$volume)
    # Scale the volume by the rolling volume
    volumes <- short_back*volumes/HighFreq::roll_sum(volumes, look_back=short_back)
    # retv <- rutils::diffit(closep())
    # Calculate the cumulative returns scaled by the rolling volume
    scaled <- ifelse(volumes > 0, returns/(volumes^input$exponent), 0)
    scaled[is.na(scaled) | is.infinite(scaled)] <- 0

    scaled <- cbind(retv, scaled)
    colnames(scaled) <- c("returns", "scaled")
    scaled
    
  })  # end reactive
  
  # Plot the histogram of the simulated data
  output$histp <- shiny::renderPlot({
    # isolate() prevents automatic recalculation when n_bins is updated
    symbol <- input$symbol
    cat("Plotting data for ", symbol, "\n")
    
    n_bins <- input$n_bins
    retv <- scaled()$returns
    scaled <- scaled()$scaled
    
    # Calculate kurtosis of the returns
    nrows <- NROW(retv)
    kurto_sis <- sum((retv/sd(retv))^4) nrows
    kurtosis_scaled <- sum((scaled/sd(scaled))^4) nrows
    pacfd <- pacf(retv, lag=10, plot=FALSE)
    pacfd <- sum(drop(pacfd$acf))
    pacf_scaled <- pacf(scaled, lag=10, plot=FALSE)
    pacf_scaled <- sum(drop(pacf_scaled$acf))
    
    # Calculate breaks based on input$bins from ui.R
    madv <- mad(retv)
    break_s <- seq(min(retv), max(retv), length.out=n_bins+1)
    # Calculate the kernel density using density()
    # b_w <- mad(rutils::diffit(retv, lagg=10))/10
    densityv <- density(retv, bw=madv/10)
    
    # Plot the histogram with the specified number of breaks
    captiont <- paste("Histogram of", symbol, "Returns Scaled by the Trading Volumes \n", 
                      "kurtosis=", round(kurto_sis, 2), "kurtosis scaled=", round(kurtosis_scaled, 2), "\n", 
                      "pacf=", round(pacfd, 2), "pacf scaled=", round(pacf_scaled, 2))
    hist(retv, breaks=break_s, main=captiont, 
         xlim=c(-5*madv, 5*madv), ylim=1.05*range(densityv$y),  
         xlab="returns", ylab="", 
         freq=FALSE, col="darkgray", border="white")
    # Draw kernel density of returns
    lines(densityv, col="blue", lwd=3)
    # Draw kernel density of scaled
    lines(density(scaled, bw=madv/10), col="red", lwd=3)
    # Add density of normal distribution
    curve(expr=dnorm(x, mean=mean(retv), sd=sd(retv)),
          add=TRUE, lwd=2, col="green")
    # Add legend
    legend("topright", inset=0.05, bty="n", cex=1.5, 
           leg=c("density", "scaled", "normal"),
           lwd=6, lty=1, col=c("blue", "red", "green"))
    
  })  # end renderPlot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
