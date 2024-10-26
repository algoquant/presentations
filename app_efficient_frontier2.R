##############################
# This is a shiny app for the efficient frontier 
# of a portfolio of two stocks.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(shiny)

## Model and data setup

captiont <- "Efficient Frontier of Two Stocks"
widthp <- 6
heightp <- 5
## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  # Create single row with two slider inputs
  fluidRow(
    # Risk free rate
    column(width=2, sliderInput("raterf", label="Risk-free", min=0.0, max=0.1, value=0.03, step=0.001)),
    # Stock returns
    column(width=2, sliderInput("ret1", label="Return1", min=0.0, max=0.1, value=0.07, step=0.01)),
    column(width=2, sliderInput("ret2", label="Return2", min=0.0, max=0.1, value=0.1, step=0.01)), 
    style="width:1500px;height:100px;font-size:13px"
  ),  # end fluidRow
  
  fluidRow(
    # Correlation
    column(width=2, sliderInput("corrp", label="Correlation", min=-0.99, max=0.99, value=0.3, step=0.01)), 
    # Standard deviations
    column(width=2, sliderInput("std1", label="StDev1", min=0.0, max=0.5, value=0.1, step=0.01)),
    column(width=2, sliderInput("std2", label="StDev2", min=0.0, max=0.5, value=0.2, step=0.01)),
    style="width:1500px;height:100px;font-size:13px"
  ),  # end fluidRow
  
  # Render plot in panel
  shiny::plotOutput("ploteffront", width="70%", height=500)
  
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # Return to the output argument a dygraph plot with two y-axes
  output$ploteffront <- shiny::renderPlot({
    
    cat("Calculating the efficient frontier\n")
    # Get model parameters from input argument
    raterf <- input$raterf
    retp <- c(stock1=input$ret1, stock2=input$ret2)
    stdevs <- c(stock1=input$std1, stock2=input$std2)
    corrp <- input$corrp
    
    # Calculate the efficient frontier
    covmat <- matrix(c(1, corrp, corrp, 1), nc=2) # Covariance matrix
    covmat <- t(t(stdevs*covmat)*stdevs)
    weightv <- seq(from=(-1), to=2, length.out=71) # Weights
    weightv <- cbind(weightv, 1-weightv)
    retport <- weightv %*% retp # Portfolio returns
    portfsd <- sqrt(rowSums(weightv*(weightv %*% covmat))) # Portfolio volatility
    sharper <- (retport-raterf)/portfsd # Portfolio Sharpe ratios
    
    # Plot the efficient frontier
    # x11(widthp <- 6, heightp <- 5)
    # dev.new(widthp <- 6, heightp <- 5, noRStudioGD=TRUE)
    plot(portfsd, retport, t="l",
         main=paste0("Efficient Frontier and CML for Two Stocks\ncorrelation = ", 100*corrp, "%"),
         xlab="standard deviation", ylab="return",
         lwd=2, col="orange",
         xlim=c(0, max(portfsd)),
         ylim=c(0.02, max(retport)))
    # Add the maximum Sharpe ratio portfolio
    whichmax <- which.max(sharper)
    sharpem <- max(sharper) # Maximum Sharpe ratio
    retmax <- retport[whichmax]
    sdmax <- portfsd[whichmax]
    weightm <- round(weightv[whichmax], 2)
    points(sdmax, retmax, col="blue", lwd=3)
    text(x=sdmax, y=retmax,
      labels=paste(c("Max Sharpe\n",
        structure(c(weightm, (1-weightm)), names=c("stock1", "stock2"))), collapse=" "),
         pos=2, cex=0.8)
    
    # Plot individual stocks
    points(stdevs, retp, col="green", lwd=3)
    text(stdevs, retp, labels=names(retp), pos=4, cex=0.8)
    # Add point at risk-free rate and draw Capital Market Line
    points(x=0, y=raterf, col="blue", lwd=3)
    text(0, raterf, labels="risk-free\nrate", pos=4, cex=0.8)
    abline(a=raterf, b=sharpem, lwd=2, col="blue")
    rangev <- par("usr")
    text(sdmax/2, (retmax+raterf)/2,
         labels="Capital Market Line", cex=0.8, , pos=3,
         srt=45*atan(sharpem*(rangev[2]-rangev[1])/
                       (rangev[4]-rangev[3])*heightp/widthp)/(0.25*pi))
    
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
