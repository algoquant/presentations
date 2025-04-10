##############################
# This is a shiny app for simulating the wealth distributions 
# of VTI and IEF portfolios, for different holding periods.
# 
# Just press the "Run App" button on upper right of this panel.
##############################


##############################
# Below is the setup code that runs only once at startup 
# when the shiny app is started.
# In the setup code you can load packages, define functions 
# and variables, source files, and load data.

# Load packages here (if needed)
library(rutils)
# Coerce the log prices from xts time series to matrix 
pricev <- na.omit(rutils::etfenv$prices[, c("VTI", "IEF")])
pricev <- log(zoo::coredata(pricev))
nrows <- NROW(pricev)

# Define the risk-adjusted wealth measure
riskretfun <- function(wealthv) {
  mean(wealthv)/sd(wealthv)
}  # end riskretfun

# Set the number of bootstrap samples
nboot <- 1e3

# End setup code
##############################


##############################
## Define the user interface

uifun <- shiny::fluidPage(
  titlePanel("Terminal Wealth Distribution of a VTI and IEF Portfolio"),
  
  # Create four slider inputs with parameters to lossdistr()
  fluidRow(
    column(width=2, sliderInput("weightv", label="VTI weight:",
                                min=0.01, max=0.99, value=0.5, step=0.01)),
    column(width=2, sliderInput("holdp", label="Holding period (years):",
                                min=1.0, max=(nrows %/% 252)/2, value=(nrows %/% 252)/4, step=0.1))
  ),  # end fluidRow
  
  # Render plot in panel
  shiny::plotOutput("plot_portf", width="100%", height=650)
)  # end fluidPage interface


##############################
## Define the server code
# The function servfun() accepts the arguments "input" and "output".

servfun <- function(input, output) {

  ## Recalculate the model with new parameters
  # The function shiny::reactive() accepts a block of expressions
  # which calculate the model, and returns the model output.
  wealthv <- shiny::reactive({
    
    # Extract model parameters from the argument "input"
    weightv <- input$weightv
    holdp <- 252*input$holdp

    # Sample the start dates for the bootstrap
    set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
    startd <- sample.int(nrows-holdp, nboot, replace=TRUE)
    # Calculate a vector of portfolio wealths from startd and weightv
    wealthv <- sapply(startd, function(x) {
      retm <- pricev[x+holdp-1, ] - pricev[x, ]
      drop(c(weightv, 1-weightv) %*% retm)
    })  # end sapply
    wealthv
    
  })  # end reactive code
  
  ## Create plot and return it to the output argument
  output$plot_portf <- shiny::renderPlot({

    # Copy the wealth data
    wealthv <- wealthv()

    # Calculate the wealth parameters
    meanv <- mean(wealthv)
    stdev <- sd(wealthv)
    skewv <- mean(((wealthv - meanv)/stdev)^3)
    
    # Calculate the risk-adjusted wealth measure
    riskret <- riskretfun(wealthv)
    
    # Calculate the density of wealth distribution
    densityv <- density(wealthv, from=0)

    # Plot the density of portfolio wealth
    densx <- densityv$x
    densy <- densityv$y
    plot(densityv, col="blue", lwd=3, xlim=c(0.1*min(densx), 0.9*max(densx)), 
         xlab="Wealth", ylab="Density",
         cex.main=1.5, cex.lab=1.5, cex.axis=1.5, 
         main="Density of the Terminal Wealths of VTI and IEF Portfolio")
    abline(v=meanv, lty="dashed", lwd=3, col="blue")
    
    text(x=0.85*max(densx), y=0.85*max(densy), 
         labels=paste0("Mean = ", format(meanv, digits=3), "\n",
                       "Standard deviation = ", format(stdev, digits=3), "\n",
                       "Skewness = ", format(skewv, digits=3), "\n",
                       "Risk-adjusted wealth = ", format(riskret, digits=3)),
         adj=c(1, 1), lwd=2, cex=1.5)
    
  })  # end output plot

}  # end server function


##############################
## Return a Shiny app object

shiny::shinyApp(ui=uifun, server=servfun)
