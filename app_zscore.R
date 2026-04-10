##############################
# This is a shiny app with a histogram of the price z-scores.
# To run it, just press the "Run App" button on upper right of this panel.
##############################

##############################
# Below is the setup code that runs only once at startup 
# when the shiny app is started.
# In the setup code you can load packages, define functions 
# and variables, source files, and load data.

library(rutils)

# Load the SPY prices
symboln <- "SPY"
pricev <- log(na.omit(get(symboln, rutils::etfenv$prices)))

# End setup code
##############################


##############################
## Define the user interface

## Create elements of the user interface
uifun <- shiny::fluidPage(
  
  titlePanel("Histogram of Price Z-scores"),
  
  # Create single row with inputs
  fluidRow(
    
    # Create number of bins for the histogram
    column(width=3, sliderInput("nbins", "Number of bins:", min=10, max=200, value=100, step=1)),
    # Input lambda decay parameter
    column(width=3, sliderInput("lambdaf", label="lambda:", min=0.1, max=0.99, value=0.9, step=0.01)),
    
    # Render the plot
    plotOutput("plotobj", height=500, width=700),
    
  ),  # end fluidRow
  
)  # end user interface



##############################
## Define the server function, with the arguments "input" and "output".
# The server function performs the calculations and creates the plots.

servfun <- function(input, output) {
  
  # Calculate the z-scores
  zscores <- shiny::reactive({
    cat("Calculating the z-scores\n")
    # Get model parameters from input argument
    lambdaf <- input$lambdaf
    
    volp <- HighFreq::run_var(pricev, lambdaf=lambdaf)
    pricema <- volp[, 1]
    volp <- sqrt(volp[, 2])
    # Calculate the z-scores
    zscores <- (pricev - pricema)/volp
    return(zscores)
    
  })  # end z-scores
  
  
  # Plot the data
  output$plotobj <- shiny::renderPlot({
    cat("Plotting the z-scores\n")
    
    # Get the z-scores
    zscores <- zscores()
    # Number of bins for the histogram
    nbins <- input$nbins
    # Calculate breaks based on input$bins from ui.R
    breakv <- seq(min(zscores), max(zscores), length.out=nbins+1)
    
    # Plot the data
    # par(mar=c(2, 4, 4, 0), oma=c(0, 0, 0, 0))
    hist(zscores, main="Histogram of Z-scores", 
         breaks=breakv, col="darkgray", border="white")
    
  })  # end renderPlot
  
}  # end servfun


## Return a Shiny app object

shiny::shinyApp(ui=uifun, server=servfun)
