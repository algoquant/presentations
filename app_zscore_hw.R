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
    
    # Write your code here
    
    # Input lambda decay parameter
    
    # Write your code here
    
    # Render the plot
    
    # Write your code here
    
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
    
    # Calculate the z-scores
    # Use the function HighFreq::run_var() to calculate the
    # EMA variance.
    
    # Write your code here
    
    
  })  # end z-scores
  
  
  # Plot the data
  output$plotobj <- shiny::renderPlot({
    cat("Plotting the z-scores\n")

    # Write your code here

  })  # end renderPlot
  
}  # end servfun


## Return a Shiny app object

shiny::shinyApp(ui=uifun, server=servfun)
