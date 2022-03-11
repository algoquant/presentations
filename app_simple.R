##############################
# This is a simple shiny app.
# To run it, just press the "Run App" button on upper right of this panel.
##############################

##############################
# Below is the setup code that runs only once at startup 
# when the shiny app is started.
# In the setup code you can load packages, define functions 
# and variables, source files, and load data.

ndata <- 1e4
stdev <- 1.0

# End setup code
##############################


##############################
## Define the user interface

uiface <- shiny::fluidPage(
  
  titlePanel("A Simple Shiny App"),

  ## Create interface for the input parameters.
  
  # Create numeric input for the number of data points.
  numericInput('ndata', "Number of data points:", value=ndata),
  
  # Create slider input for the standard deviation parameter.
  sliderInput("stdev", label="Standard deviation:",
              min=0.1, max=3.0, value=stdev, step=0.1),
  
  ## Produce output.
  # Render plot in a panel.
  plotOutput("plotobj", height=300, width=500)
  
)  # end user interface



##############################
## Define the server function, with the arguments "input" and "output".
# The server function performs the calculations and creates the plots.

servfunc <- function(input, output) {
  
  ## Recalculate the model with new parameters
  # The function reactive() accepts a block of expressions
  # which calculate the model, and returns the model output.
  datav <- reactive({
    cat("Calculating the data\n")
    
    # Simulate the data
    rnorm(input$ndata, sd=1)
    
  })  # end reactive code
  
  
  # Plot the data
  output$plotobj <- shiny::renderPlot({
    cat("Plotting the data\n")
    
    datav <- input$stdev*datav()
    
    # Plot the data
    par(mar=c(2, 4, 4, 0), oma=c(0, 0, 0, 0))
    hist(datav, xlim=c(-4, 4), main="Histogram of Random Data")
    
  })  # end renderPlot
  
}  # end servfunc


## Return a Shiny app object

shiny::shinyApp(ui=uiface, server=servfunc)
