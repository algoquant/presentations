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
library(parallel)  # Load package parallel

# Extract the percentage returns for VTI and IEF.
retsp <- rutils::etfenv$returns[, c("VTI", "IEF")]
retsp <- zoo::coredata(na.omit(retsp))
nrows <- NROW(retsp)

# Define the risk-adjusted wealth measure.

wealthfun <- function(wealthv)
  (mean(wealthv)-1)/sd(wealthv[wealthv < median(wealthv)])


## Bootstrap the retsp returns.
nboot <- 1e3
set.seed(1121)

### Write your code here


# End setup code
##############################


##############################
## Define the user interface

uiface <- shiny::fluidPage(
  titlePanel("Terminal Wealth Distribution of a VTI and IEF Portfolio"),
  
  # Create four slider inputs with parameters to lossdistr()
  fluidRow(
    column(width=2, sliderInput("weightvti", label="VTI weight:",
                                min=0.01, max=0.99, value=0.5, step=0.01)),
    column(width=2, sliderInput("holdp", label="Holding period (years):",
                                min=1.0, max=(nrows %/% 252), value=(nrows %/% 252)/2, step=0.1))
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
  datav <- shiny::reactive({
    
    # Extract model parameters from the argument "input"
    weightvti <- input$weightvti
    holdp <- 252*input$holdp

    # Use these weights:
    weightv <- c(weightvti, 1-weightvti)
    
    # Calculate the wealth distribution from boot_data
    
    ### Write your code here
    
    wealthv
    
  })  # end reactive code
  
  ## Create plot and return it to the output argument
  output$plot_portf <- shiny::renderPlot({

    # Copy the wealth data
    wealthv <- datav()

    # Calculate the wealth parameters
    meanv <- mean(wealthv)
    medianv <- median(wealthv)
    sdv <- sd(wealthv)
    skewv <- (meanv-medianv)/sdv
    
    # Calculate the risk-adjusted wealth measure
    ### Write your code here
    
    # Calculate the density of wealth distribution
    ### Write your code here
    
    # Plot density of portfolio wealth
    par(mar=c(5.1, 5.1, 4.1, 2.1))
    plot(densityv, col="blue", lwd=3, 
         xlab="Wealth", ylab="Density",
         cex.main=1.5, cex.lab=1.5, cex.axis=1.5, 
         main="Density of the Terminal Wealths of VTI and IEF Portfolio")

    text(x=0.75*max(densityv$x), y=0.75*max(densityv$y), 
         labels=paste0("Mean of wealth = ", format(meanv, digits=3), "\n",
                       "Standard deviation of wealth = ", format(sdv, digits=3), "\n",
                       "Skewness of wealth = ", format(skewv, digits=3), "\n",
                       "Risk-adjusted wealth = ", format(wealthm, digits=3)),
         adj=c(1, 1), lwd=2, cex=1.5)
    
  })  # end output plot

}  # end server function


##############################
## Return a Shiny app object

shiny::shinyApp(ui=uiface, server=servfun)
