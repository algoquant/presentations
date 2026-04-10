##############################
# This is a shiny app with yield curve under the Vasicek model.
# To run it, just press the "Run App" button on upper right of this panel.
##############################

##############################
# Below is the setup code that runs only once at startup 
# when the shiny app is started.
# In the setup code you can load packages, define functions 
# and variables, source files, and load data.

library(rutils)

# Maturities in years
tauv <- seq(1, 30, by=1)

# End setup code
##############################


##############################
## Define the user interface

## Create elements of the user interface
uifun <- shiny::fluidPage(
  
  titlePanel("Yield Curve Under the Vasicek Model"),
  
  # Create single row with inputs
  fluidRow(
    
    # Get Vasicek model parameters
    column(width=2, sliderInput("ratet", "Short rate:", min=0.1, max=6.0, value=2.0, step=0.1)),
    column(width=2, sliderInput("muv", label="Equilibrium rate:", min=0.1, max=7.0, value=4.0, step=0.1)),
    column(width=2, sliderInput("thetav", label="Reversion strength:", min=0.01, max=1.0, value=0.1, step=0.01)),
    column(width=2, sliderInput("sigmav", label="Volatility:", min=0.01, max=0.2, value=0.05, step=0.01)),

    # Render the plot
    plotOutput("plotobj", height=500, width=700),
    
  ),  # end fluidRow
  
)  # end user interface



##############################
## Define the server function, with the arguments "input" and "output".
# The server function performs the calculations and creates the plots.

servfun <- function(input, output) {
  
  # Calculate the yield curve
  ycurve <- shiny::reactive({
    cat("Calculating the yield curve\n")
    # Get model parameters from input argument
    ratet <- input$ratet
    thetav <- input$thetav
    sigmav <- input$sigmav
    muv <- input$muv

    # Calculate the yield curve under the Vasicek model
    B <- (1 - exp(-thetav*tauv))/thetav
    A <- (muv - sigmav^2/(2*thetav^2))
    A <- A*(B - tauv) - (sigmav^2*B^2)/(4*thetav)
    ycurve <- (-A + B*ratet)/tauv
    return(ycurve)
    
  })  # end yield curve
  
  
  # Plot the data
  output$plotobj <- shiny::renderPlot({
    cat("Plotting the yield curve\n")
    
    # Get the yield curve
    ycurve <- ycurve()
    # Number of bins for the histogram
    nbins <- input$nbins
    # Plot the yield curve
    plot(tauv, ycurve, type="l", lwd=3, col="blue",
         main="Yield Curve Under the Vasicek Model",
         xlab="Maturity (years)", ylab="Yield (%)")
    
  })  # end renderPlot
  
}  # end servfun


## Return a Shiny app object

shiny::shinyApp(ui=uifun, server=servfun)
