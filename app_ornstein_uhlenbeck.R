##############################
# This is a shiny app for the Ornstein-Uhlenbeck process.
# To run it, just press the "Run App" button on upper right of this panel.
##############################

##############################
# Below is the setup code that runs only once at startup 
# when the shiny app is started.
# In the setup code you can load packages, define functions 
# and variables, source files, and load data.

library(rutils)
# Simulate the random innovations for the Ornstein-Uhlenbeck process
nrows <- 1000
innov <- rnorm(nrows)


# End setup code
##############################


##############################
## Define the user interface

## Create elements of the user interface
uifun <- shiny::fluidPage(
  
  titlePanel("Ornstein-Uhlenbeck Process"),
  
  # Create single row with inputs
  fluidRow(
    
    # Get model parameters
    column(width=2, sliderInput("volatp", label="Volatility:", min=0.001, max=0.05, value=0.01, step=0.001)),
    column(width=2, sliderInput("priceq", label="Equlibrium price:", min=1.0, max=10.0, value=5.0, step=0.1)),
    column(width=2, sliderInput("thetav", label="Theta parameter:", min=0.0001, max=0.05, value=0.005, step=0.0001)),
    
  ),  # end fluidRow
  
  # Render the plot in a new row
  fluidRow(
    column(width=11, plotOutput("plotobj", height=600, width=900))
  ),  # end fluidRow
  
)  # end user interface



##############################
## Define the server function, with the arguments "input" and "output".
# The server function performs the calculations and creates the plots.

servfun <- function(input, output) {
  
  pricev <- shiny::reactive({
    cat("Simulating Ornstein-Uhlenbeck process", "\n")
    # cat("nrows = ", nrows, "\n")
    
    # Get the Ornstein-Uhlenbeck parameters from input argument
    volatp <- input$volatp
    priceq <- input$priceq
    thetav <- input$thetav
    
    # Simulate the Ornstein-Uhlenbeck process
    retv <- numeric(nrows)
    pricev <- numeric(nrows)
    pricev[1] <- 0.5*priceq
    for (i in 2:nrows) {
      retv[i] <- thetav*(priceq - pricev[i-1]) + volatp*innov[i]
      pricev[i] <- pricev[i-1] * exp(retv[i])
    }  # end for
    
    return(pricev)
    
  })  # end pricev
  
  
  # Plot the data
  output$plotobj <- shiny::renderPlot({

    
    # Get the Ornstein-Uhlenbeck parameters from input argument
    volatp <- input$volatp
    priceq <- input$priceq
    thetav <- input$thetav
    # Get the Ornstein-Uhlenbeck process
    pricev <- pricev()
    # Plot
    plot(pricev, type="l", cex.lab=1.7, cex.axis=1.7, cex.main=1.7, lwd=2, col="blue",
         xlab="time", ylab="price", 
         main="Ornstein-Uhlenbeck process")
    abline(h=priceq, col="red", lty=2, lwd=3)
    legend("bottomright", 
           title=paste(c(paste0("Volatility = ", volatp), 
                         paste0("Equlibrium price = ", priceq),
                         paste0("Theta parameter = ", thetav)),
                       collapse="\n"),
           legend="", cex=1.5, 
           inset=0.01, bg="white", bty="n")
    
  })  # end renderPlot
  
}  # end servfun


## Return a Shiny app object

shiny::shinyApp(ui=uifun, server=servfun)
