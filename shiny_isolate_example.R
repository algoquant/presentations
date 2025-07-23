#
# This is a Shiny web application. 
# You can run the application by clicking the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/
#
# Find out more about controlling Recalculation using isolate() here:
# https://shiny.rstudio.com/articles/isolation.html


## Below is the setup code that runs once when the shiny app is started

library(shiny)

# Simulate random data
datav <- rnorm(1e3)

## End setup code


# Define user interface for application that draws a histogram
uifun <- fluidPage(

    # Application title
    titlePanel("Example of Controlling Recalculation Using isolate()"),
    h4("The Shiny App simulates random data and plots a histogram."),
    h4("The number of histogram breaks can be specified by the user."),
    h4("The Shiny App Recalculates only after the user clicks the button 'Recalculate plot'."),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("nbins",
                        "Number of bins:",
                        min=1,
                        max=50,
                        value=30),
            # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
            h4("Click the button 'Recalculate plot' to Recalculate the Shiny App."),
            actionButton("recalcb", "Recalculate plot")
        ),

        # Show a plot of the generated distribution
        plotOutput("histp")
        
    )  # end sidebarLayout
    
)  # end fluidPage

# Define server code required to draw a histogram
servfun <- function(input, output) {

    # Plot the histogram of the simulated data
    output$histp <- shiny::renderPlot({
        # isolate() prevents automatic Recalculation when nbins is updated
        nbins <- isolate(input$nbins)
        # Model is recalculated when the recalcb variable is updated
        input$recalcb
        # Calculate breaks based on input$bins from ui.R
        break_s <- seq(min(datav), max(datav), length.out=nbins+1)

        # Plot the histogram with the specified number of breaks
        hist(datav, breaks=break_s, col="darkgray", border="white",
             main="Histogram of random data")
    })  # end renderPlot
    
}  # end servfun

# Run the Shiny application 
shinyApp(ui=uifun, server=servfun)

