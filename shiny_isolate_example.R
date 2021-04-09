#
# This is a Shiny web application. 
# You can run the application by clicking the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/
#
# Find out more about controlling re-calculation using isolate() here:
# https://shiny.rstudio.com/articles/isolation.html


## Below is the setup code that runs once when the shiny app is started

library(shiny)

# Simulate random data
da_ta <- rnorm(1e3)

## End setup code


# Define user interface for application that draws a histogram
inter_face <- fluidPage(

    # Application title
    titlePanel("Example of Controlling Re-calculation Using isolate()"),
    h4("The Shiny App simulates random data and plots a histogram."),
    h4("The number of histogram breaks can be specified by the user."),
    h4("The Shiny App re-calculates only after the user clicks the button 'Recalculate plot'."),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n_bins",
                        "Number of bins:",
                        min=1,
                        max=50,
                        value=30),
            # The Shiny App is re-calculated when the actionButton is clicked and the re_calculate variable is updated
            h4("Click the button 'Recalculate plot' to re-calculate the Shiny App."),
            actionButton("re_calculate", "Recalculate plot")
        ),

        # Show a plot of the generated distribution
        mainPanel(plotOutput("histo_gram"))
        
    )  # end sidebarLayout
    
)  # end fluidPage

# Define server code required to draw a histogram
serv_er <- function(input, output) {

    # Plot the histogram of the simulated data
    output$histo_gram <- shiny::renderPlot({
        # isolate() prevents automatic re-calculation when n_bins is updated
        n_bins <- isolate(input$n_bins)
        # Model is re-calculated when the re_calculate variable is updated
        input$re_calculate
        # Calculate breaks based on input$bins from ui.R
        break_s <- seq(min(da_ta), max(da_ta), length.out=n_bins+1)

        # Plot the histogram with the specified number of breaks
        hist(da_ta, breaks=break_s, col="darkgray", border="white",
             main="Histogram of random data")
    })  # end renderPlot
    
}  # end serv_er

# Run the Shiny application 
shinyApp(ui=inter_face, server=serv_er)

