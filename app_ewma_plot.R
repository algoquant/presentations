##############################
# This is an example of creating a dynamic shiny app
# which produces an interactive dygraphs plot.
# Just press the "Run App" button on upper right of this panel.
##############################

library(shiny)
library(dygraphs)
library(rutils)


# Define elements of the UI user interface
uifun <- shiny::shinyUI(fluidPage(
  
  titlePanel("VTI EWMA Prices"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambda", label="Decay parameter:",
                  min=0.01, max=0.99, value=0.9, step=0.01),
      numericInput("widthp", label="Filter width:", min=51, max=301, value=151)
    ),
    mainPanel(
      dygraphs::dygraphOutput("dygraph", width="90%", height="600px")
    )
  )
))  # end shinyUI interface


# Define the server code
servfun <- function(input, output) {

  # Calculate the data for plotting
  datav <- shiny::reactive({
    # get model parameters from input
    lambda <- input$lambda
    widthp <- input$widthp
    # Calculate close prices
    pricev <- log(quantmod::Cl(rutils::etfenv$VTI["2008/2009"]))
    
    # Calculate EWMA prices using filter
    # ewmap <- filter(pricev, filter=weights, sides=1)
    # weights <- exp(-lambda*(1:widthp))
    # weights <- weights/sum(weights)
    # ewmap <- .Call(stats:::C_cfilter, pricev, filter=weights, sides=1, circular=FALSE)
    # ewmap[1:(widthp-1)] <- ewmap[widthp]
    
    # Calculate EWMA prices recursively
    ewmap <- HighFreq::run_mean(pricev, lambda=lambda)
    ewmap <- cbind(pricev, ewmap)
    colnames(ewmap) <- c("VTI", "EWMA")
    ewmap  # Return data for plotting
  })  # end reactive data
  
  # Define the output plot
  output$dygraph <- dygraphs::renderDygraph({
    dygraph(datav(), main="VTI EWMA Prices") %>%
      dySeries(name="VTI", label="VTI", strokeWidth=2, color="blue") %>%
      dySeries(name="EWMA", label="EWMA", strokeWidth=2, color="red")
  })  # end output plot
  
}  # end server code

# Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
