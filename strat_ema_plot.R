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
  
  # titlePanel("EMA Prices"),
  
  # Create single row of widgets
  h3(fluidRow(
    # Input look-back interval
    column(width=12, sliderInput("lambda", label="Lambda decay factor",
                                min=0.5, max=0.99, value=0.9, step=0.01))
  )),  # end fluidRow
  

  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="700px", height="400px")
  
))  # end shinyUI interface


# Define the server code
servfun <- function(input, output) {

  # Calculate the data for plotting
  datav <- shiny::reactive({
    # get model parameters from input
    lambda <- input$lambda
    # Calculate close prices
    pricev <- log(quantmod::Cl(rutils::etfenv$VTI["2008/2009"]))
    
    # Calculate EMA prices using filter
    # pricema <- filter(pricev, filter=weights, sides=1)
    # weights <- exp(-lambda*(1:widthp))
    # weights <- weights/sum(weights)
    # pricema <- .Call(stats:::C_cfilter, pricev, filter=weights, sides=1, circular=FALSE)
    # pricema[1:(widthp-1)] <- pricema[widthp]
    
    # Calculate EMA prices recursively
    pricema <- HighFreq::run_mean(pricev, lambda=lambda)
    pricema <- cbind(pricev, pricema)
    colnames(pricema) <- c("VTI", "EMA")
    pricema  # Return data for plotting
  })  # end reactive data
  
  # Define the output plot
  output$dyplot <- dygraphs::renderDygraph({
    dygraph(datav(), main="EMA Prices") %>%
      dySeries(name="VTI", label="VTI", strokeWidth=2, color="blue") %>%
      dySeries(name="EMA", label="EMA", strokeWidth=2, color="red")
  })  # end output plot
  
}  # end server code

# Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
