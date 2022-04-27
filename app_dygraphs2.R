##############################
# This is an example of creating a dynamic shiny app
# which produces an interactive dygraphs plot.
# Just press the "Run App" button on upper right of this panel.
##############################

library(shiny)
library(dygraphs)
library(rutils)


# Define elements of the UI user interface
uiface <- shiny::shinyUI(fluidPage(
  
  titlePanel("VTI prices"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambda", label="lambda:",
                  min=0.001, max=0.1, value=0.02, step=0.01),
      numericInput("widthp", label="widthp:", min=51, max=301, value=151)
    ),
    mainPanel(
      dygraphs::dygraphOutput("dygraph")
    )
  )
))  # end shinyUI interface


# Define the server code
servfun <- function(input, output) {

  # source the model function
  source("/Users/jerzy/Develop/lecture_slides/scripts/ewma_model.R")
  
  # Calculate the data for plotting
  datav <- shiny::reactive({
    # get model parameters from input
    lambda <- input$lambda
    widthp <- input$widthp
    # calculate close prices
    closep <- quantmod::Cl(rutils::etfenv$VTI["2007/2010"])
    # calculate EWMA prices
    weights <- exp(-lambda*(1:widthp))
    weights <- weights/sum(weights)
    ew_ma <- .Call(stats:::C_cfilter, closep, filter=weights, sides=1, circular=FALSE)
    # ew_ma <- filter(closep, filter=weights, sides=1)
    ew_ma[1:(widthp-1)] <- ew_ma[widthp]
    ew_ma <- xts(cbind(closep, ew_ma), order.by=index(closep))
    colnames(ew_ma) <- c("VTI", "VTI_EWMA")
    ew_ma  # return data for plotting
  })  # end reactive data
  
  # Define the output plot
  output$dygraph <- dygraphs::renderDygraph({
    dygraph(datav(), main="VTI prices") %>%
      dySeries("VTI", name="VTI", strokeWidth=1.5, color="blue") %>%
      dySeries("VTI_EWMA", name="VTI_EWMA", strokeWidth=1.5, color="red")
  })  # end output plot
  
}  # end server code

# Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
