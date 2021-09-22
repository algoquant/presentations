##############################
# This is an example of creating a dynamic shiny app
# which produces an interactive dygraphs plot.
# Just press the "Run App" button on upper right of this panel.
##############################

library(shiny)
library(dygraphs)
library(rutils)


# Define elements of the UI user interface
inter_face <- shiny::shinyUI(fluidPage(
  
  titlePanel("VTI prices"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("lamb_da", label="lambda:",
                  min=0.001, max=0.1, value=0.02, step=0.01),
      numericInput("wid_th", label="wid_th:", min=51, max=301, value=151)
    ),
    mainPanel(
      dygraphs::dygraphOutput("dygraph")
    )
  )
))  # end shinyUI interface


# Define the server code
ser_ver <- function(input, output) {

  # source the model function
  source("C:/Develop/lecture_slides/scripts/ewma_model.R")
  
  # Calculate the data for plotting
  da_ta <- reactive({
    # get model parameters from input
    lamb_da <- input$lamb_da
    wid_th <- input$wid_th
    # calculate close prices
    clos_e <- quantmod::Cl(rutils::etf_env$VTI["2007/2010"])
    # calculate EWMA prices
    weight_s <- exp(-lamb_da*(1:wid_th))
    weight_s <- weight_s/sum(weight_s)
    ew_ma <- .Call(stats:::C_cfilter, clos_e, filter=weight_s, sides=1, circular=FALSE)
    # ew_ma <- filter(clos_e, filter=weight_s, sides=1)
    ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
    ew_ma <- xts(cbind(clos_e, ew_ma), order.by=index(clos_e))
    colnames(ew_ma) <- c("VTI", "VTI_EWMA")
    ew_ma  # return data for plotting
  })  # end reactive data
  
  # Define the output plot
  output$dygraph <- dygraphs::renderDygraph({
    dygraph(da_ta(), main="VTI prices") %>%
      dySeries("VTI", name="VTI", strokeWidth=1.5, color="blue") %>%
      dySeries("VTI_EWMA", name="VTI_EWMA", strokeWidth=1.5, color="red")
  })  # end output plot
  
}  # end server code

# Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
