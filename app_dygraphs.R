library(shiny)
library(dygraphs)
library(datasets)


# Define the UI
ui <- shiny::shinyUI(fluidPage(
  
  titlePanel("Predicted Deaths from Lung Disease (UK)"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("months", label = "Months to Predict", 
                   value = 72, min = 12, max = 144, step = 12),
      selectInput("interval", label = "Prediction Interval",
                  choices = c("0.80", "0.90", "0.95", "0.99"),
                  selected = "0.95"),
      checkboxInput("showgrid", label = "Show Grid", value = TRUE)
    ),
    mainPanel(
      dygraphs::dygraphOutput("dygraph")
    )
  )
))


# Define the server code
server <- shiny::shinyServer(function(input, output) {
  
  predicted <- shiny::reactive({
    hw <- HoltWinters(ldeaths)
    predict(hw, n.ahead = input$months, 
            prediction.interval = TRUE,
            level = as.numeric(input$interval))
  })
  
  output$dygraph <- dygraphs::renderDygraph({
    dygraph(predicted(), main = "Predicted Deaths/Month") %>%
      dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
      dyOptions(drawGrid = input$showgrid)
  })
  
})

# Return a Shiny app object
shiny::shinyApp(ui = ui, server = server)
