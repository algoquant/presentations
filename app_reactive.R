##############################
# This is a shiny app for testing reactive code.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

library(shiny)
library(dygraphs)
# library(datasets)


# References about reactive code.
# https://community.rstudio.com/t/reactivevalues-vs-reactive-and-eventreactive-a-general-question/27449/5
# https://riptutorial.com/shiny/example/32343/observeevent
# https://riptutorial.com/shiny/example/32341/eventreactive
# https://stackoverflow.com/questions/39170161/r-shiny-eventreactive-with-two-controls
# https://stackoverflow.com/questions/33662033/shiny-how-to-make-reactive-value-initialize-with-default-value
# https://stackoverflow.com/questions/33519816/shiny-what-is-the-difference-between-observeevent-and-eventreactive
# https://stackoverflow.com/questions/37710358/how-to-create-if-statement-with-reactive-values-in-r-shiny
# https://stackoverflow.com/questions/39436713/r-shiny-reactivevalues-vs-reactive


## Define the User Interface
uiface <- shiny::fluidPage(
  titlePanel("View mtcars Data Frame"),
  fluidRow(
    # Input stock symbol
    column(width=4, numericInput("nrows", "Enter number of rows of data: ", value=5)),
    column(width=4, actionButton("button", "Press to Show Data"))  # end column
  ),  # end fluidRow
  
  column(8, tableOutput("tablev"))
  
)  # end fluidPage interface


## Define the server function
servfun <- function(input, output) {
  
  ##############################
  # The function reactiveValues() creates a list for storing 
  # reactive values, which can be updated by event handlers. 
  # The list elements have dependencies with reactive expressions
  # where they are called.
  
  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  
  ##############################
  # The function shiny::reactive() transforms an expression into a 
  # reactive expression.  Reactive expressions are evaluated 
  # only when their input data is updated.  This avoids 
  # performing unnecessary calculations.
  # The function shiny::reactive() usually returns a value.
  # If the reactive expression is invalidated (recalculated), 
  # then other expressions that depend on its output are also 
  # recalculated. 
  # This way calculations cascade through the expressions that 
  # depend on each other.
  
  # Get input parameters from the user interface.
  nrows <- shiny::reactive({
    # Add nrows to list of reactive values.
    values$nrows <- input$nrows
    input$nrows
  })  # end reactive code
  
  
  
  ##############################
  # The functions observeEvent() and eventReactive() are event 
  # handlers.
  # They evaluate their expressions when their event expression 
  # eventExpr is invalidated (updated). 
  # eventReactive() returns a value, while observeEvent() 
  # produces a side-effect, but doesn't return a value.
  # observeEvent() and eventReactive() are wrapper functions
  # for the functions observe() and isolate().
  
  ##############################
  # The difference between shiny::reactive() and eventReactive() is that 
  # shiny::reactive() recalculates its expression only when its inputs 
  # are updated.  But eventReactive() recalculates its handler 
  # expression only when the event expression eventExpr is 
  # invalidated (updated). 
  
  ##############################
  # The function observeEvent() is an event handler.
  # It evaluates the handler expression handlerExpr when the 
  # event expression eventExpr is invalidated (updated). 
  # For example, the event can be a button press.
  # observeEvent() produces a side-effect but doesn't 
  # return a value.
  
  # Broadcast a message to the console when the button is pressed.
  observeEvent(eventExpr=input$button, handlerExpr={
    cat("Input button pressed\n")
  })  # end observeEvent
  
  
  ##############################
  # The function eventReactive() is an event handler.
  # It evaluates the handler expression valueExpr when the 
  # event expression eventExpr is invalidated (updated). 
  # For example, the event can be a button press.
  # eventReactive() returns a value.
  # eventReactive() doesn't react to other events inside 
  # the function.
  
  # Send the data when the button is pressed.
  datav <- eventReactive(eventExpr=input$button, valueExpr={
    # eventReactive() executes on input$button, but
    # not on nrows() or input$nrows.
    cat("Sending", nrows(), "rows of data\n")
    datav <- head(mtcars, input$nrows)
    values$mpg <- mean(datav$mpg)
    datav
  })  # end eventReactive
  #   datav
  
  
  # Draw table of the data when the button is pressed.
  observeEvent(eventExpr=input$button, handlerExpr={
    datav <- datav()
    cat("Received", values$nrows, "rows of data\n")
    cat("Average mpg = ", values$mpg, "\n")
    cat("Drawing table\n")
    output$tablev <- renderTable(datav) 
  })  # end observeEvent
  
}  # end server code


## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)

