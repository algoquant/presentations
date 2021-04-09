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
inter_face <- shiny::fluidPage(
  titlePanel("View mtcars Data Frame"),
  fluidRow(
    # Input stock symbol
    column(width=4, numericInput("n_rows", "Enter number of rows of data: ", value=5)),
    column(width=4, actionButton("but_ton", "Press to Show Data")),  # end column
  ),  # end fluidRow
  
  column(8, tableOutput("tabl_e"))
  
)  # end fluidPage interface


## Define the server function
ser_ver <- function(input, output) {
  
  ##############################
  # The function reactiveValues() creates a list for storing 
  # reactive values, which can be updated by event handlers. 
  # The list elements have dependencies with reactive expressions
  # where they are called.
  
  # Create an empty list of reactive values.
  value_s <- reactiveValues()
  
  
  ##############################
  # The function reactive() transforms an expression into a 
  # reactive expression.  Reactive expressions are evaluated 
  # only when their input data is updated.  This avoids 
  # performing unnecessary calculations.
  # The function reactive() usually returns a value.
  # If the reactive expression is invalidated (recalculated), 
  # then other expressions that depend on its output are also 
  # recalculated. 
  # This way calculations cascade through the expressions that 
  # depend on each other.
  
  # Get input parameters from the user interface.
  n_rows <- reactive({
    # Add n_rows to list of reactive values.
    value_s$n_rows <- input$n_rows
    input$n_rows
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
  # The difference between reactive() and eventReactive() is that 
  # reactive() recalculates its expression only when its inputs 
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
  observeEvent(eventExpr=input$but_ton, handlerExpr={
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
  da_ta <- eventReactive(eventExpr=input$but_ton, valueExpr={
    # eventReactive() executes on input$but_ton, but
    # not on n_rows() or input$n_rows.
    cat("Sending", n_rows(), "rows of data\n")
    da_ta <- head(mtcars, input$n_rows)
    value_s$mpg <- mean(da_ta$mpg)
    da_ta
  })  # end eventReactive
  #   da_ta
  
  
  # Draw table of the data when the button is pressed.
  observeEvent(eventExpr=input$but_ton, handlerExpr={
    da_ta <- da_ta()
    cat("Received", value_s$n_rows, "rows of data\n")
    cat("Average mpg = ", value_s$mpg, "\n")
    cat("Drawing table\n")
    output$tabl_e <- renderTable(da_ta) 
  })  # end observeEvent
  
}  # end server code


## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)

