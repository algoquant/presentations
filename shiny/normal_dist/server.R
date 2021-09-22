library(shiny)
# Define server logic required to draw a plot
shinyServer(function(input, output) {
# Expression that generates a plot. The expression is
# wrapped in a call to renderPlot to indicate that:
#  1) It is "reactive" and therefore should be automatically
#     re-executed when inputs change
#  2) Its output type is a plot
  output$distPlot <- renderPlot({
# plot a Normal probability distribution
    par(mar=c(3, 2, 0, 0), oma=c(0, 0, 0, 0))
    curve(expr=dnorm(x, mean=input$mean, sd=input$std_dev), type="l", 
          xlim=c(-4, 4), 
          xlab="", ylab="", lwd=2, col="blue")
  }, height=300, width=500)  # end renderPlot
})  # end shinyServer
