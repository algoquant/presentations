library(shiny)
library(quantmod)
# Define server logic required to plot xts
shinyServer(function(input, output) {
# Expression that generates a histogram. The expression is
# wrapped in a call to renderPlot to indicate that:
#  1) It is "reactive" and therefore should be automatically
#     re-executed when inputs change
#  2) Its output type is a plot
  output$custom_plot <- renderPlot({
# plot a Normal probability distribution
    chartSeries(input$x_ts, 
                name=strsplit(names(input$x_ts)[1], split="[.]")[[1]][1], 
                theme=chartTheme("white"))
    
    curve(expr=dnorm(x, mean=input$mean, sd=input$std_dev), type="l", 
          xlim=c(-3, 3), 
          xlab="", ylab="", lwd=2, col="blue")
  })  # end renderPlot
})  # end shinyServer
