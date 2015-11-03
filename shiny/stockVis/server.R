# server.R

library(quantmod)
# source("helpers.R")

# Define server logic required to plot xts
shinyServer(function(input, output) {

  output$plot <- renderPlot({
    ran_ge <- paste(input$dates[1], input$dates[2], sep="/")
    chartSeries(get(input$sym_bol)[ran_ge], 
                name=input$sym_bol, 
                theme=chartTheme("white"))
  })
  
})