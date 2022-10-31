library(shiny)
# Define UI for application that draws a histogram
fluidPage(

  # Application title
  # titlePanel("Normal distribution"),
  sidebarLayout(
    # div(style="height: 10px;",
    sidebarPanel(
      # style = paste0("height: 50vh; overflow-y: auto;"), ##CHANGE
      # Slider inputs
      fluidRow(
               column(width=6, sliderInput("rho", label="Correlation:", min=0.0, max=0.9, value=0.2, step=0.01)),
               column(width=6, sliderInput("defprob", label="Default probability:", min=0.0, max=0.9, value=0.2, step=0.01))),
      fluidRow(
               column(width=6, sliderInput("lgd", label="Loss severity:", min=0.0, max=0.9, value=0.4, step=0.01)),
               column(width=6, sliderInput("attachp", label="Tranche attachment:", min=0.0, max=0.5, value=0.15, step=0.01))),
      fluidRow(
               column(width=6, sliderInput("detachp", label="Tranche detachment:", min=0.0, max=0.5, value=0.2, step=0.01))), 
      height=4, width=4
    ),  # end sidebarPanel
    
    # Render plot in panel
    mainPanel(plotOutput("plot_portf", width="100%", height=400))
  )  # end sidebarLayout
  
)  # end fluidPage
