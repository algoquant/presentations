library(shiny)
# Define UI for application that draws a histogram
fluidPage(
  
  # application title
  # titlePanel("Normal distribution"),
  sidebarLayout(
    sidebarPanel(
      # slider inputs
      sliderInput("rh_o", label="Correlation:", min=0.0, max=0.9, value=0.2, step=0.01),
      sliderInput("def_prob", label="Default probability:", min=0.0, max=0.9, value=0.2, step=0.01),
      sliderInput("l_gd", label="Loss severity:", min=0.0, max=0.9, value=0.4, step=0.01),
      sliderInput("at_tach", label="Tranche attachment:", min=0.0, max=0.5, value=0.15, step=0.01),
      sliderInput("de_tach", label="Tranche detachment:", min=0.0, max=0.5, value=0.2, step=0.01), 
      width=3
    ),  # end sidebarPanel
    
    # Render plot in panel
    mainPanel(plotOutput("plot_portf"))
  )  # end sidebarLayout
  
)  # end fluidPage
