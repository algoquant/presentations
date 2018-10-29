library(shiny)
# Define UI for application that draws a histogram
fluidPage(

# application title
  titlePanel("Normal distribution"),
# sidebar with a slider input for the std dev
  sidebarLayout(
    sidebarPanel(
# slider input for the mean
      sliderInput(inputId="mean", label="mean:", min=-2.0, max=2.0, value=0.0, step=0.1),
# slider input for the std dev
      sliderInput(inputId="std_dev", label="std dev:", min=0.1, max=2.0, value=1.0)
    ),  # end sidebarPanel

# Show a plot of the generated distribution
    mainPanel(plotOutput("plo_t"))
  )  # end sidebarLayout

)  # end fluidPage
