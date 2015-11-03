library(shiny)

# Define UI for application that plots an xts
shinyUI(fluidPage(
  titlePanel("chartSeries High Frequency Data"),

  sidebarLayout(
    sidebarPanel(
      helpText("Select a stock symbol to plot"),

      textInput(inputId="sym_bol", label="Symbol", value="SPY"),

      dateRangeInput("dates", 
        "Date range",
        start="2013-01-01", 
        end=as.character(Sys.Date()))
    ),

    mainPanel(plotOutput("plot"))
  )
))