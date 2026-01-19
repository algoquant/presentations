# A shiny app in R for a 3x3 dashboard of random plotly plots
# If the user clicks on any of the plots then enlarge it


library(shiny)
library(bslib)
library(plotly)

# Generate random data for different plot types
generate_random_plot <- function(plot_id) {
  set.seed(plot_id * 42)  # Different seed for each plot
  
  plot_types <- c("scatter", "bar", "histogram", "line", "boxplot", "heatmap")
  plot_type <- sample(plot_types, 1)
  
  n <- sample(20:50, 1)
  
  if (plot_type == "scatter") {
    x <- rnorm(n)
    y <- rnorm(n) + 0.5 * x
    p <- plot_ly(x = ~x, y = ~y, type = 'scatter', mode = 'markers',
                 marker = list(color = sample(colors(), 1))) %>%
      layout(title = paste("Scatter Plot", plot_id))
  } else if (plot_type == "bar") {
    categories <- paste("Cat", 1:8)
    values <- sample(10:100, 8)
    p <- plot_ly(x = ~categories, y = ~values, type = 'bar',
                 marker = list(color = sample(colors(), 8))) %>%
      layout(title = paste("Bar Chart", plot_id))
  } else if (plot_type == "histogram") {
    data <- rnorm(n, mean = sample(-5:5, 1), sd = sample(1:3, 1))
    p <- plot_ly(x = ~data, type = "histogram", 
                 marker = list(color = sample(colors(), 1))) %>%
      layout(title = paste("Histogram", plot_id))
  } else if (plot_type == "line") {
    x <- 1:n
    y <- cumsum(rnorm(n))
    p <- plot_ly(x = ~x, y = ~y, type = 'scatter', mode = 'lines',
                 line = list(color = sample(colors(), 1))) %>%
      layout(title = paste("Line Chart", plot_id))
  } else if (plot_type == "boxplot") {
    groups <- rep(paste("Group", 1:4), each = n/4)
    values <- rnorm(length(groups), mean = rep(c(0, 2, -1, 3), each = n/4))
    p <- plot_ly(y = ~values, color = ~groups, type = "box") %>%
      layout(title = paste("Box Plot", plot_id))
  } else {  # heatmap
    matrix_data <- matrix(rnorm(64), nrow = 8, ncol = 8)
    p <- plot_ly(z = ~matrix_data, type = "heatmap") %>%
      layout(title = paste("Heatmap", plot_id))
  }
  
  return(p)
}

ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Interactive 3x3 Dashboard"),
  
  # Modal for enlarged plot
  modalDialog(
    id = "plot_modal",
    title = "Enlarged Plot",
    plotlyOutput("enlarged_plot", height = "500px"),
    footer = modalButton("Close"),
    size = "l",
    fade = FALSE
  ) %>% 
    tagList() %>% 
    {.[["children"]] <- NULL; .} %>% 
    {div(id = "plot_modal", style = "display: none;", .)},
  
  # Generate refresh button
  fluidRow(
    column(12, 
      actionButton("refresh", "Generate New Plots", 
                   class = "btn-primary mb-3")
    )
  ),
  
  # 3x3 grid of plots
  fluidRow(
    column(4, card(plotlyOutput("plot1", height = "300px"))),
    column(4, card(plotlyOutput("plot2", height = "300px"))),
    column(4, card(plotlyOutput("plot3", height = "300px")))
  ),
  fluidRow(
    column(4, card(plotlyOutput("plot4", height = "300px"))),
    column(4, card(plotlyOutput("plot5", height = "300px"))),
    column(4, card(plotlyOutput("plot6", height = "300px")))
  ),
  fluidRow(
    column(4, card(plotlyOutput("plot7", height = "300px"))),
    column(4, card(plotlyOutput("plot8", height = "300px"))),
    column(4, card(plotlyOutput("plot9", height = "300px")))
  ),
  
  # Add modal HTML
  tags$div(id = "plotModal", class = "modal fade", tabindex = "-1",
    tags$div(class = "modal-dialog modal-lg",
      tags$div(class = "modal-content",
        tags$div(class = "modal-header",
          tags$h5("Enlarged Plot", class = "modal-title"),
          tags$button(type = "button", class = "btn-close", 
                     `data-bs-dismiss` = "modal")
        ),
        tags$div(class = "modal-body",
          plotlyOutput("enlarged_plot", height = "500px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to trigger plot regeneration
  plot_trigger <- reactiveVal(1)
  
  # Regenerate plots when refresh button is clicked
  observeEvent(input$refresh, {
    plot_trigger(plot_trigger() + 1)
  })
  
  # Store the current plots
  plots <- reactive({
    plot_trigger()  # Depend on the trigger
    lapply(1:9, generate_random_plot)
  })
  
  # Generate all 9 plots
  output$plot1 <- renderPlotly({
    plots()[[1]] %>% 
      config(displayModeBar = FALSE) %>%
      event_register("plotly_click")
  })
  output$plot2 <- renderPlotly({
    plots()[[2]] %>% 
      config(displayModeBar = FALSE) %>%
      event_register("plotly_click")
  })
  output$plot3 <- renderPlotly({
    plots()[[3]] %>% 
      config(displayModeBar = FALSE) %>%
      event_register("plotly_click")
  })
  output$plot4 <- renderPlotly({
    plots()[[4]] %>% 
      config(displayModeBar = FALSE) %>%
      event_register("plotly_click")
  })
  output$plot5 <- renderPlotly({
    plots()[[5]] %>% 
      config(displayModeBar = FALSE) %>%
      event_register("plotly_click")
  })
  output$plot6 <- renderPlotly({
    plots()[[6]] %>% 
      config(displayModeBar = FALSE) %>%
      event_register("plotly_click")
  })
  output$plot7 <- renderPlotly({
    plots()[[7]] %>% 
      config(displayModeBar = FALSE) %>%
      event_register("plotly_click")
  })
  output$plot8 <- renderPlotly({
    plots()[[8]] %>% 
      config(displayModeBar = FALSE) %>%
      event_register("plotly_click")
  })
  output$plot9 <- renderPlotly({
    plots()[[9]] %>% 
      config(displayModeBar = FALSE) %>%
      event_register("plotly_click")
  })
  
  # Handle clicks on each plot to show enlarged version
  observeEvent(event_data("plotly_click", source = "plot1"), {
    output$enlarged_plot <- renderPlotly({
      plots()[[1]] %>% layout(height = 500)
    })
    showModal(modalDialog(
      title = "Enlarged Plot 1",
      plotlyOutput("enlarged_plot"),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  observeEvent(event_data("plotly_click", source = "plot2"), {
    output$enlarged_plot <- renderPlotly({
      plots()[[2]] %>% layout(height = 500)
    })
    showModal(modalDialog(
      title = "Enlarged Plot 2",
      plotlyOutput("enlarged_plot"),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  observeEvent(event_data("plotly_click", source = "plot3"), {
    output$enlarged_plot <- renderPlotly({
      plots()[[3]] %>% layout(height = 500)
    })
    showModal(modalDialog(
      title = "Enlarged Plot 3",
      plotlyOutput("enlarged_plot"),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  observeEvent(event_data("plotly_click", source = "plot4"), {
    output$enlarged_plot <- renderPlotly({
      plots()[[4]] %>% layout(height = 500)
    })
    showModal(modalDialog(
      title = "Enlarged Plot 4",
      plotlyOutput("enlarged_plot"),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  observeEvent(event_data("plotly_click", source = "plot5"), {
    output$enlarged_plot <- renderPlotly({
      plots()[[5]] %>% layout(height = 500)
    })
    showModal(modalDialog(
      title = "Enlarged Plot 5",
      plotlyOutput("enlarged_plot"),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  observeEvent(event_data("plotly_click", source = "plot6"), {
    output$enlarged_plot <- renderPlotly({
      plots()[[6]] %>% layout(height = 500)
    })
    showModal(modalDialog(
      title = "Enlarged Plot 6",
      plotlyOutput("enlarged_plot"),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  observeEvent(event_data("plotly_click", source = "plot7"), {
    output$enlarged_plot <- renderPlotly({
      plots()[[7]] %>% layout(height = 500)
    })
    showModal(modalDialog(
      title = "Enlarged Plot 7",
      plotlyOutput("enlarged_plot"),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  observeEvent(event_data("plotly_click", source = "plot8"), {
    output$enlarged_plot <- renderPlotly({
      plots()[[8]] %>% layout(height = 500)
    })
    showModal(modalDialog(
      title = "Enlarged Plot 8",
      plotlyOutput("enlarged_plot"),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  observeEvent(event_data("plotly_click", source = "plot9"), {
    output$enlarged_plot <- renderPlotly({
      plots()[[9]] %>% layout(height = 500)
    })
    showModal(modalDialog(
      title = "Enlarged Plot 9",
      plotlyOutput("enlarged_plot"),
      size = "l",
      easyClose = TRUE
    ))
  })
}

shinyApp(ui = ui, server = server)
