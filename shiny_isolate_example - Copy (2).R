#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    inputPanel(
        sliderInput("look_back", label="lookback value:",
                    min=5, max=111, value=11, step=1),
        # actionButton("re_calculate", "Recalculate z-scores"),
        sliderInput("al_pha", label="Shrinkage intensity:",
                    min=0, max=1, value=0, step=0.01),
        selectInput("max_eigen", label="max-eigen:",
                    choices=2:10, selected=3),
        selectInput("lagg", label="lag:",
                    choices=2:10, selected=2)
    ),  # end inputPanel
    
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    # This is the R chunk setup code
    knitr::opts_chunk$set(echo=TRUE)
    library(HighFreq)  # load package HighFreq
    # End R chunk setup code
    
    # Data setup code
    load("C:/Develop/R/data/returns_percent_sp500.RData")
    # Subset 100 columns to reduce computations
    re_turns <- re_turns[, sample(1:NCOL(re_turns), 100)]
    stock_symbols <- colnames(re_turns)
    n_cols <- NCOL(re_turns)
    end_points <- rutils::calc_endpoints(re_turns, inter_val="months")
    end_points <- end_points[end_points > (n_cols+1)]
    n_rows <- NROW(end_points)
    
    # Define the strategy function
    run_strategy <- function(re_turns, look_back, al_pha, max_eigen, lagg) {
        # browser()
        # cat("look_back =", look_back, "\nal_pha =", al_pha, "\nmax_eigen =", max_eigen, "\nlagg =", lagg, "\n")
        start_points <- c(rep_len(1, look_back-1), end_points[1:(n_rows-look_back+1)])
        # Perform backtest in RcppArmadillo
        pnl_s <- HighFreq::back_test(ex_cess=re_turns, 
                                     re_turns=re_turns,
                                     start_points=start_points-1,
                                     end_points=end_points-1,
                                     al_pha=al_pha,
                                     max_eigen=max_eigen)
        xts(cumsum(pnl_s), order.by=index(re_turns))
    }  # end run_strategy
    
    
    output$distPlot <- renderPlot({
        # Extract from input the strategy model parameters
        look_back <- input$look_back
        al_pha <- input$al_pha
        max_eigen <- input$max_eigen
        lagg <- input$lagg
        
        # Run the trading strategy and plot it
        pnl_s <- run_strategy(re_turns, look_back, al_pha, max_eigen, lagg)
        chart_Series(pnl_s, name="Cumulative PnL of a Stock Momentum Strategy")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
