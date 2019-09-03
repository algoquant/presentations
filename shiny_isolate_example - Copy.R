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
ui <- inputPanel(
    
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    sliderInput("look_back", label="look_back:",
                min=5, max=1111, value=111, step=1),
    actionButton("re_calculate", "Recalculate z-scores"),
    sliderInput("thresh_old", label="threshold:",
                min=0.1, max=10, value=1, step=0.1),
    
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("plot_strat")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    library(HighFreq)  # load package HighFreq
    # load("C:/Develop/data/SPY_design.RData")
    price_s <- as.numeric(HighFreq::SPY[, "SPY.Close"])
    in_dex <- matrix(.index(HighFreq::SPY), nc=1)
    re_turns <- rutils::diff_it(HighFreq::SPY[, "SPY.Close"])
    
    output$plot_strat <- renderPlot({
        
        input$re_calculate
        look_back <- isolate(input$look_back)
        thresh_old <- input$thresh_old
        z_scores <- HighFreq::roll_zscores(price_s, in_dex, look_back=look_back)
        position_s <- rep.int(NA_integer_, NROW(HighFreq::SPY))
        position_s[1] <- 0
        # buy signal
        bu_y <- (z_scores < -thresh_old)
        position_s[bu_y] <- 1
        se_ll <- (z_scores > thresh_old)
        position_s[se_ll] <- (-1)
        position_s <- zoo::na.locf(position_s)
        # Lag the positions
        position_s <- c(0, position_s[-NROW(position_s)])
        chart_Series(cumsum(position_s * re_turns)[endpoints(HighFreq::SPY, on="days")], name="Backtest of static beta strategy for SPY")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
