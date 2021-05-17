#
# This is a Shiny web application for a 
#   Rolling Portfolio Optimization Strategy for a S&P500 Sub-portfolio
# You can run the application by clicking the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/
#
# Find out more about controlling Recalculation using isolate() here:
# https://shiny.rstudio.com/articles/isolation.html


## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(shiny)
library(HighFreq)

## End setup code


# Define user interface for application that draws a histogram
inter_face <- fluidPage(

    # Application title
    titlePanel("Rolling Portfolio Optimization Strategy for a S&P500 Sub-portfolio"),

    fluidRow(
        # The Shiny App is recalculated when the actionButton is clicked and the re_calculate variable is updated
        column(width=8, 
               h4("Click the button 'Recalculate plot' to Recalculate the Shiny App."),
               actionButton("re_calculate", "Recalculate the Model"))
    ),  # end fluidRow
    
    fluidRow(
        column(width=2, sliderInput("look_back", label="lookback value:",
                                    min=5, max=111, value=11, step=1)),
        column(width=2, sliderInput("al_pha", label="Shrinkage intensity:",
                    min=0, max=1, value=0, step=0.01)),
        column(width=2, selectInput("max_eigen", label="max-eigen:",
                    choices=2:10, selected=3)),
        column(width=2, selectInput("lagg", label="lag:",
                    choices=2:10, selected=2))
    ),  # end fluidRow
    
    # Show a plot of the generated distribution
    # Create output plot panel
    # mainPanel(dygraphs::dygraphOutput("dy_graph"), width=12)
    mainPanel(plotOutput("distPlot"), width=12)
    )  # end fluidPage


# Define server code required to run the strategy and plot it
serv_er <- function(input, output) {
    
    # Data setup code
    load("C:/Develop/R/data/returns_percent_sp500.RData")
    # Subset 100 columns to reduce computations
    re_turns <- re_turns[, sample(1:NCOL(re_turns), 100)]
    stock_symbols <- colnames(re_turns)
    n_cols <- NCOL(re_turns)
    end_points <- rutils::calc_endpoints(re_turns, inter_val="weeks")
    end_points <- end_points[end_points > (n_cols+1)]
    n_rows <- NROW(end_points)
    # Calculate returns on equal weight portfolio
    in_dex <- xts(cumsum(re_turns %*% rep(1/sqrt(n_cols), n_cols)), index(re_turns))
    
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
    
    
    output$distPlot <- shiny::renderPlot({
        # Extract from input the strategy model parameters
        look_back <- isolate(input$look_back)
        al_pha <- isolate(input$al_pha)
        max_eigen <- isolate(as.numeric(input$max_eigen))
        lagg <- isolate(as.numeric(input$lagg))
        # Model is recalculated when the re_calculate variable is updated
        input$re_calculate
        
        # Run the trading strategy and plot it
        pnl_s <- run_strategy(re_turns, look_back, al_pha, max_eigen, lagg)
        pnl_s <- cbind(pnl_s, in_dex*max(pnl_s)/max(in_dex))
        colnames(pnl_s) <- c("Strategy", "Index")
        pnl_s[c(1, end_points), ]
        # output$dy_graph <- dygraphs::renderDygraph({
        #     col_names <- colnames(da_ta())
        #     dygraphs::dygraph(da_ta(), main="Rolling Portfolio Optimization Strategy") %>%
        #         dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
        #         dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
        #         dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
        #         dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue")
        # })  # end output plot
        plot_theme <- chart_theme()
        plot_theme$col$line.col <- c("orange", "blue")
        ch_ob <- chart_Series(pnl_s, theme=plot_theme, name="Cumulative PnL of a Stock Momentum Strategy")
        plot(ch_ob)
        legend("top", legend=colnames(pnl_s),
               inset=0.1, bg="white", lty=1, lwd=6,
               col=plot_theme$col$line.col, bty="n")
    })
}

# Run the Shiny application 
shinyApp(ui = inter_face, server = serv_er)

