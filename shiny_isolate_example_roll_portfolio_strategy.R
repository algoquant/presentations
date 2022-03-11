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
uiface <- fluidPage(

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
        column(width=2, sliderInput("alpha", label="Shrinkage intensity:",
                    min=0, max=1, value=0, step=0.01)),
        column(width=2, selectInput("max_eigen", label="max-eigen:",
                    choices=2:10, selected=3)),
        column(width=2, selectInput("lagg", label="lag:",
                    choices=2:10, selected=2))
    ),  # end fluidRow
    
    # Show a plot of the generated distribution
    # Create output plot panel
    # mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
    mainPanel(plotOutput("distPlot"), width=12)
    )  # end fluidPage


# Define server code required to run the strategy and plot it
serv_er <- function(input, output) {
    
    # Data setup code
    load("C:/Develop/R/data/returns_percent_sp500.RData")
    # Subset 100 columns to reduce computations
    returns <- returns[, sample(1:NCOL(returns), 100)]
    stock_symbols <- colnames(returns)
   .n_cols <- NCOL(returns)
    endpoints <- rutils::calc_endpoints(returns, interval="weeks")
    endpoints <- endpoints[endpoints > .n_cols+1)]
   .n_rows <- NROW(endpoints)
    # Calculate returns on equal weight portfolio
    indeks <- xts(cumsum(returns %*% rep(1/sqrt.n_cols),.n_cols)), index(returns))
    
    # Define the strategy function
    run_strategy <- function(returns, look_back, alpha, max_eigen, lagg) {
        # browser()
        # cat("look_back =", look_back, "\nalpha =", alpha, "\nmax_eigen =", max_eigen, "\nlagg =", lagg, "\n")
        startpoints <- c(rep_len(1, look_back-1), endpoints[1:.n_rows-look_back+1)])
        # Perform backtest in RcppArmadillo
        pnls <- HighFreq::back_test(excess=returns, 
                                     returns=returns,
                                     startpoints=startpoints-1,
                                     endpoints=endpoints-1,
                                     alpha=alpha,
                                     max_eigen=max_eigen)
        xts(cumsum(pnls), order.by=index(returns))
    }  # end run_strategy
    
    
    output$distPlot <- shiny::renderPlot({
        # Extract from input the strategy model parameters
        look_back <- isolate(input$look_back)
        alpha <- isolate(input$alpha)
        max_eigen <- isolate(as.numeric(input$max_eigen))
        lagg <- isolate(as.numeric(input$lagg))
        # Model is recalculated when the re_calculate variable is updated
        input$re_calculate
        
        # Run the trading strategy and plot it
        pnls <- run_strategy(returns, look_back, alpha, max_eigen, lagg)
        pnls <- cbind(pnls, indeks*max(pnls)/max(indeks))
        colnames(pnls) <- c("Strategy", "Index")
        pnls[c(1, endpoints), ]
        # output$dyplot <- dygraphs::renderDygraph({
        #     colnamev <- colnames(datav())
        #     dygraphs::dygraph(datav(), main="Rolling Portfolio Optimization Strategy") %>%
        #         dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        #         dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        #         dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
        #         dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue")
        # })  # end output plot
        plot_theme <- chart_theme()
        plot_theme$col$line.col <- c("orange", "blue")
        chobj <- chart_Series(pnls, theme=plot_theme, name="Cumulative PnL of a Stock Momentum Strategy")
        plot(chobj)
        legend("top", legend=colnames(pnls),
               inset=0.1, bg="white", lty=1, lwd=6,
               col=plot_theme$col$line.col, bty="n")
    })
}

# Run the Shiny application 
shinyApp(ui = uiface, server = serv_er)

