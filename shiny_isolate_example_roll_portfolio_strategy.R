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
uifun <- fluidPage(

    # Application title
    titlePanel("Rolling Portfolio Optimization Strategy for a S&P500 Sub-portfolio"),

    fluidRow(
        # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
        column(width=8, 
               h4("Click the button 'Recalculate plot' to Recalculate the Shiny App."),
               actionButton("recalcb", "Recalculate the Model"))
    ),  # end fluidRow
    
    fluidRow(
        column(width=2, sliderInput("look_back", label="lookback value:",
                                    min=5, max=111, value=11, step=1)),
        column(width=2, sliderInput("alpha", label="Shrinkage intensity:",
                    min=0, max=1, value=0, step=0.01)),
        column(width=2, selectInput("dimax", label="max-eigen:",
                    choices=2:10, selected=3)),
        column(width=2, selectInput("lagg", label="lag:",
                    choices=2:10, selected=2))
    ),  # end fluidRow
    
    # Show a plot of the generated distribution
    # Create output plot panel
    # mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
    plotOutput("distPlot")
    )  # end fluidPage


# Define server code required to run the strategy and plot it
serv_er <- function(input, output) {
    
    # Data setup code
    load("/Users/jerzy/Develop/R/data/returns_percent_sp500.RData")
    # Subset 100 columns to reduce computations
    retv <- retv[, sample(1:NCOL(retv), 100)]
    stock_symbols <- colnames(retv)
    ncols <- NCOL(retv)
    endp <- rutils::calc_endpoints(retv, interval="weeks")
    endp <- endp[endp > (ncols+1)]
    nrows <- NROW(endp)
    # Calculate returns on equal weight portfolio
    indeks <- xts::xts(cumsum(retv %*% rep(1/sqrt(ncols), ncols)), index(retv))
    
    # Define the strategy function
    run_strategy <- function(retv, look_back, alpha, dimax, lagg) {
        # browser()
        # cat("look_back =", look_back, "\nalpha =", alpha, "\ndimax =", dimax, "\nlagg =", lagg, "\n")
        startp <- c(rep_len(1, look_back-1), endp[1:(nrows-look_back+1)])
        # Perform backtest in RcppArmadillo
        pnls <- HighFreq::back_test(excess=retv, 
                                     returns=retv,
                                     startp=startp-1,
                                     endp=endp-1,
                                     alpha=alpha,
                                     dimax=dimax)
        xts(cumsum(pnls), order.by=index(retv))
    }  # end run_strategy
    
    
    output$distPlot <- shiny::renderPlot({
        # Extract from input the strategy model parameters
        look_back <- isolate(input$look_back)
        alpha <- isolate(input$alpha)
        dimax <- isolate(as.numeric(input$dimax))
        lagg <- isolate(as.numeric(input$lagg))
        # Model is recalculated when the recalcb variable is updated
        input$recalcb
        
        # Run the trading strategy and plot it
        pnls <- run_strategy(retv, look_back, alpha, dimax, lagg)
        pnls <- cbind(pnls, indeks*max(pnls)/max(indeks))
        colnames(pnls) <- c("Strategy", "Index")
        pnls[c(1, endp), ]
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
shinyApp(ui = uifun, server = serv_er)

