##############################
# This is a shiny app for simulating rolling portfolio 
# optimization strategies, which produces an interactive 
# dygraphs plot.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load packages
library(shiny)
library(dygraphs)
library(rutils)

# Model and data setup

symbolv <- c("DBC", "IEF", "VTI", "XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY")
nweights <- NROW(symbolv)
returns <- rutils::etfenv$returns[, symbolv]
returns <- zoo::na.locf(returns, na.rm=FALSE)
returns <- zoo::na.locf(returns, fromLast=TRUE)
# Calculate the vector of average daily excess returns.
# riskf is the daily risk-free rate.
riskf <- 0.03/260
excess <- returns - riskf

# Calculate equal weight portfolio
# ncols <- NCOL(returns)
indeks <- xts::xts(cumprod(1 + rowMeans(returns)),
                        index(returns))

# Define endpoints
endpoints <- rutils::calc_endpoints(returns, interval="months")
endpoints <- endpoints[endpoints > 2*nweights]
nrows <- NROW(endpoints)

# End setup code


## Define elements of the UI user interface
uiface <- shiny::shinyUI(fluidPage(
  
  titlePanel("Max Sharpe Strategy"),
  
  sidebarLayout(
    sidebarPanel(
      # Define look_back interval
      sliderInput("look_back", label="lookback interval (months):",
                  min=2, max=30, value=6, step=1),
      sliderInput("max_eigen", label="max_eigen:",
                  min=2, max=nweights, value=3, step=1),
      # Define the shrinkage intensity
      sliderInput("alpha", label="shrinkage intensity alpha:",
                  min=0.01, max=0.99, value=0.5, step=0.05)
    ),
    mainPanel(
      dygraphOutput("dygraph")
    )
  )
))  # end shinyUI interface


## Define the server code
servfunc <- shiny::shinyServer(function(input, output) {

  # Re-calculate the data and rerun the model
  datav <- reactive({
    # get model parameters from input
    look_back <- input$look_back
    max_eigen <- input$max_eigen
    alpha <- input$alpha
    # define startpoints
    startpoints <- c(rep_len(1, look_back-1), endpoints[1:(nrows-look_back+1)])
    # rerun the model
    retsp <- drop(HighFreq::back_test(excess=excess, 
                             returns=returns, 
                             startpoints=startpoints-1, 
                             endpoints=endpoints-1, 
                             alpha=alpha, 
                             max_eigen=max_eigen))
    strat_rets <- cbind(indeks,
                        cumprod(1 + retsp))
    colnames(strat_rets) <- c("equal_weight", "strat_rets")
    strat_rets
  })  # end reactive code
  
  # Create the output plot
  output$dygraph <- renderDygraph({
    dygraph(datav(), main="Max Sharpe Strategy") %>%
      dyAxis("y", label="strat_rets", independentTicks=TRUE) %>%
      dyAxis("y2", label="equal_weight", independentTicks=TRUE) %>%
      dySeries(name="strat_rets", strokeWidth=1, axis="y", col="red") %>%
      dySeries(name="equal_weight", strokeWidth=1, axis="y2", col="blue")
    
  })  # end output plot
  
})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfunc)
