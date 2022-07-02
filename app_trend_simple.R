##############################
# This is a shiny app for simulating trend following and
# mean reverting strategies using a single predictor.
# It uses reactive code to avoid unnecessary calculations.
# 
# Just press the "Run App" button on upper right of this panel.
##############################


## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Model and data setup

# symbolv <- names(data_env)
symbolv <- rutils::etfenv$symbolv
# symbolv <- symbolv[!(symbolv %in% c("TLT", "IEF", "MTUM", "QUAL", "VLUE", "USMV"))]
# returns <- rutils::etfenv$returns[, symbolv]

# load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# symbolv <- sort(colnames(returns))


captiont <- paste("Trend Following and Mean Reverting Strategies")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  # fluidRow(
  # The Shiny App is recalculated when the actionButton is clicked and the add_annotations variable is updated
  #   column(width=12,
  #          h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
  #          actionButton("add_annotations", "Recalculate the Model"))
  # ),  # end fluidRow
  
  # Create single row with inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol",
                                choices=symbolv, selected="VTI")),
    # Input data type Boolean
    column(width=2, selectInput("data_type", label="Select data type", choices=c("Returns", "OHLC"), selected="Returns")),
    # Input data type Boolean
    column(width=2, selectInput("predictor_type", label="Select predictor type", choices=c("Returns", "Sharpe", "Volatility", "Skew"), selected="Sharpe"))
  ),  # end fluidRow
  
  # Create single row with inputs
  fluidRow(
    # Input Look back interval
    column(width=2, sliderInput("look_back", label="Look back", min=3, max=250, value=100, step=1)),
    # Input Look back interval
    column(width=2, sliderInput("lambda", label="leverage parameter", min=0.1, max=10, value=1, step=0.1)),
    # If trend=1 then trending, If trend=(-1) then contrarian
    column(width=2, selectInput("trend", label="Trend coefficient",
                                choices=c(1, -1), selected=(1)))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot", width="100%", height="600px"), height=10, width=12)

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # Recalculate the data and rerun the model
  # datav <- shiny::reactive({
  # Get model parameters from input argument
  # dimax <- isolate(input$dimax)
  # look_lag <- isolate(input$look_lag
  # lambda <- isolate(input$lambda)
  # typev <- isolate(input$typev)
  # alpha <- isolate(input$alpha)
  # quant <- isolate(input$quant)
  # coeff <- as.numeric(isolate(input$coeff))
  # bid_offer <- isolate(input$bid_offer)
  # Model is recalculated when the add_annotations variable is updated
  # input$add_annotations
  
  
  # look_back <- 11
  # half_window <- look_back %/% 2

  # Create an empty list of reactive values.
  globals <- reactiveValues()
  
  # Calculate returns and variance
  datav <- shiny::reactive({
    # Get model parameters from input argument
    data_type <- input$data_type
    symbol <- input$symbol
    look_back <- input$look_back
    
    # Load data if needed
    switch(data_type,
           "Returns" = {
             cat("Loading Returns data \n")
             returns <- na.omit(rutils::etfenv$returns[, symbol])
             cumrets <- HighFreq::roll_sum(returns, look_back=look_back)
             variance <- HighFreq::roll_var(returns, look_back=look_back)
           },
           "OHLC" = {
             ohlc <- get(symbol, envir=rutils::etfenv)
             returns <- rutils::diffit(log(quantmod::Cl(ohlc)))
             cumrets <- HighFreq::roll_sum(returns, look_back=look_back)
             variance <- HighFreq::roll_var_ohlc(log(ohlc), look_back=look_back)
           }
    )  # end switch
    
    datav <- cbind(returns, cumrets, variance)
    colnames(datav) <- c(symbol, "cumrets", "variance")
    datav
    
  })  # end Load the data



  # Calculate predictor
  predictor <- shiny::reactive({
    cat("Calculating predictor\n")
    predictor_type <- input$predictor_type

    # Calculate the predictor
    switch(predictor_type,
           "Returns" = {
             datav()[, 2]
           },
           "Sharpe" = {
             variance <- datav()[, 3]
             ifelse(variance > 0, datav()[, 2]/sqrt(variance), 0)
           },
           "Volatility" = {
             sqrt(datav()[, 3])
           },
           "Skew" = {
             sqrt(datav()[, 3])
           }
    )  # end switch
    
  })  # end Calculate predictors
  
  # Plot histogram of predictor
  # range(predictor)
  # predictor <- predictor[predictor > quantile(predictor, 0.05)]
  # predictor <- predictor[predictor < quantile(predictor, 0.95)]
  # x11(width=6, height=5)
  # hist(predictor, xlim=c(quantile(predictor, 0.05), quantile(predictor, 0.95)), breaks=50, main=paste("Z-scores for", "look_back =", look_back))
  
  # Calculate pnls
  pnls <- shiny::reactive({
    cat("Calculating pnls\n")
    lambda <- input$lambda
    trend <- as.numeric(input$trend)
    
    posit <- tanh(lambda*predictor())
    posit <- rutils::lagit(posit, lagg=1)
    pnls <- trend*posit*datav()[, 1, drop=FALSE]
    colnames(pnls) <- "Strategy"
    pnls
    
  })  # end Calculate pnls
  

  # Plot dygraph
  dyplot <- shiny::reactive({
    cat("Plotting pnls\n")
    
    datav <- cbind(datav()[, 1, drop=FALSE], pnls())
    colnamev <- colnames(datav)

    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(datav, function(x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 3)

    # captiont <- paste("Contrarian Strategy for", input$symbol, "Using the Hampel Filter Over Prices")
    if (input$trend == "1") {
      captiont <- paste("Trending Strategy for", input$symbol, "Over ", input$data_type, "/ \n", 
                        paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "))
    } else if (input$trend == "-1") {
      captiont <- paste("Mean Reverting Strategy for", input$symbol, "Over ", input$data_type, "/ \n", 
                        paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "))
    }  # end if
    
    dygraphs::dygraph(cumsum(datav), main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red")

  })  # end reactive

  # Render the dyplot object
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph(dyplot())
    
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
