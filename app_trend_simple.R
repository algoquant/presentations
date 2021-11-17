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

# sym_bols <- names(data_env)
sym_bols <- rutils::etf_env$sym_bols
# sym_bols <- sym_bols[!(sym_bols %in% c("TLT", "IEF", "MTUM", "QUAL", "VLUE", "USMV"))]
# re_turns <- rutils::etf_env$re_turns[, sym_bols]

# load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# sym_bols <- sort(colnames(re_turns))


cap_tion <- paste("Trend Following and Mean Reverting Strategies")

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(cap_tion),
  
  # fluidRow(
  # The Shiny App is recalculated when the actionButton is clicked and the add_annotations variable is updated
  #   column(width=12,
  #          h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
  #          actionButton("add_annotations", "Recalculate the Model"))
  # ),  # end fluidRow
  
  # Create single row with inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("sym_bol", label="Symbol",
                                choices=sym_bols, selected="VTI")),
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
  mainPanel(dygraphs::dygraphOutput("dy_graph", width="100%", height="600px"), height=10, width=12)

)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {
  
  # Recalculate the data and rerun the model
  # da_ta <- reactive({
  # Get model parameters from input argument
  # max_eigen <- isolate(input$max_eigen)
  # look_lag <- isolate(input$look_lag
  # lamb_da <- isolate(input$lamb_da)
  # typ_e <- isolate(input$typ_e)
  # al_pha <- isolate(input$al_pha)
  # percen_tile <- isolate(input$percen_tile)
  # co_eff <- as.numeric(isolate(input$co_eff))
  # bid_offer <- isolate(input$bid_offer)
  # Model is recalculated when the add_annotations variable is updated
  # input$add_annotations
  
  
  # look_back <- 11
  # half_window <- look_back %/% 2

  # Create an empty list of reactive values.
  globals <- reactiveValues()
  
  # Calculate re_turns and variance
  da_ta <- reactive({
    # Get model parameters from input argument
    data_type <- input$data_type
    sym_bol <- input$sym_bol
    look_back <- input$look_back
    
    # Load data if needed
    switch(data_type,
           "Returns" = {
             cat("Loading Returns data \n")
             re_turns <- na.omit(rutils::etf_env$re_turns[, sym_bol])
             cumrets <- HighFreq::roll_sum(re_turns, look_back=look_back)
             variance <- HighFreq::roll_var(re_turns, look_back=look_back)
           },
           "OHLC" = {
             oh_lc <- get(sym_bol, envir=rutils::etf_env)
             re_turns <- rutils::diff_it(log(quantmod::Cl(oh_lc)))
             cumrets <- HighFreq::roll_sum(re_turns, look_back=look_back)
             variance <- HighFreq::roll_var_ohlc(log(oh_lc), look_back=look_back)
           }
    )  # end switch
    
    da_ta <- cbind(re_turns, cumrets, variance)
    colnames(da_ta) <- c(sym_bol, "cumrets", "variance")
    da_ta
    
  })  # end Load the data



  # Calculate predictor
  predictor <- reactive({
    cat("Calculating predictor\n")
    predictor_type <- input$predictor_type

    # Calculate the predictor
    switch(predictor_type,
           "Returns" = {
             da_ta()[, 2]
           },
           "Sharpe" = {
             variance <- da_ta()[, 3]
             ifelse(variance > 0, da_ta()[, 2]/sqrt(variance), 0)
           },
           "Volatility" = {
             sqrt(da_ta()[, 3])
           },
           "Skew" = {
             sqrt(da_ta()[, 3])
           }
    )  # end switch
    
  })  # end Calculate predictors
  
  # Plot histogram of predictor
  # range(predictor)
  # predictor <- predictor[predictor > quantile(predictor, 0.05)]
  # predictor <- predictor[predictor < quantile(predictor, 0.95)]
  # x11(width=6, height=5)
  # hist(predictor, xlim=c(quantile(predictor, 0.05), quantile(predictor, 0.95)), breaks=50, main=paste("Z-scores for", "look_back =", look_back))
  
  # Calculate pnl_s
  pnl_s <- reactive({
    cat("Calculating pnl_s\n")
    lambda <- input$lambda
    trend <- as.numeric(input$trend)
    
    position_s <- tanh(lambda*predictor())
    position_s <- rutils::lag_it(position_s, lagg=1)
    pnl_s <- trend*position_s*da_ta()[, 1, drop=FALSE]
    colnames(pnl_s) <- "Strategy"
    pnl_s
    
  })  # end Calculate pnl_s
  

  # Plot dygraph
  dy_graph <- reactive({
    cat("Plotting pnl_s\n")
    
    da_ta <- cbind(da_ta()[, 1, drop=FALSE], pnl_s())
    col_names <- colnames(da_ta)

    # Calculate Sharpe ratios
    sharp_e <- sqrt(252)*sapply(da_ta, function(x) mean(x)/sd(x[x<0]))
    sharp_e <- round(sharp_e, 3)

    # cap_tion <- paste("Contrarian Strategy for", input$sym_bol, "Using the Hampel Filter Over Prices")
    if (input$trend == "1") {
      cap_tion <- paste("Trending Strategy for", input$sym_bol, "Over ", input$data_type, "/ \n", 
                        paste0(c("Index SR=", "Strategy SR="), sharp_e, collapse=" / "))
    } else if (input$trend == "-1") {
      cap_tion <- paste("Mean Reverting Strategy for", input$sym_bol, "Over ", input$data_type, "/ \n", 
                        paste0(c("Index SR=", "Strategy SR="), sharp_e, collapse=" / "))
    }  # end if
    
    dygraphs::dygraph(cumsum(da_ta), main=cap_tion) %>%
      dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
      dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="blue") %>%
      dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="red")

  })  # end reactive

  # Render the dy_graph object
  # Return to the output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph(dy_graph())
    
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
