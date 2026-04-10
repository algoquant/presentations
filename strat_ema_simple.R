##############################
# This is a shiny app for simulating an EMA strategy using 
# a single predictor.
# The predictors can be the EMA returns, EMA volatility,
# Sharpe ratio, etc.
# The strategy positions depend on the EMA predictor.
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

# symbolv <- names(datenv)
symboletf <- rutils::etfenv$symbolv
symboletf <- symboletf[!(symboletf %in% c("MTUM", "QUAL", "VLUE", "USMV", "AIEQ"))]
# retp <- rutils::etfenv$returns[, symboletf]

load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
symbolstock <- sort(colnames(retstock))


captiont <- paste("EMA Trend Following Strategy")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  # Create single row with inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symboln", label="Symbol", choices=c(symboletf, symbolstock), selected="SPY")),
    # Input data type Boolean
    # column(width=2, selectInput("datatype", label="Select data type", choices=c("ETF", "Stock"), selected="ETF")),
    # Input predictor type Boolean
    column(width=2, selectInput("predictype", label="Select predictor type", choices=c("Returns", "Sharpe", "Kelly", "Volatility", "Variance"), selected="Sharpe"))
  ),  # end fluidRow
  
  # Create single row with inputs
  fluidRow(
    # Input lambda decay parameter
    column(width=2, sliderInput("lambdaf", label="lambda:", min=0.1, max=0.99, value=0.75, step=0.01)),
    # Input Look back interval
    # column(width=2, sliderInput("lookb", label="Look back", min=3, max=250, value=100, step=1)),
    # Input leverage parameter
    column(width=2, sliderInput("leveragep", label="leverage parameter", min=0.1, max=10, value=1, step=0.1)),
    # If trend=1 then trending, If trend=(-1) then contrarian
    column(width=2, selectInput("trend", label="Trend coefficient", choices=c(1, -1), selected=(-1)))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Calculate returns and variance
  retpar <- shiny::reactive({
    # Get model parameters from input argument
    symboln <- input$symboln
    lambdaf <- input$lambdaf

    if (symboln %in% symboletf) {
      cat("Loading ETF returns \n")
      # Get ETF returns from rutils::etfenv
      retp <- na.omit(get(symboln, rutils::etfenv$returns))
    } else if (symboln %in% symbolstock) {
      cat("Loading stock returns \n")
      # Get stock returns from retstock
      retp <- na.omit(get(symboln, retstock))
    }  # end if
    
    # Calculate the EMA returns and variance
    varema <- HighFreq::run_var(retp, lambdaf=lambdaf)
    retpar <- cbind(retp, varema)
    colnames(retpar) <- c(symboln, "retema", "variance")
    return(retpar)
    
  })  # end Load the data



  # Calculate predictor
  predv <- shiny::reactive({
    cat("Calculating predictor\n")
    predictype <- input$predictype

    # Calculate the predictor
    retpar <- retpar()
    switch(predictype,
           "Returns" = {
             predv <- retpar$retema
           },
           "Sharpe" = {
             vars <- retpar$variance
             predv <- ifelse(vars > 0, retpar$retema/sqrt(vars), 0)
           },
           "Kelly" = {
             vars <- retpar$variance
             predv <- ifelse(vars > 0, retpar$retema/vars, 0)
           },
           "Volatility" = {
             predv <- 1/sqrt(retpar$variance)
           },
           "Variance" = {
             predv <- 1/retpar$variance
           }
    )  # end switch
    
    return(predv)
    
  })  # end Calculate predictors
  
  # Plot histogram of predictor
  # range(predv)
  # predv <- predv[predictor > quantile(predv, 0.05)]
  # predv <- predv[predictor < quantile(predv, 0.95)]
  # x11(width=6, height=5)
  # hist(predv, xlim=c(quantile(predv, 0.05), quantile(predv, 0.95)), breaks=50, main=paste("Z-scores for", "lookb =", lookb))
  
  # Calculate pnls
  pnls <- shiny::reactive({
    cat("Calculating pnls\n")
    symboln <- input$symboln
    leveragep <- input$leveragep
    trend <- as.numeric(input$trend)

    # Winsorize predictor to avoid extreme positions
    # posv <- tanh(leveragep*predv())
    posv <- (leveragep*predv())
    posv <- rutils::lagit(posv, lagg=1)
    retp <- retpar()[, 1, drop=FALSE]
    pnls <- trend*posv*retp
    # Scale the PnL volatility to that of the index
    pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
    pnls <- cbind(retp, pnls)
    colnames(pnls) <- c(symboln, "Strategy")
    return(pnls)

  })  # end Calculate pnls
  

  # Plot dygraph
  dyplot <- shiny::reactive({
    cat("Plotting pnls\n")
    
    symboln <- input$symboln
    pnls <- pnls()
    colv <- colnames(pnls)

    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 3)

    # captiont <- paste("Contrarian Strategy for", symboln, "Using the Hampel Filter Over Prices")
    if (input$trend == "1") {
      captiont <- paste("Trending Strategy for", symboln, "/ \n", 
                        paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "))
    } else if (input$trend == "-1") {
      captiont <- paste("Mean Reverting Strategy for", symboln, "/ \n", 
                        paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "))
    }  # end if
    
    dygraphs::dygraph(cumsum(pnls), main=captiont) %>%
      dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
      dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
      dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red")

  })  # end reactive

  # Render the dyplot object
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph(dyplot())
    
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
