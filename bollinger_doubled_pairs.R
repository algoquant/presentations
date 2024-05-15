##############################
# This is a shiny app for cointegration.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

symbolv <- c("AAPL", "MSFT", "GOOG", "MSFT", "SPY", "QQQ", "USO", "XLK", "VXX", "SVXY", "TLT")

filex <- "_second_202401.RData"

captiont <- "Bollinger Double Down Pair Strategy"

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  fluidRow(
  # Input stock symbol
  column(width=2, selectInput("stock1", label="Stock 1:", choices=symbolv, selected="AAPL")),
  # Input ETF symbol
  column(width=2, selectInput("stock2", label="Stock 2:", choices=symbolv, selected="SPY"))
  ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input stock beta
    column(width=2, sliderInput("betav", label="Beta", min=0.0, max=15.0, value=0.6, step=0.1)),
    # Input the EMA decay factor and z-scores thresholds
    column(width=2, sliderInput("lambdaf", label="Lambda decay:", min=0.8, max=0.99, value=0.96, step=0.01)),
    column(width=2, sliderInput("volf", label="Volatility floor:", min=0.1, max=0.3, value=0.2, step=0.01)),
    column(width=2, sliderInput("threshv", label="Threshold:", min=0.5, max=3.0, value=1.0, step=0.1)),
    column(width=2, sliderInput("threshd", label="Double down:", min=0.5, max=5.0, value=3.0, step=0.1)),
    column(width=2, sliderInput("maxd", label="Max double down:", min=1, max=4, value=2, step=1))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="80%", height="600px")
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Load the prices
  price1 <- shiny::reactive({
    
    cat("Loading the prices for stock1", "\n")

    # Load the list of intraday prices for stock1
    filen <- paste0("/Users/jerzy/Develop/data/", input$stock1, filex)
    load(filen)
    
    # Sample prices to minutes
    # pricel <- lapply(pricel, function(pricev) {
    #   pricev <- pricev[, 1]
    #   pricev <- quantmod::Cl(xts::to.minutes(pricev))
    #   # colnames(pricev) <- symboln
    #   pricev
    # }) # end lapply
    pricel

  })  # end reactive code
  
  
  # Load the prices
  price2 <- shiny::reactive({
    
    cat("Loading the prices for stock2", "\n")
    
    # Load the list of intraday prices for stock2
    filen <- paste0("/Users/jerzy/Develop/data/", input$stock2, filex)
    load(filen)
    
    # Sample prices to minutes
    # pricel <- lapply(pricel, function(pricev) {
    #   pricev <- pricev[, 1]
    #   pricev <- quantmod::Cl(xts::to.minutes(pricev))
    #   # colnames(pricev) <- symboln
    #   pricev
    # }) # end lapply
    pricel
    
  })  # end reactive code
  
  
  # Calculate the PnLs
  pnls <- shiny::reactive({

    price1 <- price1()
    price2 <- price2()
    
    # Align the two lists of stock prices
    # Calculate the Boolean vectors if the number of rows of stock prices is greater than 1
    nrows1 <- (sapply(price1, dim)[1, ] > 1)
    nrows2 <- (sapply(price2, dim)[1, ] > 1)
    
    if (sum(nrows1) > sum(nrows2)) {
      price1 <- price1[nrows2]
      price2 <- price2[nrows2]
    } else if (sum(nrows2) > sum(nrows1)) {
      price1 <- price1[nrows1]
      price2 <- price2[nrows1]
    }  # end if
    
    # Calculate the intraday PnLs in a loop
    pnls <- lapply(seq_along(price1), function(it) {
      # cat("it =", it, "\n")
      price1 <- price1[[it]][, 1]
      price2 <- price2[[it]][, 1]
      pricev <- (price1 - input$betav*price2)
      # stratm <- revert_to_open(pricev)
      stratm <- bollinger_double(pricev, input$lambdaf, input$threshv, input$threshd, input$maxd, input$volf)
      cbind(rutils::diffit(pricev[, 1]), stratm[, 1])
    }) # end lapply
    
    pnls <- do.call(rbind, pnls)
    colnames(pnls) <- c("Pair", "Strategy")
    pnls
    
  })  # end Load the data
  

  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    pnls <- pnls()
    colnamev <- colnames(pnls)
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 3)
    pnls <- cumsum(pnls)
    captiont <- paste0(c("Pair SR=", "Strategy SR="), sharper, collapse=" / ")
    
    dygraphs::dygraph(pnls, main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
      dySeries(axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
      dyLegend(show="always", width=300)
    
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
