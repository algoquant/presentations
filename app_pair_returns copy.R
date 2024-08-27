##############################
# This is a shiny app which plots the daily, daytime, and 
# overnight returns of a stock pair with a fixed beta.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs only once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Load daily S&P500 stock prices and returns
# load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# symetfs <- sort(colnames(pricestock))
# symstock <- "AAPL"
# 
# symbolsetf <- colnames(rutils::etfenv$prices)
# symetf <- "XLK"


symetfs <- rutils::etfenv$symbolv
symstocks <- symetfs


symstock <- "VTI"
symetf <- "VXX"

# captiont <- paste("Stat-arb Portfolio Strategy app_statarb_strat.R")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Returns of a Stock Pair With Fixed Beta"),
  
  # create single row with four slider inputs
  fluidRow(
    # Input stock symbol
    column(width=1, selectInput("symstock", label="Stock", choices=symstocks, selected=symstock)),
    # Input ETF symbol
    column(width=1, selectInput("symetf", label="ETF", choices=symetfs, selected=symetf)),
    # Input beta parameter
    column(width=2, sliderInput("betac", label="beta:", min=0.0, max=2.0, value=1.0, step=0.1)),
  ),  # end fluidRow
  
  # create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  # mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface


## Define the server code
servfun <- shiny::shinyServer(function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Rerun the strategy
  retv <- shiny::reactive({
    
    cat("Recalculating strategy", "\n")
    # Get model parameters from input argument
    symstock <- input$symstock
    symetf <- input$symetf
    # lagg <- input$lagg
    # coeff <- as.numeric(input$coeff)

    # Get the stock prices for symstock
    ohlc <- get(symstock, rutils::etfenv)
    openp <- log(quantmod::Op(ohlc))
    closep <- log(quantmod::Cl(ohlc))
    ret1 <- rutils::diffit(closep) # Daily returns
    retd1 <- (closep - openp) # Daytime returns
    colnames(retd1) <- "daytime1"
    reton1 <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE)) # Overnight returns
    colnames(reton1) <- "overnight1"
    
    # Get the stock prices for symetf
    ohlc <- get(symetf, rutils::etfenv)
    openp <- log(quantmod::Op(ohlc))
    closep <- log(quantmod::Cl(ohlc))
    ret2 <- rutils::diffit(closep) # Daily returns
    retd2 <- (closep - openp) # Daytime returns
    colnames(retd2) <- "daytime2"
    reton2 <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE)) # Overnight returns
    colnames(reton2) <- "overnight2"
    
    # retv <- na.omit(reton1 - input$betac*reton2)
    # retv <- retd1 - input$betac*retd2
    retv <- ret1 + input$betac*ret2
    colnames(retv) <- "pair"
    sharper <- sqrt(252)*mean(retv)/sd(retv[retv<0])
    values$sharper <- round(sharper, 3)
    retv

  })  # end reactive code
  
  
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    
    # symstock <- "XLK"
    # symetf <- "VTI"
    retv <- retv()
    colnamev <- colnames(retv)
    
    captiont <- paste("Sharpe =", values$sharper)

    dygraphs::dygraph(cumsum(retv), main=captiont) %>%
      dyOptions(colors="blue", strokeWidth=2) %>%
      dyLegend(show="never", width=300)
    
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
