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
# pricerefs <- sort(colnames(pricestock))
# pricetarg <- "AAPL"
# 
# symbolsetf <- colnames(rutils::etfenv$prices)
# priceref <- "XLK"

symboltarg <- "SVXY"
symbolref <- "VTI"

ohlctarg <- log(get(symboltarg, rutils::etfenv))
datev <- zoo::index(ohlctarg)
ohlcref <- log(get(symbolref, rutils::etfenv))
ohlcref <- ohlcref[datev]

opentarg <- quantmod::Op(ohlctarg)
closetarg <- quantmod::Cl(ohlctarg)
retontarg <- (opentarg - rutils::lagit(closetarg, lagg=1, pad_zeros=FALSE))
retdtarg <- (closetarg - opentarg)
retdtarg <- rutils::lagit(retdtarg)

openref <- quantmod::Op(ohlcref)
closeref <- quantmod::Cl(ohlcref)
retonref <- (openref - rutils::lagit(closeref, lagg=1, pad_zeros=FALSE))
retdref <- (closeref - openref)
retdref <- rutils::lagit(retdref)

nrows <- NROW(ohlcref)
symbolpair <- paste0(symboltarg, "/", symbolref)
captiont <- paste0("Ratchet Strategy For ", symbolpair)


# pricerefs <- rutils::etfenv$symbolv
# pricetargs <- pricerefs


# captiont <- paste("Stat-arb Portfolio Strategy app_statarb_strat.R")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Returns of a Stock Pair With Fixed Beta"),
  
  # create single row with four slider inputs
  fluidRow(
    # Input stock symbol
    # column(width=1, selectInput("pricetarg", label="Stock", choices=pricetargs, selected=pricetarg)),
    # Input ETF symbol
    # column(width=1, selectInput("priceref", label="ETF", choices=pricerefs, selected=priceref)),
    # Input beta parameter
    column(width=2, sliderInput("betac", label="beta:", min=0.0, max=2.0, value=1.0, step=0.1)),
  ),  # end fluidRow
  
  # create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  # mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface


## Define the server code
servfun <- shiny::shinyServer(function(input, output) {

  # Rerun the strategy
  pnls <- shiny::reactive({
    
    cat("Recalculating strategy", "\n")
    # Get model parameters from input argument
    # lagg <- input$lagg
    # coeff <- as.numeric(input$coeff)
    reton <- retontarg - input$betac*retonref
    retd <- retdtarg - input$betac*retdref

    # Trade the overnight returns based on the lagged daytime returns
    pnls <- (reton*sign(retd))
    colnames(pnls) <- c("Strategy")
    pnls
    # pnls <- pnls*sd(reton[reton<0])/sd(pnls[pnls<0])

  })  # end reactive code
  
  
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    
    pnls <- pnls()
    # colnamev <- colnames(pricev)
    
    dygraphs::dygraph(cumsum(pnls), main=captiont) %>%
      dyOptions(colors="blue", strokeWidth=1) %>%
      dyLegend(show="always", width=300)
    
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
