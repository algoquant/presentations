##############################
# This is a shiny app for illustrating the Bollinger Bands strategy.
# The stock position is equal to minus the sign of the excess price.
# If the excess price is positive then the position is -$1 of stock, 
# and vice versa.
# Version for dual EMA:
#   The excess price is equal to the fast EMA price minus the slow EMA.
#   The model depends on two lambda decay parameters used to calculate the 
#   trailing average prices - fast lambda (small) and slow lambda (large).
# Version for single EMA:
#   The excess price is equal to the current price minus the trailing mean price.
#   The model depends on a single lambda decay parameter used to calculate 
#   the trailing average prices.
# 
# Just press the "Run App" button on the upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Load daily ETF prices

envv <- rutils::etfenv
symbolv <- sort(get("symbolv", envv))
symboln <- "QQQ"

# Date range
dater <- "2019-01/"
trendind <- 1
lagg <- 1

captiont <- paste("Bollinger Bands")


## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  # create single row with four slider inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symboln", label="Symbol", choices=symbolv, selected=symboln)),
    # column(width=2, sliderInput("lambdaf", label="lambda fast:", min=0.5, max=0.99, value=0.9, step=0.01)),
    column(width=2, sliderInput("lambdaf", label="Decay factor:", min=0.1, max=0.99, value=0.98, step=0.01)),
    # Input lag parameter
    # column(width=1, sliderInput("lagg", label="lag", min=1, max=3, value=1, step=1)),
    # Input trending or reverting (contrarian) strategy
    column(width=1, selectInput("trendind", label="Trend (1) Revert (-1)", choices=c(1, -1), selected=(1))),
    # Input the Bid-ask spread
    # column(width=1, numericInput("bidask", label="Bid-ask:", value=0.0, step=0.001))
  ),  # end fluidRow
  
  # create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="700px")
  # mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface


## Define the server code
servfun <- shiny::shinyServer(function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Get the close prices in a reactive environment
  pricev <- shiny::reactive({
    cat("Getting the close prices\n")
    # Get the data
    symboln <- input$symboln
    ohlc <- get(symboln, envv)
    pricev <- log(quantmod::Cl(ohlc))
    colnames(pricev) <- symboln
    # Return the prices
    return(pricev)
  })  # end reactive code
  
  # Recalculate the Bollinger Bands prices
  priceb <- shiny::reactive({
    
    cat("Recalculating the EMA prices", "\n")
    pricev <- pricev()
    volp <- HighFreq::run_var(pricev, lambda=input$lambdaf)
    pricema <- volp[, 1]
    volp <- sqrt(volp[, 2])
    priceb <- cbind(pricema, pricema+volp, pricema-volp)
    colnames(priceb) <- c("mean", "upper", "lower")
    return(priceb)
    
  })  # end reactive code
  

  # Rerun the strategy
  wealthv <- shiny::reactive({
    cat("Recalculating the strategy", "\n")
    # Get model parameters from input argument
    # symboln <- input$symboln
    trendind <- as.numeric(input$trendind)
    pricev <- pricev()
    priceb <- priceb()
    pricem <- priceb[, "mean"]
    pricup <- priceb[, "upper"]
    pricelo <- priceb[, "lower"]
    nrows <- NROW(pricev)

    # Calculate the positions
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0 ##  Initial position
    posv <- ifelse(pricev > pricup, trendind, posv)
    posv <- ifelse(pricev < pricelo, -trendind, posv)
    posv <- zoo::na.locf(posv)
    posv <- rutils::lagit(posv, lagg=lagg)
    # posv <- trendind*posv
    
    # Calculate the number of trades
    flipi <- rutils::diffit(posv)
    values$ntrades <- sum(abs(flipi) > 0)

    # Calculate the PnLs
    # reti <- (retp$Stock - rutils::lagit(betas)*retp$ETF)
    # pnls <- retp*posv
    # Calculate the transaction costs
    # costs <- 0.5*input$bidask*flipi
    # pnls <- (pnls - costs)
    
    # pnls <- pnls*sd(retp)/sd(pnls)
    wealthv <- cbind(pricev, priceb, posv)
    colnames(wealthv)[NCOL(wealthv)] <- "Position"
    
    # Calculate Sharpe ratios
    # sharper <- sqrt(252)*sapply(wealthv[, 1:2], function(x) mean(x)/sd(x[x<0]))
    # values$sharper <- round(sharper, 3)
    
    return(wealthv)
    
  })  # end reactive code
  
  
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    
    # symboln <- input$symboln
    # symboletf <- input$symboletf
    wealthv <- wealthv()[dater]
    colnamev <- colnames(wealthv)
    
    # captiont <- paste(paste0(colnamev[1:2], collapse=" / "), "/ \n",
    #                   "Number of trades=", values$ntrades)
    
    ##  Create dygraph plot
    # endd <- rutils::calc_endpoints(wealthv, interval="weeks")
    # dygraphs::dygraph(cumsum(wealthv), main=captiont) %>%
    # dyplot <- dygraphs::dygraph(cumsum(wealthv[, 1:2]), main=captiont) %>%
    #   dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
    #   dyLegend(show="always", width=300)
    
    ##  Create dygraph with shading
    # Create colors for background shading
    posv <- sign(wealthv[, "Position"])
    datev <- zoo::index(wealthv)
    nrows <- NROW(wealthv)
    indic <- (rutils::diffit(posv) != 0) # Indices of crosses
    crossd <- c(datev[indic], datev[nrows]) # Dates of crosses
    shadev <- ifelse(posv[indic] == -1, "antiquewhite", "lightgreen")
    # Create dygraph object without plotting it
    colv <- colnames(wealthv)
    dyplot <- dygraphs::dygraph(wealthv[, -NCOL(wealthv)], main=NA) %>%
      dySeries(name=colv[1], strokeWidth=1, col="blue") %>%
      dySeries(name=colv[2], strokeWidth=1, col="purple") %>%
      dySeries(name=colv[3], strokeWidth=2, strokePattern="dashed", col="green") %>%
      dySeries(name=colv[4], strokeWidth=2, strokePattern="dashed", col="red") %>%
      dyLegend(show="always", width=400)
    # Add shading to dygraph object
    for (i in 1:NROW(shadev)) {
      dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
    }  # end for
    
    ##  Return the dygraph object
    return(dyplot)
    
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
