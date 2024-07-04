##############################
# This is a shiny app for simulating a contrarian strategy using 
# the Hampel filter over prices.
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
symbolv <- c("SPY", rutils::etfenv$symbolv)
symbol <- "VTI"

captiont <- paste("Hampel Filter Strategy Over Prices")
# captiont <- paste("Contrarian Strategy for", symbol, "Using the Hampel Filter Over Prices")

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
  
  # Create single row with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol", choices=symbolv, selected=symbol)),
    # Input short look-back interval
    column(width=2, sliderInput("lookb", label="Lookback", min=3, max=40, value=3, step=1)),
    # Input threshold level
    column(width=2, sliderInput("threshold", label="threshold", min=0.5, max=3.0, value=1.0, step=0.1)),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False"))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Load data
  ohlc <- shiny::reactive({
    cat("Loading data\n")
    symbol <- input$symbol
    # load(file="/Users/jerzy/Develop/data/AAPL_minute.RData")
    # ohlc
    if (symbol == "SPY")
      HighFreq::SPY#["2008/2009"]
    else
      get(symbol, envir=rutils::etfenv)
  })  # end reactive
  
  # Calculate log returns
  retv <- shiny::reactive({
    cat("Calculating log returns\n")
    rutils::diffit(log(Cl(ohlc())))
  })  # end reactive
  
  # Calculate zscores if the lookb is updated
  zscores <- shiny::reactive({
    cat("Calculating zscores\n")
    lookb <- input$lookb
    # long_back <- input$long_back
    pricev <- quantmod::Cl(ohlc())
    medianv <- roll::roll_median(pricev, width=lookb)
    madv <- HighFreq::roll_var(pricev, lookb=lookb, method="nonparametric")
    zscores <- ifelse(madv > 0, (pricev - medianv)/madv, 0)
    zscores[1:lookb, ] <- 0
    zscores
  })  # end reactive
  

  # Calculate positions and pnls if there's new threshold value
  pnls <- shiny::reactive({
    cat("Calculating positions and pnls\n")
    threshold <- input$threshold
    # retv <- rutils::diffit(pricev())
    retv <- retv()
    zscores <- zscores()
    nrows <- NROW(retv)
    positv <- rep(NA_integer_, nrows)
    positv[1] <- 0
    positv[zscores < -threshold] <- 1
    positv[zscores > threshold] <- (-1)
    positv <- zoo::na.locf(positv)
    positv <- rutils::lagit(positv)

    # Number of trades
    values$ntrades <- sum(abs(rutils::diffit(positv)))/2
    
    pnls <- cbind(retv, positv*retv)
    colnames(pnls) <- c(input$symbol, "Strategy")
    
    # Sharpe
    sharper <- sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sqrt(252)*sharper, 3)

    # Cumsum
    pnls <- cumsum(pnls)
    retsum <- pnls[, 2]
    
    # Add buy/sell indicators
    indic <- rutils::diffit(positv)
    longi <- (indic > 0)
    shorti <- (indic < 0)
    
    pnls <- cbind(pnls, retsum[longi], retsum[shorti])
    colnames(pnls)[3:4] <- c("Buy", "Sell")
    pnls
    # list(caption=captiont, pnls=pnls)
  })  # end reactive
  
  
  # Plot dygraph of pnls if the add_annotations variable is updated
  dyplot <- shiny::reactive({
    cat("Plotting pnls\n")
    add_annotations <- input$add_annotations
    # captiont <- pnls()$caption
    pnls <- pnls()
    colnamev <- colnames(pnls)
    # cat(paste("colnamev\n", colnamev, "\n"))
    # cat(paste("pnls\n", tail(pnls), "\n"))
    
    captiont <- paste(paste0(paste(colnamev[1:2], "Sharpe =", values$sharper), collapse=" / "), "/ \n",
                      "Number of trades=", values$ntrades)
    
    if (inherits(end(pnls), "POSIXct")) 
      endd <- rutils::calc_endpoints(pnls, interval="days")
    else if (inherits(end(pnls), "Date"))
      endd <- rutils::calc_endpoints(pnls, interval="weeks")
    
    if (add_annotations == "True") {
      # Create a dygraph object with annotations (no plot is created)
      dygraphs::dygraph(pnls, main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
        # Add second y-axis
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red") %>%
        dySeries(name=colnamev[3], axis="y2", drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=colnamev[4], axis="y2", drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == "False") {
      # Create a dygraph object without annotations (no plot is created)
      dygraphs::dygraph(pnls[endd, 1:2], main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        # Add second y-axis
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red")
    }  # end if
    
  })  # end reactive
  
  # Render (plot) the dygraph object and return it to the output argument
  output$dyplot <- dygraphs::renderDygraph(dyplot())
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
