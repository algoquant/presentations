##############################
# This is a shiny app for simulating the stop-start strategy
# for different ETFs.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)


## Set up ETF data

datenv <- rutils::etfenv
symbolv <- get("symbolv", datenv)
symboln <- "VTI"


# Define function for simulating a stop-start strategy
sim_stopstart <- function(pricev, retp, stopl) {
  maxp <- pricev[1] # Trailing maximum price
  minp <- pricev[1] # Trailing minimum price
  insl <- FALSE # Is in stop-loss?
  insg <- FALSE # Is in start-gain?
  pnls <- retp # Initialize PnLs
  for (i in 1:NROW(pricev)) {
    if (insl) { # In stop-loss
      pnls[i] <- 0 # Set PnLs = 0 if in stop-loss
      minp <- min(minp, pricev[i]) # Update minimum price to current price
      if (pricev[i] > ((1 + stopl)*minp)) { # Check for start-gain
        insg <- TRUE # Is in start-gain?
        insl <- FALSE # Is in stop-loss?
        maxp <- pricev[i] # Reset trailing maximum price
      }  # end if
    } else if (insg) { # In start-gain
      maxp <- max(maxp, pricev[i]) # Update maximum price to current price
      if (pricev[i] < ((1 - stopl)*maxp)) { # Check for stop-loss
        insl <- TRUE # Is in stop-loss?
        insg <- FALSE # Is in start-gain?
        minp <- pricev[i] # Reset trailing minimum price
      }  # end if
    } else { # Warmup period
      # Update the maximum and minimum prices
      maxp <- max(maxp, pricev[i])
      minp <- min(minp, pricev[i])
      # Update the stop-loss and start-gain indicators
      insl <- (pricev[i] < ((1 - stopl)*maxp)) # Is in stop-loss?
      insg <- (pricev[i] > ((1 + stopl)*minp)) # Is in start-gain?
    }  # end if
  }  # end for
  return(pnls)
} # end sim_stopstart

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Stop-Start Strategy"),
  
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=1, selectInput("symboln", label="Symbol",
                                choices=symbolv, selected=symboln)),
    # Input stop-loss level
    column(width=2, sliderInput("stopl", label="Stop-loss level",
                                min=0.01, max=0.3, value=0.1, step=0.01))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="80%", height="600px")

)  # end fluidPage interface


## Define the server function
servfun <- shiny::shinyServer(function(input, output) {
  
  # Get the close prices in a reactive environment
  pricev <- shiny::reactive({
    cat("Getting the close prices\n")
    # Get the OHLC prices
    ohlc <- get(input$symboln, datenv)
    # Return the close prices
    quantmod::Cl(ohlc)
  })  # end reactive code
  
  # Simulate the stop-start strategy in a reactive environment
  wealthv <- shiny::reactive({
    cat("Simulating the stop-start strategy\n")
    pricev <- pricev()
    retp <- rutils::diffit(log(pricev))

    pnls <- sim_stopstart(pricev, retp, input$stopl)

    # Combine the data
    wealthv <- cbind(retp, pnls)
    colnames(wealthv) <- c(input$symboln, "Strategy")
    wealthv
    
  })  # end reactive code
  
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    cat("Plotting the dygraph\n")
    
    wealthv <- wealthv()
    colnamev <- colnames(wealthv)
    nrows <- NROW(wealthv)
    datev <- zoo::index(wealthv)
    pnls <- wealthv[, "Strategy"]
    
    # Annualized Sharpe ratios of crossover strategies
    sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
    captiont <- paste("Stop-Loss =", input$stopl, "/ Sharpe",
                      paste(paste(names(sharper), round(sharper, 3), sep="="), collapse=", "))

    # Plot dygraph with shading
    # Create colors for background shading
    insl <- (pnls == 0) # Is in stop-loss?
    indic <- (rutils::diffit(insl) != 0) # Indices of crosses
    crossd <- c(datev[indic], datev[nrows]) # Dates of crosses
    shadev <- ifelse(insl[indic] == 1, "antiquewhite", "lightgreen")
    # Create dygraph object without plotting it
    dyplot <- dygraphs::dygraph(cumsum(wealthv), main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
      dyLegend(show="always", width=200)
    # Add shading to dygraph object
    for (i in 1:NROW(shadev)) {
      dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
    }  # end for
    # Plot the dygraph object
    dyplot

  })  # end output plot
})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
