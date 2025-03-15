##############################
# This is a shiny app for calculating the average rescaled 
# range of daily prices for a pair of stocks, as a function 
# of the beta weight parameter.

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

# Load the intraday minute prices
load(paste0("/Users/jerzy/Develop/data/SPY_minute_202425.RData"))
pricetarg <- pricel
load(paste0("/Users/jerzy/Develop/data/XLK_minute_202425.RData"))
priceref <- pricel

priceref <- do.call(rbind, priceref)
pricetarg <- do.call(rbind, pricetarg)

# Calculate the symbol names
symboltarg <- rutils::get_name(colnames(pricetarg))
symbolref <- rutils::get_name(colnames(priceref))
symbolpair <- paste0(symboltarg, "/", symbolref)
captiont <- paste0("Range For ", symbolpair)

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input the beta parameter
    column(width=2, sliderInput("betac", label="beta:", min=0.0, max=3.0, value=2.0, step=0.1)),
  ),  # end fluidRow

  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()

  # Recalculate the range
  pricev <- shiny::reactive({
    
    cat("Recalculating the range for", symbolpair, "\n")

    # Calculate the pair prices
    pricev <- pricetarg - input$betac*priceref
    colnames(pricev) <- "Pair"
    # Calculate the range of daily prices for VTI
    ohlc <- xts::to.daily(pricev)
    openp <- quantmod::Op(ohlc)
    closep <- quantmod::Cl(ohlc)
    retd <- (closep - openp)
    colnames(retd) <- "daytime"
    highp <- quantmod::Hi(ohlc)
    lowp <- quantmod::Lo(ohlc)
    hilo <- highp - lowp
    # Actual value of the rescaled range is:
    rangev <- mean(hilo)/sd(retd)
    values$rangev <- round(rangev, 3)
    # cat("range =", rangev, "\n")
    
    pricev

  })  # end Recalculate the range
  

  # Plot the pair
  output$dyplot <- dygraphs::renderDygraph({
    
    # Get the PnLs
    pricev <- pricev()
    colnamev <- colnames(pricev)
    
    # Get the range
    rangev <- values$rangev
    # cat("range =", rangev, "\n")
    captiont <- paste0(captiont, " / range = ", rangev)
    # Return to the output argument a dygraph plot
    dygraphs::dygraph(pricev, main=captiont)
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
