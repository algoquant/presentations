##############################
# This is a shiny app for simulating a mean reverting 
# strategy for stocks.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# load packages
library(shiny)
library(dygraphs)
library(rutils)

# Load the SP500 OHLC prices
if (!exists("retstock")) {
  cat("Loading the S&P500 returns.\n")
  load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
} # end if
symbolv <- sort(colnames(retstock))

symboln <- "AAPL"
captiont <- "Mean Reverting Strategy"

## End setup code



## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  # create single row with four slider inputs
  fluidRow(
    # Input stock symbol
    column(width=1, selectInput("symboln", label="Symbol", choices=symbolv, selected=symboln)),
    # Input look-back interval
    column(width=2, sliderInput("lambdaf", label="Lambda decay factor",
                                min=0.1, max=0.99, value=0.5, step=0.1))
  ),  # end fluidRow
  
  # create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  ##  Create an empty list of reactive values.
  values <- reactiveValues()
  
  ##  Recalculate the strategy
  pnls <- shiny::reactive({
    
    symboln <- input$symboln
    cat("Recalculating PnLs for", symboln, "\n")
    lambdaf <- input$lambdaf
    
    retp <- na.omit(get(symboln, retstock))
    retp <- retp[!(retp==0)]
    retm <- HighFreq::run_mean(retp, lambda=lambdaf)
    posv <- -sign(retm)
    posv <- rutils::lagit(posv, lagg=1)
    pnls <- posv*retp
    
    ##  Bind together strategy pnls
    pnls <- cbind(retp, pnls, (retp + pnls)/2)
    # pnls <- cbind(cumsum(retp), probv)
    colnames(pnls) <- c(symboln, "Strategy", "Combined")
    
    ##  Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    names(sharper) <- colnames(pnls)
    values$sharper <- round(sharper, 3)
    
    pnls <- cumsum(pnls)
    return(pnls)
    
  })  ##  end Recalculate the strategy
  
  
  ##  Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    ##  Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    ##  Get Sharpe ratios
    sharper <- values$sharper
    
    ##  Create the caption with Sharpe ratios
    captiont <- paste0(paste0(names(sharper), " SR=", sharper), collapse=" / ")
    
    ##  Plot dygraph of the cumulative PnLs
    dyplot <- dygraphs::dygraph(pnls, main=captiont) %>%
      dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
      dyLegend(show="always", width=300)
    
    ##  Return the dygraph object
    return(dyplot)
  })  ##  end output plot
  
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
