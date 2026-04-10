##############################
# This is a shiny app for simulating a volatility timing strategy 
# for overnight stock returns, using range volatility.
#
# Just press the "Run App" button on upper right of this panel.
##############################


## Model and data setup
## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Load the SP500 OHLC prices
if (!exists("sp500env")) {
  cat("Loading the S&P500 OHLC prices.\n")
  load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
} # end if
symbolv <- sort(names(sp500env))


# Calculate the range of daily prices for SPY
symboln <- "NVDA"
volt <- 0.01 ##  Volatility target for scaling the strategy PnLs

stringv <- "Overnight"
captiont <- paste("Stocks Volatility Timing Strategy")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    ##  Input stock symboln
    column(width=1, selectInput("symboln", label="Symbol", choices=symbolv, selected=symboln)),
    ##  Input lambda decay parameter
    column(width=2, sliderInput("lambdaf", label="Lambda", min=0.1, max=0.99, value=0.97, step=0.01)),
  ),  ##  end fluidRow

  ##  Render the plot in a new row
  fluidRow(
    dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  ),  ##  end fluidRow
  
)  ##  end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  ##  Create an empty list of reactive values.
  values <- reactiveValues()

  ##  Load the OHLC prices
  ohlc <- shiny::reactive({
    
    symboln <- input$symboln
    cat("Loading data for", symboln, "\n")

    ohlc <- log(get(symboln, sp500env))
    return(ohlc)
    
  })  ##  end Load the data
  
  ##  Calculate the returns
  retp <- shiny::reactive({
    
    # symboln <- input$symboln
    cat("Recalculating returns\n")
    
    ohlc <- ohlc()
    openp <- quantmod::Op(ohlc)
    closep <- quantmod::Cl(ohlc)
    # retp <- rutils::diffit(closep) ##  daily returns
    # colnames(retp) <- "daily"
    # retd <- (closep - openp) ##  daytime returns
    # colnames(retd) <- "daytime"
    reton <- (openp - rutils::lagit(closep, lagg=1)) ##  overnight returns
    colnames(reton) <- "overnight"
    reton[1] <- 0
    # highp <- quantmod::Hi(ohlc)
    # lowp <- quantmod::Lo(ohlc)
    # hilo <- (highp - lowp) ##  range of daily prices
    
    return(reton)
    
  })  ##  end Calculate the returns
  
  ##  Calculate the range variance estimator
  varv <- shiny::reactive({
    
    ##  Get model parameters from input argument
    # symboln <- input$symboln
    cat("Recalculating variance\n")
    lambdaf <- input$lambdaf
    
    ##  Calculate the range variance estimator
    ohlc <- ohlc()
    varv <- HighFreq::run_var_ohlc(ohlc, lambda=lambdaf)
    varv <- rutils::lagit(varv, lagg=1)
    varv[1:3] <- 1
    return(varv)
    
  })  ##  end Calculate the range variance estimator
  
  ##  Recalculate the strategy
  pnls <- shiny::reactive({
    
    symboln <- input$symboln
    cat("Recalculating PnLs for", symboln, "\n")

    ##  Calculate the strategy PnLs
    varv <- varv()
    ##  Scale the returns by the range variance
    retp <- retp()
    pnls <- retp*volt^2/varv
    pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
    
    ##  Bind together strategy pnls
    pnls <- cbind(retp, pnls)
    colnames(pnls) <- c(symboln, "Strategy")
    
    ##  Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    
    return(cumsum(pnls))

  })  ##  end Recalculate the strategy
  

  ##  Plot the cumulative scaled returns
  ##  Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    ##  Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    ##  Get Sharpe ratios
    sharper <- values$sharper

    ##  Standard plot without shading
    captiont <- paste0(c(paste0(stringv, " SR="), "Timing SR="), sharper, collapse=" / ")
    ##  Plot dygraph without shading
    dyplot <- dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
      dyLegend(show="always", width=200)
    
    ##  Plot the dygraph object
    return(dyplot)


  })  ##  end output plot

}  ##  end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
