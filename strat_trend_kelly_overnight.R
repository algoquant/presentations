##############################
# This is a shiny app for simulating a trending Kelly strategy 
# using overnight ETF returns and range volatility.
#
# Just press the "Run App" button on upper right of this panel.
##############################


## Model and data setup
## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Calculate the range of daily prices for SPY
symbolv <- rutils::etfenv$symbolv
symboln <- "SPY"
volt <- 0.01 ##  Volatility target for scaling the strategy PnLs

stringv <- "Overnight"
captiont <- paste("Trending Kelly Overnight Strategy")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    ##  Input stock symboln
    column(width=1, selectInput("symboln", label="Symbol", choices=symbolv, selected="SPY")),
    ##  Input lambda decay parameter
    column(width=2, sliderInput("lambdaf", label="Lambda", min=0.9, max=0.999, value=0.99, step=0.001)),
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

    ohlc <- log(get(symboln, rutils::etfenv))
    return(ohlc)
    
  })  ##  end Load the data
  
  ##  Calculate the returns
  rets <- shiny::reactive({
    
    # symboln <- input$symboln
    cat("Recalculating returns\n")
    
    ohlc <- ohlc()
    openp <- quantmod::Op(ohlc)
    closep <- quantmod::Cl(ohlc)
    retp <- rutils::diffit(closep) ##  daily returns
    colnames(retp) <- "daily"
    retd <- (closep - openp) ##  daytime returns
    colnames(retd) <- "daytime"
    reton <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE)) ##  overnight returns
    colnames(reton) <- "overnight"
    # highp <- quantmod::Hi(ohlc)
    # lowp <- quantmod::Lo(ohlc)
    # hilo <- (highp - lowp) ##  range of daily prices
    
    return(cbind(retp, retd, reton))
    
  })  ##  end Calculate the returns
  
  ##  Calculate the EMA overnight returns
  retm <- shiny::reactive({
    
    # symboln <- input$symboln
    cat("Recalculating EMA returns\n")
    lambdaf <- input$lambdaf
    
    rets <- rets()
    reton <- rets[, "overnight"]
    retm <- HighFreq::run_mean(reton, lambda=lambdaf)

    return(retm)
    
  })  ##  end Calculate the EMA returns
  
  ##  Calculate the range variance
  varv <- shiny::reactive({
    
    ##  Get model parameters from input argument
    # symboln <- input$symboln
    cat("Recalculating variance\n")
    lambdaf <- input$lambdaf
    
    ##  Calculate the range variance
    ohlc <- ohlc()
    varv <- HighFreq::run_var_ohlc(ohlc, lambda=lambdaf)
    # varv[1:2] <- 1
    return(varv)
    
  })  ##  end Calculate the range variance
  
  ##  Recalculate the strategy
  pnls <- shiny::reactive({
    
    symboln <- input$symboln
    cat("Recalculating PnLs for", symboln, "\n")

    ##  Calculate the strategy PnLs
    varv <- varv()
    ##  Scale the returns by the range variance
    rets <- rets()
    reton <- rets[, "overnight"]
    # retp <- rets[, "daily"]
    retm <- retm()
    posv <- sign(retm)*volt^2/varv
    posv <- rutils::lagit(posv, lagg=1)
    pnls <- posv*reton

    ##  Bind together strategy pnls
    pnls <- cbind(reton, pnls)
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
    captiont <- paste0(c(paste0(stringv, " SR="), "Trending SR="), sharper, collapse=" / ")
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
