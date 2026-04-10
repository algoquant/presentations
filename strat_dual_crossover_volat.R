##############################
# This is a shiny app for simulating a dual crossover 
# volatility strategy.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Model and data setup

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

captiont <- paste("Dual Crossover EMA Volatility Strategy")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symboln
    column(width=1, selectInput("symboln", label="Symbol",
                                choices=rutils::etfenv$symbolv, selected="SPY")),
    # Input the bid-offer spread
    # column(width=2, numericInput("bid_offer", label="Bid-offer:", value=0.0000, step=0.0001))
    # Input the EMA decays
    column(width=2, sliderInput("lambdaf", label="Fast lambda:", min=0.1, max=0.99, value=0.76, step=0.01)),
    column(width=2, sliderInput("lambdas", label="Slow lambda:", min=0.1, max=0.99, value=0.92, step=0.01)),
    # Input the trade lag
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=4, value=1, step=1)),
    # Input trending or reverting (contrarian) strategy
    column(width=1, selectInput("coeff", label="Trend (1) Revert (-1)", choices=c(1, -1), selected=(-1))),
    # Input add shading Boolean
    column(width=1, selectInput("add_shading", label="Add shading?", choices=c("True", "False"), selected="False")),
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="650px")

)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()

  # Load the OHLC prices
  ohlc <- shiny::reactive({
    
    symboln <- input$symboln
    cat("Loading data for ", symboln, "\n")
    
    get(symboln, rutils::etfenv)
    
  })  # end Load the data
  
  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    cat("Recalculating strategy", "\n")
    # Get model parameters from input argument
    symboln <- input$symboln
    lambdaf <- input$lambdaf
    lambdas <- input$lambdas
    lagg <- input$lagg
    # lagg <- 1
    coeff <- as.numeric(input$coeff)

    # Calculate cumulative returns
    ohlc <- ohlc()
    nrows <- NROW(ohlc)
    pricev <- log(quantmod::Cl(ohlc))
    colnames(pricev) <- symboln
    retp <- rutils::diffit(pricev)
    retc <- cumsum(retp)
    
    # Calculate EMA returns
    # emaf <- HighFreq::run_mean(retp, lambda=lambdaf)
    # emas <- HighFreq::run_mean(retp, lambda=lambdas)
    # Calculate EMA volatilities
    emaf <- HighFreq::run_var(retp, lambda=lambdaf)[, 2]
    emas <- HighFreq::run_var(retp, lambda=lambdas)[, 2]
    # Calculate range volatilities
    # emaf <- HighFreq::run_var_ohlc(ohlc=ohlc, lambdaf=lambdaf)
    # emas <- HighFreq::run_var_ohlc(ohlc=ohlc, lambdaf=lambdas)
    
    # Determine dates when the emas have crossed
    crossi <- sign(emaf - emas)
    
    # Calculate cumulative sum of EMA crossing indicator
    crossi <- HighFreq::roll_sum(matrix(crossi), lookb=lagg)
    crossi[1:2] <- 0
    
    # Calculate the positions
    # Flip position only if the crossi and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    posv <- ifelse(crossi == lagg, 1, posv)
    posv <- ifelse(crossi == (-lagg), -1, posv)
    posv <- zoo::na.locf(posv, na.rm=FALSE)
    posv <- coeff*posv
    
    # Calculate indicator of flipped positions
    flipi <- rutils::diffit(posv)
    # Calculate number of trades
    values$ntrades <- sum(abs(flipi)>0)
    
    # Lag the positions to trade in next period
    posv <- rutils::lagit(posv, lagg=1)
    
    # Calculate strategy pnls
    pnls <- posv*retp
    
    # Calculate transaction costs
    # costs <- 0.5*input$bid_offer*abs(flipi)
    # pnls <- (pnls - costs)

    # Scale the pnls so they have same SD as the returns
    # pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
    
    # Bind together strategy pnls
    pnls <- cbind(retp, pnls)
    colnames(pnls) <- c(symboln, "Strategy")
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    pnls <- cumsum(pnls)
    
    pnls <- cbind(pnls, posv)
    colnames(pnls) <- c(symboln, "Strategy", "Positions")
    
    return(pnls)

  })  # end Recalculate the strategy
  

  # Plot the cumulative strategy pnls
  output$dyplot <- dygraphs::renderDygraph({
    
    # Get the pnls
    # cat("PLotting1", "\n")
    pnls <- pnls()
    colv <- colnames(pnls)
    
    # Get Sharpe ratios
    sharper <- values$sharper
    # Get number of trades
    ntrades <- values$ntrades
    
    captiont <- paste("Strategy for", input$symboln, "/ \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
                      "Number of trades=", ntrades)

    # Plot with shading
    add_shading <- input$add_shading
    
    if (add_shading == "True") {
      # Plot dygraph with shading
      # Create colors for background shading
      posv <- pnls[, "Positions"]
      crossd <- (rutils::diffit(posv) != 0)
      shadev <- posv[crossd]
      crossd <- c(zoo::index(shadev), end(posv))
      shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
      # Plot Optimal Dual EMA strategy
      dyplot <- dygraphs::dygraph(pnls[, 1:2], main=captiont) %>% 
        dyOptions(colors=c("blue", "red"), strokeWidth=2) %>% 
        dyLegend(show="always", width=300)
      # Add shading to dygraph object
      for (i in 1:NROW(shadev)) {
        dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
      }  # end for
      # Plot the dygraph object
      dyplot
      
    } else if (add_shading == "False") {
      # Plot dygraph without shading
      dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
        dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
        dyLegend(show="always", width=300)
    }  # end if
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
