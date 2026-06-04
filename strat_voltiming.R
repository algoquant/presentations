##############################
# This is a shiny app for simulating a volatility timing 
# strategy for either daily or overnight stock returns. 
# The positions are equal to the inverse of the volatility 
# or the variance, depending on the user selection.
# The user can select variance estimators based on either 
# returns, downside returns, or range-based estimators.
#
# Just press the "Run App" button on upper right of this panel.
##############################


## Model and data setup
## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

captiont <- paste("Volatility Timing Strategy")

# Uncomment the following lines to load the S&P500 OHLC prices
# if (!("sp500env" %in% ls())) {
#   cat("Loading the S&P500 OHLC prices.\n")
#   load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
# } # end if
# envv <- sp500env
# symboln <- "NVDA"

# Uncomment the following lines to load the ETF OHLC prices.
if (!("etfenv" %in% ls())) {
  cat("Loading the ETF OHLC prices.\n")
  load("/Users/jerzy/Develop/data/etf_ohlc.RData")
} # end if
envv <- etfenv
symboln <- "QQQ"

# Vector of stock symbols in the environment
symbolv <- sort(names(envv))

volt <- 0.01 ##  Volatility target for scaling the strategy PnLs
varfloor <- 1e-6 ##  Variance floor to prevent division by zero in Kelly ratio calculations


## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    ##  Input stock symboln
    column(width=1, selectInput("symboln", label="Symbol", choices=symbolv, selected=symboln)),
    ##  Input return type
    column(width=1, selectInput("return_type", label="Returns", 
      choices=c("Daily"="daily", "Overnight"="overnight"), selected="daily")),
    ##  Input variance type
    column(width=1, selectInput("variance_type", label="Variance", 
      choices=c("EMA"="ema", "Downside"="downside", "Range"="range"), selected="range")),
    ##  Input position scaling method
    column(width=1, selectInput("scaling_method", label="Scaling", 
      choices=c("Volatility"="inverse_volatility", "Variance"="inverse_variance"), selected="inverse_volatility")),
    ##  Input lambda decay parameter
    column(width=2, sliderInput("lambdaf", label="Decay factor", min=0.1, max=0.99, value=0.9, step=0.01)),
  ),  ##  end fluidRow

  ##  Render the plot in a new row
  fluidRow(
    dygraphs::dygraphOutput("dyplot", width="90%", height="700px")
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

    ohlc <- log(get(symboln, envv))
    return(ohlc)
    
  })  ##  end Load the data
  
  ##  Calculate the returns
  retp <- shiny::reactive({
    
    # symboln <- input$symboln
    cat("Recalculating returns\n")
    return_type <- input$return_type
    
    ohlc <- ohlc()
    openp <- quantmod::Op(ohlc)
    closep <- quantmod::Cl(ohlc)
    if (return_type == "overnight") {
      retp <- openp - rutils::lagit(closep, lagg=1) ##  overnight returns
      retp[1] <- 0
      colnames(retp) <- "overnight"
    } else if (return_type == "daily") {
      retp <- rutils::diffit(closep) ##  daily returns
      colnames(retp) <- "daily"
    } ## end if
    # retd <- (closep - openp) ##  daytime returns
    # colnames(retd) <- "daytime"
    # reton <- (openp - rutils::lagit(closep, lagg=1)) ##  overnight returns
    # colnames(reton) <- "overnight"
    # reton[1] <- 0
    # highp <- quantmod::Hi(ohlc)
    # lowp <- quantmod::Lo(ohlc)
    # hilo <- (highp - lowp) ##  range of daily prices
    
    # cat("sum(retp) = ", sum(retp), "\n")
    return(retp)
    
  })  ##  end Calculate the returns
  
  ##  Calculate the range variance estimator
  varv <- shiny::reactive({
    
    ##  Get model parameters from input argument
    # symboln <- input$symboln
    cat("Recalculating variance\n")
    lambdaf <- input$lambdaf
    variance_type <- input$variance_type

    if (variance_type == "ema") {
      # Downside variance estimator
      varv <- HighFreq::run_var(retp(), lambda=lambdaf)
      varv <- varv[, 2]
    } else if (variance_type == "downside") {
      ##  Calculate the range variance estimator
      retp <- retp()
      retp[retp > 0] <- 0
      varv <- HighFreq::run_var(retp, lambda=lambdaf)
      varv <- varv[, 2]
    } else if (variance_type == "range") {
      ##  Calculate the range variance estimator
      ohlc <- ohlc()
      # highp <- quantmod::Hi(ohlc)
      # lowp <- quantmod::Lo(ohlc)
      # hilo <- (highp - lowp) ##  range of daily prices
      varv <- HighFreq::run_var_ohlc(ohlc, lambda=lambdaf)
    } ## end if
    varv[varv < varfloor] <- varfloor
    varv[1:3] <- 1
    # cat("sum(varv) = ", sum(varv), "\n")
    return(varv)
    
  })  ##  end Calculate the range variance estimator
  
  ##  Recalculate the strategy
  pnls <- shiny::reactive({
    
    symboln <- input$symboln
    cat("Recalculating PnLs for", symboln, "\n")
    scaling_method <- input$scaling_method

    ##  Calculate the strategy PnLs
    varv <- varv()
    retp <- retp()
    if (scaling_method == "inverse_variance") {
      # Positions proportional to the inverse of the variance
      posv <- volt^2/varv
    } else if (scaling_method == "inverse_volatility") {
      # Positions proportional to the inverse of the volatility
      posv <- volt/sqrt(varv)
    } ## end if

    posv <- rutils::lagit(posv, lagg=1)
    pnls <- posv*retp
    
    # Scale the strategy returns to have the same volatility as the underlying returns
    pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
    
    ##  Bind together strategy pnls
    pnls <- cbind(retp, pnls)
    colnames(pnls) <- c(symboln, "Strategy")
    
    ##  Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    
    # cat("sum(pnls) = ", sum(pnls), "\n")
    pnls <- cbind(cumsum(pnls), posv)
    colnames(pnls) <- c(symboln, "Strategy", "Position")
    return(pnls)

  })  ##  end Recalculate the strategy
  

  ##  Plot the cumulative scaled returns
  ##  Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    ##  Get the pnls
    pnls <- pnls()

    ##  Get Sharpe ratios
    sharper <- values$sharper
    return_label <- if (input$return_type == "overnight") "Overnight" else "Daily"

    ##  Create dygraph plot
    captiont <- paste0(c(paste0(return_label, " SR="), "Timing SR="), sharper, collapse=" / ")
    colv <- colnames(pnls)
    dyplot <- dygraphs::dygraph(pnls, main=captiont) %>%
      dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colv[3], independentTicks=TRUE) %>%
      dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
      dySeries(name=colv[2], axis="y", strokeWidth=1, col="red") %>%
      dySeries(name=colv[3], axis="y2", strokeWidth=1, col="green") %>%
      dyLegend(show="always", width=300)
    
    ##  Return the dygraph object
    return(dyplot)


  })  ##  end output plot

}  ##  end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
