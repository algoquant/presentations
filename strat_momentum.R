##############################
# This is a shiny app for simulating a momentum strategy 
# for overnight stock or ETF returns.
#
# Just press the "Run App" button on upper right of this panel.
##############################


## Model and data setup
## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Load the overnight returns of SP500 stocks or ETFs
if (!exists("reton")) {
  cat("Loading the S&P500 overnight returns.\n")
  load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns_overnight.RData")
  # cat("Loading the ETF overnight returns.\n")
  # load("/Users/jerzy/Develop/lecture_slides/data/etf_returns_overnight.RData")
} # end if

# Select the most liquid ETFs based on trading volumes
# load("/Users/jerzy/Develop/data/etf_ohlc.RData")
# volumev <- eapply(etfenv, function(ohlc) {
# sum(quantmod::Vo(ohlc))
# })  # end eapply
# volumev <- sort(unlist(volumev), decreasing=TRUE)
# symbolv <- names(volumev[1:30])
# reton <- reton[, symbolv]

# Parameters for the strategy
varfloor <- 1e-8 ##  Variance floor to prevent division by zero in Kelly ratio calculations
volt <- 0.03 ##  Volatility target for scaling the strategy PnLs

# The date range for the strategy backtest
rangev <- "2007/"
reton <- reton[rangev]
nrows <- NROW(reton)
nstocks <- NCOL(reton)
datev <- index(reton)
# Calculate the index returns as the average of the overnight returns of all stocks
retm <- rowMeans(reton, na.rm=TRUE)
retm <- xts::xts(retm, order.by=datev)
symboln <- "Index"

stringv <- "Overnight"
captiont <- paste("Momentum With Overnight Stock Returns")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    ##  Input stock symboln
    # column(width=1, selectInput("symboln", label="Symbol", choices=symbolv, selected=symboln)),
    ##  Input lambda decay parameter
    column(width=2, sliderInput("lambdaf", label="Lambda", min=0.9, max=0.999, value=0.96, step=0.001)),
    # Input the bid-ask spread
    column(width=2, numericInput("bidask", label="Bid-ask:", value=0.0019, step=0.0001)),
    # Input Kelly quantile
    column(width=2, sliderInput("probv", label="Quantile:", min=0.01, max=0.99, value=0.98, step=0.01)),
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

  ##  Recalculate the strategy
  pnls <- shiny::reactive({
    
    # symboln <- input$symboln
    # cat("Recalculating PnLs for", symboln, "\n")

    lambdaf <- input$lambdaf
    bidask <- input$bidask
    probv <- input$probv
    
    # Calculate the EMA returns and volatilities
    varm <- HighFreq::run_var(reton, lambda=lambdaf)
    retsm <- varm[, 1:nstocks]
    retsm[is.na(retsm)] <- 0.0
    colnames(retsm) <- colnames(reton)
    varm <- varm[, (nstocks+1):(2*nstocks)]
    varm[1:3] <- 1.0
    varm[is.na(varm)] <- 1.0
    varm <- varm + varfloor; # Add small number to prevent division by zero
    # Calculate the Kelly ratio
    # kellyr <- retsm*volt^2/varm
    # Calculate the Sharpe ratios
    kellyr <- retsm*volt/sqrt(varm)
    # Set small Kelly ratios less than the threshold to zero
    # madv <- matrixStats::rowMads(kellyr, na.rm=TRUE)
    # kellyr[abs(kellyr) < quantv*madv] <- 0
    quantv <- matrixStats::rowQuantiles(kellyr, probs=probv, na.rm=TRUE)
    kellyr[kellyr < quantv] <- 0.0
    # Set very large Kelly ratios greater than the median to zero
    # medv <- matrixStats::rowMedians(kellyr, na.rm=TRUE)
    # kellyr[abs(kellyr) > 15*madv] <- 0
    # No shorting, so set negative Kelly ratios to zero
    kellyr[kellyr < 0] <- 0.0
    # Scale the Kelly ratios so the portfolio has the target volatility
    # kellyr <- kellyr*volt^2/rowSums(kellyr*varm, na.rm=TRUE)
    # kellyr <- kellyr*volt^2/varm/rowSums(kellyr, na.rm=TRUE)
    # Lag the Kelly ratios to avoid look-ahead bias
    kellyr <- rutils::lagit(kellyr, lagg=1)
    kellyr[1:11, ] <- varfloor # Set the first few Kelly ratios to small number to avoid large position sizes at the beginning of the backtest
    # kellyr <- kellyr*volt/sqrt(rowSums((kellyr^2)*volm, na.rm=TRUE))
    # Scale the Kelly ratios so their sum of squares is equal to one
    # kellyr <- kellyr/sqrt(rowSums(kellyr^2, na.rm=TRUE))

    # Calculate PnLs as the daily returns times the Kelly ratios
    # Calculate PnLs as the overnight returns times the Kelly ratios
    weightv <- kellyr
    # Equally weight the stocks with positive Kelly ratios
    # weightv[weightv > 0] <- 1
    pnls <- reton*weightv
    pnls <- rowSums(pnls, na.rm=TRUE)
    pnls[is.na(pnls)] <- 0
    # Subtract the SPY returns
    # retspy <- as.numeric(reton$SPY)
    # betav <- cov(pnls, retspy)/var(retspy, na.rm=TRUE)
    # pnls <- pnls - betav*retspy
    # pnls <- pnls - rowSums(weightv, na.rm=TRUE)*retspy
    # Scale the strategy PnLs to have the same volatility as the average overnight returns for all stocks
    pnls <- pnls*sd(retm[retm<0])/sd(pnls[pnls<0])

    # Calculate the transaction costs
    flipi <- rutils::diffit(weightv)
    costs <- 0.5*bidask*rowSums(abs(flipi), na.rm=TRUE)
    costs[(is.na(costs) | is.infinite(costs))] <- 0
    pnls <- (pnls - costs)
    
    ## Combine the strategy PnLs with the average overnight returns for all stocks
    pnls <- cbind(retm, pnls)
    colnames(pnls) <- c(stringv, "Strategy")

    ##  Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    
    return(cumsum(pnls))

  })  ##  end Recalculate the strategy
  

  ##  Plot dygraph of the cumulative PnLs
  output$dyplot <- dygraphs::renderDygraph({
    
    ##  Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    ##  Get Sharpe ratios
    sharper <- values$sharper

    ##  Create the dygraph object
    captiont <- paste0(paste0(c(stringv, "Momentum"), " SR="), sharper, collapse=" / ")
    dyplot <- dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
      dyLegend(show="always", width=300)
    
    ##  Plot the dygraph object
    return(dyplot)


  })  ##  end output plot

}  ##  end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
