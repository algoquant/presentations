
# This is a shiny app for simulating a logistic regression model
# to classify oversold and overbought extreme price points.
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(rutils)
library(shiny)
library(dygraphs)

## Model and data setup

# Calculate SVXY and VXX prices
datev <- zoo::index(rutils::etfenv$VTI)
nrows <- NROW(datev)

captiont <- paste("Logistic Model for Oversold and Overbought Extreme Price Points")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol to Trade",
                                choices=rutils::etfenv$symbolv, selected="VTI")),
    # Input VIX symbol
    # column(width=2, selectInput("symbol_vix", label="Symbol VIX",
    #                             choices=c("VXX", "SVXY"), selected="VXX")),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False")),
    # Input the bid-ask spread
    column(width=2, numericInput("bidask", label="Bid-ask:", value=0.0000, step=0.0001))
  ),  # end fluidRow

  fluidRow(
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Look-back", min=3, max=51, value=25, step=1)),
    # Input confidence level for price tops and bottoms
    column(width=2, sliderInput("confex", label="Confidence for extremes", min=0.5, max=0.99, value=0.91, step=0.01)),
    # Input confidence level for tops
    column(width=2, sliderInput("confitop", label="Confidence for tops", min=0.01, max=0.99, value=0.91, step=0.01)),
    # Input confidence level for bottoms
    column(width=2, sliderInput("confibot", label="Confidence for bottoms", min=0.01, max=0.99, value=0.91, step=0.01)),
    # Input the strategy coefficient: coeff=1 for momentum, and coeff=-1 for contrarian
    column(width=2, selectInput("coeff", "Coefficient:", choices=c(-1, 1), selected=(1))),
    # column(width=2, sliderInput("look_back", label="look_back:", min=1, max=21, value=5, step=1)),
    # column(width=2, sliderInput("slow_back", label="slow_back:", min=11, max=251, value=151, step=1)),
    # Input the trade lag
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=8, value=1, step=1))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="80%", height="550px")
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()

  # Load the data
  fittedv <- shiny::reactive({
    
    symbol <- input$symbol
    cat("Loading data for ", symbol, "\n")
    
    # Extract log OHLC prices
    ohlc <- get(symbol, rutils::etfenv)[datev]
    ohlc <- log(ohlc)
    closep <- quantmod::Cl(ohlc)
    retp <- rutils::diffit(closep)
    
    cat("Recalculating GLM for ", symbol, "\n")
    # Get model parameters from input argument
    look_back <- input$look_back
    half_back <- look_back %/% 2
    
    # Calculate the centered volatility
    stdev <- roll::roll_sd(retp, width=look_back, min_obs=1)
    stdev <- rutils::lagit(stdev, lagg=(-half_back))
    
    # Calculate the z-scores of prices
    midp <- 1:nrows  # mid point
    startp <- (midp - half_back)  # start point
    startp[1:half_back] <- 1
    endp <- (midp + half_back)  # end point
    endp[(nrows-half_back+1):nrows] <- nrows
    closep <- coredata(closep)
    pricez <- (2*closep[midp, ] - closep[startp, ] - closep[endp, ])
    pricez <- ifelse(stdev > 0, pricez/stdev, 0)

    # Calculate the vectors of tops and bottoms
    threshv <- quantile(pricez, input$confex)
    tops <- coredata(pricez > threshv)
    threshv <- quantile(pricez, 1-input$confex)
    bottoms <- coredata(pricez < threshv)

    # Calculate volatility z-scores
    volat <- sqrt(HighFreq::roll_var_ohlc(ohlc, look_back=look_back))
    meanv <- roll::rolregmodean(volat, width=look_back, min_obs=1)
    stdev <- roll::roll_sd(rutils::diffit(volat), width=look_back, min_obs=1)
    volatz <- ifelse(stdev > 0, (volat - meanv)/stdev, 0)

    # Calculate volume z-scores
    volumes <- quantmod::Vo(ohlc)
    meanv <- roll::rolregmodean(volumes, width=look_back, min_obs=1)
    stdev <- roll::roll_sd(rutils::diffit(volumes), width=look_back, min_obs=1)
    volumez <- ifelse(stdev > 0, (volumes - meanv)/stdev, 0)

    # Calculate trailing price regression z-scores
    dates <- matrix(zoo::index(closep))
    regz <- drop(HighFreq::roll_zscores(respv=closep, predictor=dates, look_back=look_back))
    regz[1:look_back] <- 0
    
    # Calculate SVXY z-scores
    # meanv <- roll::rolregmodean(svxyc, width=look_back, min_obs=1)
    # stdev <- sqrt(HighFreq::roll_var_ohlc(svxy, look_back=look_back, scale=FALSE))
    # svxyz <- ifelse(stdev > 0, (svxyc - meanv)/stdev, 0)
    
    # Calculate VXX z-scores
    # meanv <- roll::rolregmodean(vxxc, width=look_back, min_obs=1)
    # stdev <- sqrt(HighFreq::roll_var_ohlc(vxx, look_back=look_back, scale=FALSE))
    # vxxz <- ifelse(stdev > 0, (vxxc - meanv)/stdev, 0)
    
    # Define predictor matrix
    # predv <- cbind(vxxz, svxyz, volatz, volumez)
    predv <- cbind(volatz, volumez, regz)
    predv <- coredata(predv)
    predv[1, ] <- 0
    # colnames(predv) <- c("vxx", "svxy", "volat", "volume")
    # colnames(predv) <- c("volat", "volume")
    predv <- rutils::lagit(predv)
    
    # Calculate in-sample forecasts of tops from logistic regression
    logmod <- glm(tops ~ predv, family=binomial(logit))
    fittedv <- logmod$fitted.values
    threshv <- quantile(fittedv, input$confitop)
    forecastops <- (fittedv > threshv)
    
    # Calculate in-sample forecasts of bottoms from logistic regression
    logmod <- glm(bottoms ~ predv, family=binomial(logit))
    fittedv <- logmod$fitted.values
    threshv <- quantile(fittedv, input$confibot)
    forecastbot <- (fittedv > threshv)
    
    # Return fitted values
    fittedv <- cbind(retp, forecastops, forecastbot)
    colnames(fittedv) <- c("returns", "tops", "bottoms")
    fittedv
    
  })  # end Load the data
  

  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    symbol <- input$symbol
    cat("Recalculating strategy for ", symbol, "\n")
    # Get model parameters from input argument
    coeff <- as.numeric(input$coeff)
    lagg <- input$lagg

    # Extract fitted values
    forecastops <- as.logical(fittedv()$tops)
    forecastbot <- as.logical(fittedv()$bottoms)
    retp <- fittedv()$returns
    
    ## Backtest strategy for flipping if two consecutive positive and negative returns
    # Flip position only if the indic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # posv <- ifelse(indic == indic_lag, indic, posv)
    
    # posv <- rep(NA_integer_, nrows)
    # posv[1] <- 0
    # posv[forecastops] <- (-1)
    # posv[forecastbot] <- 1
    # posv <- zoo::na.locf(posv)
    foo <- HighFreq::roll_sum(matrix(forecastops), lagg)
    bar <- HighFreq::roll_sum(matrix(forecastbot), lagg)
    posv <- (bar-foo)
    
    # Calculate indicator of flipping the positions
    indic <- rutils::diffit(posv)
    # Calculate number of trades
    values$ntrades <- sum(abs(indic)>0)
    
    # Add buy/sell indicators for annotations
    indicb <- (indic > 0)
    indics <- (indic < 0)
    
    # Lag the positions to trade in next period
    posv <- rutils::lagit(posv, lagg=1)
    
    # Calculate strategy pnls
    pnls <- posv*retp
    
    # Calculate transaction costs
    costs <- 0.5*input$bidask*abs(indic)
    pnls <- (pnls - costs)

    # Scale the pnls so they have same SD as returns
    pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
    
    # Bind together strategy pnls
    pnls <- cbind(retp, pnls)
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)

    # Bind with indicators
    pnls <- lapply(pnls, cumsum)
    pnls <- do.call(cbind, pnls)
    pnls <- cbind(pnls, pnls[indicb, 1], pnls[indics, 1])
    colnames(pnls) <- c(paste(symbol, "Returns"), "Strategy", "Buy", "Sell")

    pnls

  })  # end Recalculate the strategy
  

  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    cat("Plotting for ", input$symbol, "\n")
    
    # Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Get Sharpe ratios
    sharper <- values$sharper
    # Get number of trades
    ntrades <- values$ntrades
    
    captiont <- paste("Strategy for", input$symbol, "Regression Z-score / \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
                      "Number of trades=", ntrades)
    
    # Plot with annotations
    add_annotations <- input$add_annotations
    
    if (add_annotations == "True") {
      dygraphs::dygraph(pnls, main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red") %>%
        dySeries(name=colnamev[3], axis="y", drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=colnamev[4], axis="y", drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == "False") {
      dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red")
    }  # end if
    
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
