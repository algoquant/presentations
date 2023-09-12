##############################
# This is a shiny app for backtesting factor models.
# It calculates the forecasts as a weighted sum of autoregressive 
# factors: a level factor, trend factor, and a revert factor.
# Other factors can also be added.
# The forecasts can be divided by their volatility.
# The stock position is proportional to the forecasts.
# 
# Just press the "Run App" button on the upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Load daily stock prices

# Load daily OHLC bars

ohlc <- rutils::etfenv$VTI
datev <- zoo::index(ohlc)
nrows <- NROW(ohlc)
pricev <- quantmod::Cl(ohlc)
symboln <- rutils::get_name(colnames(pricev))
retp <- rutils::diffit(log(pricev))
volumv <- quantmod::Vo(ohlc)
# Calculate trailing average volume
volumr <- HighFreq::run_mean(volumv, lambda=0.25)
# Scale the returns using volume clock to trading time
retsc <- ifelse(volumv > 0, volumr*retp/volumv, 0)

# Calculate the AR coefficients
# respv <- retsc
# orderp <- 5
# predm <- lapply(1:orderp, rutils::lagit, input=respv)
# predm <- rutils::do_call(cbind, predm)
# predm <- cbind(rep(1, nrows), predm)
# colnames(predm) <- c("phi0", paste0("lag", 1:orderp))
# predinv <- MASS::ginv(predm)
# coeff <- predinv %*% respv
# Calculate the in-sample forecasts of VTI
# fcasts <- predm %*% coeff
# Simulate autoregressive strategy in-sample
# pnls <- retp*fcasts


# captiont <- paste("Autoregressive Strategy For", symboln, dataf)
# captiont <- paste("Autoregressive Strategy Using the Lagged Fitted Values For", symboln, dataf)
captiont <- paste("Strategy Using Autoregressive Forecasts For", symboln)

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  # Create single row with four slider inputs
  fluidRow(
    # Input the level factor
    column(width=2, sliderInput("levelf", label="Level factor:", min=0.0, max=1.0, value=0.0, step=0.01)),
    # Input the trend factor
    column(width=2, sliderInput("trendf", label="Trend factor:", min=0.0, max=150.0, value=50.0, step=1.0)),
    # Input the revert factor
    column(width=2, sliderInput("revertf", label="Revert factor:", min=0.0, max=150.0, value=50.0, step=1.0)),
    # Input lambda decay parameter
    column(width=2, sliderInput("lambda", label="lambda:", min=0.1, max=0.99, value=0.8, step=0.01)),
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  # mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface


## Define the server code
servfun <- shiny::shinyServer(function(input, output) {

  # Recalculate the strategy pnls
  pnlsc <- shiny::reactive({

    cat("Recalculating the strategy pnls", "\n")

    levelf <- input$levelf
    trendf <- input$trendf
    revertf <- input$revertf
    lambda <- input$lambda
    
    # Calculate the forecasts as a weighted sum of three factors
    trendv <- rutils::lagit(HighFreq::run_mean(retp, lambda=lambda))
    fcasts <- (levelf + trendf*trendv - revertf*rutils::lagit(retp))
    
    # Divide the forecasts by their volatility.
    # fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=lambda))
    # fcasts <- ifelse(fcastv > 0, fcasts/fcastv, 0)
    # Simulate autoregressive strategy
    pnls <- retp*fcasts
    pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
    
    pnls

  })  # end reactive code

  
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    
    wealthv <- cbind(retp, pnlsc())
    colnames(wealthv) <- c(symboln, "Strategy")
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 3)
    colnamev <- colnames(wealthv)
    
    captiont <- paste(paste0(paste(colnamev[1:2], "Sharpe =", sharper), collapse=" / "), "/ \n")
    
    dygraphs::dygraph(cumsum(wealthv), main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
      dyLegend(show="always", width=300)
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
