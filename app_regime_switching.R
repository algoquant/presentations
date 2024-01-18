##############################
# This is a shiny app for backtesting regime switching 
# between two autoregressive strategies.
# It calculates the model weights depending on their 
# trailing performance.  
# It applies more weight to the model with better 
# trailing performance.
# First it simulates two competing autoregressive 
# models for the low and high volatility regimes, 
# using the low and high volatility coefficients. 
# It applies several different weighting schemes.
# It calculates the weights proportional to the 
# trailing regression betas - doesn't work because 
# too much noise.
# It calculates the Kelly ratios from the trailing 
# mean returns and their variance - doesn't work 
# because too much noise.
# It calculates the Kalman gains from the trailing 
# square forecast errors - doesn't work because 
# the AR forecasts are much smaller than the 
# realized returns, so the square forecast errors 
# are similar for both models.
# 
# Just press the "Run App" button on the upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Calculate the VTI daily percentage returns
retp <- na.omit(rutils::etfenv$returns$VTI)
nrows <- NROW(retp)
orderp <- 5

# Calculate the high volatility AR coefficients
respv <- retp["2008/2011"]
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, NROW(predm)), predm)
predinv <- MASS::ginv(predm)
coeffh <- drop(predinv %*% respv)
coeffn <- paste0("phi", 0:(NROW(coeffh)-1))
# Calculate the low volatility AR coefficients
respv <- retp["2012/2019"]
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, NROW(predm)), predm)
predinv <- MASS::ginv(predm)
coeffl <- drop(predinv %*% respv)
# Calculate the high volatility pnls
predm <- lapply(1:orderp, rutils::lagit, input=retp)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
fcasth <- predm %*% coeffh
residh <- (fcasth - retp)^2
pnlh <- retp*fcasth
pnlh <- pnlh*sd(retp)/sd(pnlh)
# Calculate the low volatility pnls
fcastl <- predm %*% coeffl
residl <- (fcastl - retp)^2
pnll <- retp*fcastl
pnll <- pnll*sd(retp)/sd(pnll)

# Add unit intercept column to predictor matrix
predh <- cbind(rep(1, NROW(fcasth)), fcasth)
predl <- cbind(rep(1, NROW(fcastl)), fcastl)
controlv <- HighFreq::param_reg()

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Kelly AR Strategy"),
  
  # create single row with four slider inputs
  fluidRow(
    # Input lambda decay parameter
    column(width=2, sliderInput("lambda", label="lambda:", min=0.01, max=0.99, value=0.5, step=0.01))
    # Input slow lambda decay parameter
    # column(width=2, sliderInput("lambdas", label="lambda slow:", min=0.98, max=0.999, value=0.99, step=0.001)),
    # column(width=2, sliderInput("lambdas", label="lambda slow:", min=0.1, max=0.9, value=0.5, step=0.01))
    # Input fast lambda decay parameter
    # column(width=2, sliderInput("lambdaf", label="lambda fast:", min=0.5, max=0.99, value=0.9, step=0.01)),
    # column(width=2, sliderInput("lambdaf", label="lambda fast:", min=0.1, max=0.99, value=0.9, step=0.01)),
    # Input lag parameter
    # column(width=1, sliderInput("lagg", label="lag", min=1, max=3, value=1, step=1)),
    # Input the bid-ask spread
    # column(width=2, numericInput("bidask", label="Bid-ask:", value=0.0005, step=0.0001)),
    # Input trending or reverting (contrarian) strategy
    # column(width=1, selectInput("coeff", label="Trend (1) Revert (-1)", choices=c(1, -1), selected=(-1)))
  ),  # end fluidRow
  
  # create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  # mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface


## Define the server code
servfun <- shiny::shinyServer(function(input, output) {
  
  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  
  # Rerun the strategy
  wealthv <- shiny::reactive({
    cat("Recalculating the strategy", "\n")
    lambda <- input$lambda

    # Kelly
    # wh <- HighFreq::run_mean(pnlh, lambda=lambda)/HighFreq::run_var(pnlh, lambda=lambda)
    # wh <- rutils::lagit(wh)
    # wl <- HighFreq::run_mean(pnll, lambda=lambda)/HighFreq::run_var(pnll, lambda=lambda)
    # wl <- rutils::lagit(wl)
    # smsq <- sqrt(wh^2+wl^2)
    # wh <- ifelse(smsq>0, wh/smsq, 0)
    # wl <- ifelse(smsq>0, wl/smsq, 0)
    # pnls <- (wh*pnlh + wl*pnll)
    
    # Kalman
    # wh <- HighFreq::run_mean(residh, lambda=lambda)
    # wh <- rutils::lagit(wh)
    # wl <- HighFreq::run_mean(residl, lambda=lambda)
    # wl <- rutils::lagit(wl)
    # kgain <- wh/(wh+wl)
    # kgain[1] <- 0.5
    # pnls <- ((1-kgain)*pnlh + kgain*pnll)
    
    # Regression
    regh <- HighFreq::run_reg(retp, predh, lambda=lambda, controlv=controlv)
    wh <- rutils::lagit(regh[, 2])
    regl <- HighFreq::run_reg(retp, predl, lambda=lambda, controlv=controlv)
    wh <- rutils::lagit(regl[, 2])
    smsq <- sqrt(wh^2+wl^2)
    wh <- ifelse(smsq>0, wh/smsq, 0)
    wl <- ifelse(smsq>0, wl/smsq, 0)
    pnls <- (wh*pnlh + wl*pnll)
    
    
    pnls <- pnls*sd(retp)/sd(pnls)
    # Calculate the Sharpe and Sortino ratios
    wealthv <- cbind(retp, pnls)
    colnames(wealthv) <- c("VTI", "Strategy")

    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    
    wealthv
    
  })  # end reactive code
  
  
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    
    # symboln <- input$symboln
    # symboletf <- input$symboletf
    wealthv <- wealthv()
    colnamev <- colnames(wealthv)
    values$ntrades <- 0
    
    captiont <- paste(paste0(paste(colnamev[1:2], "Sharpe =", values$sharper), collapse=" / "), "/ \n",
                      "Number of trades=", values$ntrades)
    
    # endd <- rutils::calc_endpoints(wealthv, interval="days")
    dygraphs::dygraph(cumsum(wealthv), main=captiont) %>%
    # dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
      dyLegend(show="always", width=300)
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
