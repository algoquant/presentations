##############################
# This is a shiny app for backtesting a stock versus ETF pairs strategy
# using a dynamic beta.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs only once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Load daily S&P500 stock prices
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
symbolstocks <- sort(colnames(prices))
symbolstock <- "AAPL"

symbolsetf <- colnames(rutils::etfenv$prices)
symboletf <- "XLK"


# captiont <- paste("Stat-arb Portfolio Strategy app_statarb_strat.R")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Stock vs ETF Pairs Strategy Using a Dynamic Beta"),
  
  # create single row with four slider inputs
  fluidRow(
    # Input stock symbol
    column(width=1, selectInput("symbolstock", label="Stock", choices=symbolstocks, selected=symbolstock)),
    # Input ETF symbol
    column(width=1, selectInput("symboletf", label="ETF", choices=symbolsetf, selected=symboletf)),
    # Input beta parameter
    column(width=3, sliderInput("beta", label="beta:", min=0.1, max=3.0, value=1.0, step=0.1)),
    # Input lambda decay parameter
    column(width=3, sliderInput("lambda", label="lambda:", min=0.01, max=0.99, value=0.3, step=0.01)),
    # Input threshold parameter
    column(width=2, sliderInput("threshold", label="Threshold", min=0.1, max=3.0, value=1.0, step=0.1)),
    # Input lag trade parameter
    column(width=1, sliderInput("lagg", label="Lag", min=1, max=3, value=1, step=1)),
    # Input trending or reverting (contrarian) strategy
    column(width=1, selectInput("coeff", label="Trend (1) Revert (-1)", choices=c(1, -1), selected=(-1)))
  ),  # end fluidRow
  
  # create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  # mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
)  # end fluidPage interface


## Define the server code
servfun <- shiny::shinyServer(function(input, output) {

  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Prepare the prices
  pricev <- shiny::reactive({
    
    cat("Preparing the prices", "\n")
    # Get model parameters from input argument
    symbolstock <- input$symbolstock
    symboletf <- input$symboletf

    # Prepare the prices
    pricev <- get(symbolstock, prices)
    pricetf <- get(symboletf, rutils::etfenv$prices)
    pricev <- na.omit(cbind(pricev, pricetf))
    colnames(pricev) <- c(symbolstock, symboletf)
    log(pricev)
    
  })  # end reactive code
  
  
  # Prepare the returns
  retv <- shiny::reactive({
    
    cat("Preparing the returns", "\n")
    retv <- rutils::diffit(pricev())
    retv[1, ] <- retv[2, ]
    colnames(retv) <- c(input$symbolstock, input$symboletf)
    retv
    
  })  # end reactive code
  
  
  # Recalculate the betas
  betas <- shiny::reactive({
    
    cat("Recalculating the betas", "\n")
    # Get model parameters from input argument
    lambda <- input$lambda
    retv <- retv()
    # pricev <- pricev()
    
    # Get model parameters from input argument
    # betas <- input$beta
    
    # betas <- drop(cov(pricev[, 1], pricev[, 2])/var(pricev[, 2]))
    # betas <- HighFreq::run_reg(pricev[, 1, drop=FALSE], pricev[, 2, drop=FALSE], lambda=lambda)
    # betas <- betas[, 3, drop=FALSE]
    # betas <- HighFreq::lagit(betas[, 3, drop=FALSE])
    covars <- HighFreq::run_covar(retv, lambda=lambda)
    covars[1, ] <- 1
    # covars[, 1]/covars[, 3]
    rutils::lagit(covars[, 1]/covars[, 3])
    # rep(1, NROW(retv))

  })  # end reactive code
  
  
  # Recalculate the residuals
  pricec <- shiny::reactive({
    
    cat("Recalculating the residuals", "\n")
    # Get model parameters from input argument
    lambda <- input$lambda
    # retv <- retv()
    pricev <- pricev()
    betas <- betas()
    
    # Recalculate the residuals
    # pricec <- (pricev[, 1, drop=FALSE] - betas*pricev[, 2, drop=FALSE])
    # colnames(pricec) <- "pricec"
    # # pricec - min(pricec) + 1
    # pricec

    # Add unit intercept column to the predictor matrix
    predm <- cbind(rep(1, NROW(pricev)), pricev[, 2])
    
    controlv <- HighFreq::param_reg()    
    # regs <- HighFreq::run_reg(respv=retv[, 1], predm=predm, lambda=lambda, controlv=controlv)
    regs <- HighFreq::run_reg(respv=pricev[, 1], predm=predm, lambda=lambda, controlv=controlv)
    regs[, NCOL(regs)-1, drop=FALSE]

  })  # end reactive code
  
  
  # Recalculate the trailing means of residuals
  meanv <- shiny::reactive({
    
    cat("Recalculating the trailing means", "\n")
    # Get model parameters from input argument
    lambda <- input$lambda

    # Recalculate the trailing means of residuals
    HighFreq::run_mean(pricec(), lambda=lambda)

  })  # end reactive code
  
  
  # Recalculate the trailing variance of residuals
  vars <- shiny::reactive({
    
    cat("Recalculating the trailing variance", "\n")
    # Get model parameters from input argument
    lambda <- input$lambda

    # Recalculate the trailing variance of residuals
    HighFreq::run_var(pricec(), lambda=lambda)

  })  # end reactive code
  
  
  # Rerun the strategy
  wealthv <- shiny::reactive({
    
    cat("Recalculating strategy", "\n")
    # Get model parameters from input argument
    symbolstock <- input$symbolstock
    symboletf <- input$symboletf
    lagg <- input$lagg
    threshold <- input$threshold
    coeff <- as.numeric(input$coeff)
    
    pricev <- pricev()
    nrows <- NROW(pricev)
    betas <- betas()
    pricec <- pricec()
    meanv <- meanv()
    vars <- vars()
    # datev <- zoo::index(pricev)
    # meanv <- HighFreq::lagit(meanv)
    # threshd <- threshold*HighFreq::lagit(sqrt(vars))
    threshd <- threshold*sqrt(vars)
    
    pricec <- pricec - meanv
    pricen <- as.numeric(pricec)
    
    # Code with loop in R
    # posv <- integer(nrows) # Trade positions
    # posv[1] <- 0 # Initial position
    # npos <- 0
    # betap <- betas[1]
    # pnls <- numeric(nrows)
    # for (it in (lagg+1):nrows) {
    #   # Calculate the pnls
    #   # pnls[it] <- posv[it-1]*(retv[it, 1] - betap*retv[it, 2])
    #   if (pricen[it-lagg] > threshd[it-lagg]) {
    #     # Enter short
    #     # betap <- betas[it]
    #     posv[it] <- coeff
    #     npos <- npos+1
    #   } else if (pricen[it-lagg] < -threshd[it-lagg]) {
    #     # Enter long
    #     # betap <- betas[it]
    #     posv[it] <- (-coeff)
    #     npos <- npos+1
      # } else if ((posv[it-1] < 0) && (pricen[it-lagg] < 0)) {
      #   # Unwind short
      #   posv[it] <- 0
      #   npos <- npos+1
      # } else if ((posv[it-1] > 0) && (pricen[it-lagg] > 0)) {
      #   # Unwind long
      #   posv[it] <- 0
      #   npos <- npos+1
    #   } else {
    #     # Do nothing
    #     posv[it] <- posv[it-1]
    #   }  # end if
    # }  # end for
    
    # Equivalent code without loop in R
    # Calculate positions
    posv <- rep(NA_integer_, nrows)
    posv[1] <- 0
    posv <- ifelse(pricen > threshd, coeff, posv)
    posv <- ifelse(pricen < -threshd, -coeff, posv)
    posv <- zoo::na.locf(posv)
    # Lag the positions to trade in next period
    npos <-sum(abs(rutils::diffit(posv)) > 0)
    posv <- rutils::lagit(posv, lagg=lagg)

    # Calculate the pnls
    # retr <- rutils::diffit(pricen) # wrong!
    retv <- retv()
    retr <- (retv[, 1] - betas*retv[, 2]) # correct
    
    pnls <- coeff*posv*retr
    # pnls <- retr

    # Rescale strategy cashflows to volatility of ETF
    pricetf <- pricev[, 1]
    retp <- rutils::diffit(pricetf)
    pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
    pnls <- cumsum(pnls)
    # plot(pnls, t="l")
    
    values$ntrades <- npos
    
    wealthv <- cbind(pricetf, pnls)
    colnames(wealthv) <- c(symbolstock, "Strategy")

    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(rutils::diffit(wealthv), function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    
    # wealthv <- cbind(pricetf, log(pnls - min(pnls) + 1))
    # colnames(wealthv) <- c(symbolstock, "Strategy")
    
    wealthv
    
  })  # end reactive code
  
  
  # Return the dygraph plot to output argument
  output$dyplot <- dygraphs::renderDygraph({
    
    symbolstock <- input$symbolstock
    symboletf <- input$symboletf
    wealthv <- wealthv()
    colnamev <- colnames(wealthv)
    
    captiont <- paste("Strategy for", symbolstock, "vs", symboletf, "/ \n", 
                      paste0(c("Index SR=", "Strategy SR="), values$sharper, collapse=" / "), "/ \n",
                      "Number of trades=", values$ntrades)
    
    endd <- rutils::calc_endpoints(wealthv, interval="weeks")
    dygraphs::dygraph(wealthv[endd], main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
      # dySeries(name=colnamev[3], axis="y", label=colnamev[3], strokeWidth=2, col="green") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red") %>%
      dyLegend(show="always", width=500)
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
