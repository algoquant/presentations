##############################
# This is a shiny app for simulating a rolling portfolio 
# optimization strategy with filtering of returns.
# It uses HighFreq::back_test()
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# load packages
library(HighFreq)
library(shiny)
library(dygraphs)
# Model and data setup

# Source the model function
# source("/Users/jerzy/Develop/lecture_slides/scripts/roll_portf.R")

riskf <- 0.03/260

# Variables setup for testing
# interval <- "weeks"
# look_back <- 5
# lambda <- 0.01
# method <- "max_sharpe"
# confl <- 0.25
# dimax <- 11
# alpha <- 0.01


## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  # titlePanel(captiont),
  # titlePanel("Rolling Portfolio Optimization Strategy for ETF Portfolio or for Sub-Portfolio of S&P500 Stocks"),
  titlePanel("Rolling Portfolio Optimization Strategy"),
  
  # fluidRow(
  #   # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
  #   column(width=12, 
  #          h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
  #          actionButton("recalcb", "Recalculate the Model"))
  # ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input choice of data
    column(width=2, selectInput("datas", label="Data",
                                choices=c("etf", "sp500"), selected="sp500")),
    # Input choice of model
    column(width=2, selectInput("method", label="Model type",
                                choices=c("sharpem", "kellym", "maxsharpe", "maxsharpemed", "minvarlin", "minvarquad", "ranksimple", "rank_hold", "robustm", "quantilev"), selected="maxsharpe")),
    # Input end points interval
    column(width=2, selectInput("interval", label="End points Interval",
                                choices=c("days", "weeks", "months", "years"), selected="months")),
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Lookback interval",
                                min=2, max=40, value=11, step=1)),
    # Input decay factor for averaging the portfolio weights
    column(width=2, sliderInput("lambda", label="Decay factor:",
                                min=0.01, max=0.99, value=0.01, step=0.05)),
    # Input exponent for variance
    column(width=2, sliderInput("exponent", label="Variance exponent:",
                                min=0.25, max=1.5, value=1.0, step=0.05)),
    # Input number of eigenvalues for regularized matrix inverse
    column(width=2, sliderInput("dimax", label="Number of eigenvalues",
                                min=2, max=100, value=11, step=1)),
    # Input the shrinkage intensity
    column(width=2, sliderInput("alpha", label="Shrinkage intensity",
                                min=0.01, max=0.99, value=0.01, step=0.05)),
    # Input the quantile
    column(width=2, sliderInput("confl", label="Confidence level",
                                min=0.01, max=0.49, value=0.25, step=0.01)),
    # Input choice of excess returns
    column(width=2, selectInput("returns_scaling", label="Excess returns scaling",
                                choices=c("none", "sum", "rescaled", "volatility", "sharpe", "skew"), selected="none")),
    # If trend=1 then trending, If trend=(-1) then contrarian
    column(width=2, selectInput("trend", label="Trend coefficient",
                                choices=c(1, -1), selected=(1)))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="550px")
  # Create output plot panel
  # mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  ## Create an empty list of reactive values.
  globals <- reactiveValues()
  
  # Load the data
  datav <- shiny::reactive({
    # Get model parameters from input argument
    datas <- input$datas
    
    # Load data if needed
    switch(datas,
           "etf" = {
             cat("Loading ETF data \n")
             captiont <- "Rolling Portfolio Optimization Strategy for ETF Portfolio"
             # captiont <- paste("Contrarian Strategy for", symbol, "Using the Hampel Filter Over Prices")
             # Load PCA data
             # load("/Users/jerzy/Develop/data/pcarets.RData")
             # rets <- pcarets[, 1:9]
             # Load ETF data
             # symbolv <- c("VXX", "VEU", "GLD", "EEM", "IEF", "DBC", "TLT", "SVXY", "VYM", "USO", "MTUM", "IWB", "IWD", "VTI")
             # symbolv <- c("VEU", "GLD", "EEM", "DBC", "VYM", "USO", "IWB", "IWD", "VTI")
             # Select all the ETF symbols except "VXX", "SVXY" and "MTUM"
             symbolv <- rutils::etfenv$symbolv
             symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "TLT", "IEF", "MTUM", "QUAL", "VLUE", "USMV"))]
             # symbolv <- symbolv[!(symbolv %in% c("TLT", "IEF", "VXX", "SVXY", "MTUM"))]
             # symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM"))]
             rets <- rutils::etfenv$returns[, symbolv]
             # method <- "max_sharpe"
             # look_back <- 8
             # look_back_max <- 71
             # dimax <- 5
             # alpha <- 0.01
             # rets <- rets["2001-06-02/"]
           },
           "sp500" = {
             cat("Loading S&P500 data \n")
             captiont <- "Rolling Portfolio Optimization Strategy for Sub-Portfolio of S&P500 Stocks"
             # Load S&P500 stock returns
             # cat("sp500 init load \n")
             # load("/Users/jerzy/Develop/lecture_slides/data/returns100.RData")
             load("/Users/jerzy/Develop/data/sp500_returns.RData")
             # Select data after 2000
             rets <- returns100["2000/"]
             # Copy over NA values
             # rets[1, is.na(rets[1, ])] <- 0
             # rets <- zoo::na.locf(rets, na.rm=FALSE)
             # nrows <- NROW(rets)
             # ncols <- NCOL(rets)
             # Select the columns with non-zero returns
             # rets <- rets[, !(rets[ncols %/% 10, ] == 0)]
             # Select 100 columns to reduce computations
             # set.seed(1121)  # Reset random number generator
             # samplev <- sample(1:NCOL(rets), 100)
             # rets <- rets[, samplev]
             # rets <- cbind(rets, rutils::etfenv$rets$SVXY, rutils::etfenv$rets$VXX)
             # method <- "rank"
             # look_back <- 5
             # look_back_max <- 50
             # dimax <- 35
             # alpha <- 0.01
           }
    )  # end switch
    
    # Copy over NA values
    rets[1, is.na(rets[1, ])] <- 0
    rets <- zoo::na.locf(rets, na.rm=FALSE)
    nrows <- NROW(rets)
    ncols <- NCOL(rets)
    globals$nrows <- nrows
    globals$ncols <- ncols
    
    # Random data
    # coredata(rets) <- matrix(rnorm(prod(dim(rets)))/100, nc=ncols)
    
    # Calculate returns on equal weight portfolio
    # indeks <- xts(cumprod(1 + rowMeans(rets)), index(rets))
    indeks <- rowMeans(rets)
    # sharper <- sqrt(252)*mean(indeks)/stdev
    indeks <- xts(indeks, index(rets))
    # stdev <- sd(indeks[indeks<0])
    globals$stdev <- sd(indeks[indeks<0])

    list(rets=rets, indeks=indeks)
    
  })  # end Load the data
  
  
  ## Calculate the excess
  excess <- shiny::reactive({
    cat("Calculating the excess", "\n")
    
    returns_scaling <- input$returns_scaling
    exponent <- input$exponent
    interval <- input$interval
    
    rets <- datav()$rets
    nperiods <- globals$nperiods
    
    # Scale the returns and call them excess
    switch(retv_scaling,
           "none" = {
             cat("No returns scaling \n")
             excess <- (rets - riskf)
             # excess <- returns100
           },
           "sum" = {
             # Calculate trailing sum
             excess <- HighFreq::roll_sum(rets, look_back=nperiods)
           },
           "rescaled" = {
             # Scale the rets by the trailing volatility
             excess <- rets/(HighFreq::roll_var(rets, look_back=nperiods))^exponent
             excess[!is.finite(excess)] <- 0.1
           },
           "volatility" = {
             # Calculate trailing volatilities
             excess <- HighFreq::roll_var(rets, look_back=nperiods)^exponent
             excess[!is.finite(excess)] <- 0.1
           },
           "sharpe" = {
             # Calculate trailing Sharpe ratios
             # excess <- HighFreq::roll_sum(rets, look_back=nperiods)/nperiods/(HighFreq::roll_var(rets, look_back=nperiods))^exponent
             # excess[!is.finite(excess)] <- 0.1
             volat <- HighFreq::roll_var(rets, look_back=nperiods)^exponent
             volat[volat == 0] <- 1
             excess <- HighFreq::roll_sum(rets, look_back=nperiods)/nperiods
             excess <- excess/volat
           },
           "skew" = {
             ## Calculate the skew-like stats
             maxv <- RcppRoll::rolregmodax(rets, n=nperiods, align="right")
             # minv <- -RcppRoll::rolregmodax(-rets, n=nperiods, align="right")
             # meanv <- RcppRoll::rolregmodean(rets, n=nperiods, align="right")
             medianv <- RcppRoll::roll_median(rets, n=nperiods, align="right")
             # Calculate difference between upside minus downside volatility
             # core_data <- coredata(rets)
             # upsd <- RcppRoll::roll_sd(ifelse(core_data>0, core_data, 0), n=nperiods, align="right")
             # downsd <- RcppRoll::roll_sd(ifelse(core_data<0, core_data, 0), n=nperiods, align="right")
             # Calculate rolling skew using Rcpp
             # First compile this file in R by running this command:
             # Rcpp::sourceCpp(file="/Users/jerzy/Develop/R/Rcpp/roll_skew.cpp")
             # rolling_skew <- roll_kurtosis(rets, look_back=nperiods)
             # rolling_skew <- roll_skew(rets, look_back=nperiods)
             # rolling_skew <- roll_skew(rets, typev="quantile", alpha=confl, look_back=nperiods)
             # rolling_skew[!is.finite(rolling_skew)] <- 0
             # rolling_skew <- na.locf(rolling_skew)

             # Best performing stats so far
             excess <- maxv / medianv
             # excess <- rolling_skew
             # excess <- (upsd - downsd)
             # excess <- (maxv - medianv)
             # excess <- maxv/meanv^exponent
             # excess <- maxv - minv
             # excess <- (maxv + minv - 2*medianv) / (maxv - minv)
             # excess <- (maxv - medianv) / (medianv - minv)
             # excess <- (meanv - medianv)
             excess[is.infinite(excess)] <- 0
             excess[is.na(excess)] <- 0
             # Pad zeros up-front
             excess <- rbind(matrix(1:((nperiods-1)*globals$ncols), ncol=globals$ncols), excess)
           }
    )  # end switch
    
    # Filter the excess returns using an exponential weighted filter - doesn't provide significant improvement
    # Calculate the weights
    # weights <- exp(-lambda*(1:look_back))
    # weights <- weights/sum(weights)
    # weights <- matrix(weights, nc=1)
    # Calculate smoothed excess returns
    # excess <- HighFreq::roll_conv(excess, weights=weights)
    
    excess

  })  # end Calculate the excess
  
  
  ## Calculate the end points
  roll_points <- shiny::reactive({
    cat("Calculating the end points", "\n")
    
    interval <- input$interval
    look_back <- input$look_back
    rets <- datav()$rets
    
    # Define end points
    endp <- rutils::calc_endpoints(rets, interval=interval)
    # endp <- ifelse(endp< ncols+1), ncols+1, endp)
    endp <- endp[endp > (globals$ncols+1)]
    nrows <- NROW(endp)
    # Define startp
    # Rolling window
    startp <- c(rep_len(1, look_back-1), endp[1:(nrows-look_back+1)])
    # Expanding window
    # startp <- rep_len(1, nrows)
    
    ## Calculate the number of days in the look_back interval
    # nperiods <- rutils::diffit(endp)
    # which_periods <- which.max(table(nperiods))
    # nperiods <- nperiods[which_periods]
    nperiods <- (endp[nrows] - endp[nrows-1])
    nperiods <- nperiods*look_back
    globals$nperiods <- nperiods
    
    list(startp=startp, endp=endp)
    
  })  # end Calculate the end points
  
  
  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    look_back <- input$look_back
    dimax <- input$dimax
    lambda <- input$lambda
    method <- input$method
    alpha <- input$alpha
    confl <- input$confl
    trend <- as.numeric(input$trend)
    
    # Create a named list of model parameters
    controlv <- HighFreq::param_portf(method=method, dimax=dimax, alpha=alpha, confl=confl)

    # Model is recalculated when the recalcb variable is updated
    # input$recalcb
    
    rets <- datav()$rets
    excess <- excess()
    startp <- roll_points()$startp
    endp <- roll_points()$endp
    
    if (method == "ranksimple") {
      cat("Rank simple model \n")
      # Run rank model
      # posit <- matrix(rep(NA_integer_, nrows*ncols), ncol=ncols)
      # posit[1, ] <- 0
      # Reset the positions according to the sort data in excess
      posit <- matrixStats::rowRanks(excess)
      # Reset the positions only at the endp and hold the position between the endp
      # posit[endp, ] <- excess[endp, ]
      # posit <- zoo::na.locf(posit, na.rm=FALSE)
      posit <- (posit - rowMeans(posit))
      posit <- HighFreq::lagit(posit, lagg=1)
      pnls <- trend*posit*rets
      pnls <- rowMeans(pnls)
    } else if (method == "rank_hold") {
      cat("Rank hold model \n")
      # Run rank and hold model
      # posit <- matrix(rep(NA_integer_, nrows*ncols), ncol=ncols)
      # posit[1, ] <- 0
      # Reset the positions according to the sort data in excess
      posit <- matrixStats::rowRanks(excess)
      # Reset the positions only at the endp and hold the position between the endp
      # posit[endp, ] <- excess[endp, ]
      # posit <- zoo::na.locf(posit, na.rm=FALSE)
      posit <- (posit - rowMeans(posit))
      # Average the past posit to reflect holding the position for some time
      posit <- HighFreq::roll_sum(posit, look_back=look_back)
      posit <- HighFreq::lagit(posit, lagg=1)
      pnls <- trend*posit*rets
      pnls <- rowMeans(pnls)
    } else if (input$interval == "days") {
      cat("Daily HighFreq::back_test() \n")
      # Rerun the strategy with fixed start date
      pnls <- HighFreq::back_test(excess=excess,
                                  returns=rets,
                                  startp=startp-1,
                                  endp=endp-1,
                                  lambda=lambda,
                                  controlv=controlv,
                                  coeff=trend)
      pnls[which(is.na(pnls)), ] <- 0
    } else {
      # Rerun the strategy with multiple start dates
      # endpoint <- endp[nrows]
      # nperiods <- (endpoint - endp[nrows-1] - 1)
      # 
      # pnls <- lapply(1:nperiods, function(shiftv) {
      #   ep_new <- c(endp-shiftv, endpoint)
      #   sp_new <- c(rep_len(1, look_back-1), ep_new[1:(nrows-look_back+2)])
      #   pnls <- HighFreq::back_test(excess=excess, 
      #                                returns=rets,
      #                                startp=sp_new-1,
      #                                endp=ep_new-1,
      #                                confl=confl,
      #                                dimax=dimax, 
      #                                alpha=alpha, 
      #                                method=method,
      #                                coeff=trend)
      #   pnls[which(is.na(pnls)), ] <- 0
      #   pnls
      # })  # end lapply
      # 
      # # Calculate the average of pnls of strategies with different start dates
      # pnls <- do.call(cbind, pnls)
      # pnls <- rowMeans(pnls)
      
      cat("HighFreq::back_test() \n")
      # Rerun the strategy with fixed start date
      pnls <- HighFreq::back_test(excess=excess,
                                  returns=rets,
                                  startp=startp-1,
                                  endp=endp-1,
                                  lambda=lambda,
                                  controlv=controlv,
                                  coeff=trend)
      pnls[which(is.na(pnls)), ] <- 0
    }  # end if
  
    pnls <- globals$stdev*pnls/sd(pnls[pnls<0])
    pnls <- cbind(pnls, datav()$indeks)
    # sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x))
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 3)
    pnls <- cumsum(pnls)
    # pnls <- cumprod(1 + pnls)
    colnames(pnls) <- c("Strategy", "Index")
    captiont <- paste0(c("Strategy SR = ", "Index SR = "), sharper)
    captiont <- paste("Rolling Portfolio Strategy: ", paste(captiont, collapse=" and "))
    list(captiont=captiont, pnls=pnls[c(1, endp), ])
    
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    captiont <- pnls()$captiont
    pnls <- pnls()$pnls
    colnamev <- colnames(pnls)
    dygraphs::dygraph(pnls, main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="blue") %>%
      dyLegend(width=300)
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
