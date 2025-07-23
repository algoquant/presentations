##############################
# This is a shiny app for simulating a rolling portfolio 
# optimization strategy with filtering of returns.
# It uses HighFreq::roll_portf()
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
# lookb <- 5
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
                                choices=c("etf", "sp500"), selected="etf")),
    # Input choice of model
    column(width=2, selectInput("method", label="Model type",
                                choices=c("sharpem", "kellym", "maxsharpe", "maxsharpemed", "minvarlin", "minvarquad", "ranksimple", "rank_hold", "robustm", "quantilev"), selected="maxsharpe")),
    # Input end points interval
    column(width=2, selectInput("interval", label="End points Interval",
                                choices=c("days", "weeks", "months", "years"), selected="months")),
    # Input look-back interval
    column(width=2, sliderInput("lookb", label="Lookback interval",
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
    column(width=2, selectInput("scalingm", label="Excess returns scaling",
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
             # retp <- pcarets[, 1:9]
             # Load ETF data
             # symbolv <- c("VXX", "VEU", "GLD", "EEM", "IEF", "DBC", "TLT", "SVXY", "VYM", "USO", "MTUM", "IWB", "IWD", "VTI")
             # symbolv <- c("VEU", "GLD", "EEM", "DBC", "VYM", "USO", "IWB", "IWD", "VTI")
             # Select all the ETF symbols except "VXX", "SVXY" and "MTUM"
             symbolv <- rutils::etfenv$symbolv
             symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV", "VTV", "AIEQ", "GLD"))]
             # symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "TLT", "IEF", "MTUM", "QUAL", "VLUE", "USMV", "VTV", "AIEQ", "GLD"))]
             # symbolv <- symbolv[!(symbolv %in% c("TLT", "IEF", "VXX", "SVXY", "MTUM"))]
             # symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM"))]
             retp <- rutils::etfenv$returns[, symbolv]
             # method <- "max_sharpe"
             # lookb <- 8
             # lookb_max <- 71
             # dimax <- 5
             # alpha <- 0.01
             # retp <- retp["2001-06-02/"]
           },
           "sp500" = {
             cat("Loading S&P500 data \n")
             captiont <- "Rolling Portfolio Optimization Strategy for Sub-Portfolio of S&P500 Stocks"
             # Load S&P500 stock returns
             # cat("sp500 init load \n")
             # load("/Users/jerzy/Develop/lecture_slides/data/returns100.RData")
             load("/Users/jerzy/Develop/data/sp500_returns.RData")
             # Select data after 2000
             retp <- returns100["2000/"]
             # Copy over NA values
             # retp[1, is.na(retp[1, ])] <- 0
             # retp <- zoo::na.locf(retp, na.rm=FALSE)
             # nrows <- NROW(retp)
             # ncols <- NCOL(retp)
             # Select the columns with non-zero returns
             # retp <- retp[, !(retp[ncols %/% 10, ] == 0)]
             # Select 100 columns to reduce computations
             # set.seed(1121)  # Reset random number generator
             # samplev <- sample(1:NCOL(retp), 100)
             # retp <- retp[, samplev]
             # retp <- cbind(retp, rutils::etfenv$returns$SVXY, rutils::etfenv$returns$VXX)
             # method <- "rank"
             # lookb <- 5
             # lookb_max <- 50
             # dimax <- 35
             # alpha <- 0.01
           }
    )  # end switch
    
    # Copy over NA values
    retp[1, is.na(retp[1, ])] <- 0
    retp <- zoo::na.locf(retp, na.rm=FALSE)
    nrows <- NROW(retp)
    ncols <- NCOL(retp)
    globals$nrows <- nrows
    globals$ncols <- ncols
    
    # Random data
    # coredata(retp) <- matrix(rnorm(prod(dim(retp)))/100, nc=ncols)
    
    # Calculate returns on equal weight portfolio
    # indeks <- xts(cumprod(1 + rowMeans(retp)), index(retp))
    indeks <- rowMeans(retp)
    # sharper <- sqrt(252)*mean(indeks)/stdev
    indeks <- xts(indeks, index(retp))
    # stdev <- sd(indeks[indeks<0])
    globals$stdev <- sd(indeks[indeks<0])

    list(retp=retp, indeks=indeks)
    
  })  # end Load the data
  
  
  ## Calculate the excess returns
  retx <- shiny::reactive({
    cat("Calculating the excess returns", "\n")
    
    scalingm <- input$scalingm
    exponent <- input$exponent
    interval <- input$interval
    
    retp <- datav()$retp
    nperiods <- globals$nperiods
    
    # Scale the returns and call them excess
    switch(scalingm,
           "none" = {
             cat("No returns scaling \n")
             retx <- (retp - riskf)
             # retx <- returns100
           },
           "sum" = {
             # Calculate trailing sum
             retx <- HighFreq::roll_sum(retp, lookb=nperiods)
           },
           "rescaled" = {
             # Scale the returns by the trailing volatility
             retx <- retp/(HighFreq::roll_var(retp, lookb=nperiods))^exponent
             retx[!is.finite(retx)] <- 0.1
           },
           "volatility" = {
             # Calculate trailing volatilities
             retx <- HighFreq::roll_var(retp, lookb=nperiods)^exponent
             retx[!is.finite(retx)] <- 0.1
           },
           "sharpe" = {
             # Calculate trailing Sharpe ratios
             # retx <- HighFreq::roll_sum(retp, lookb=nperiods)/nperiods/(HighFreq::roll_var(retp, lookb=nperiods))^exponent
             # retx[!is.finite(retx)] <- 0.1
             volat <- HighFreq::roll_var(retp, lookb=nperiods)^exponent
             volat[volat == 0] <- 1
             retx <- HighFreq::roll_sum(retp, lookb=nperiods)/nperiods
             retx <- retx/volat
           },
           "skew" = {
             ## Calculate the skew-like stats
             maxv <- RcppRoll::rolregmodax(retp, n=nperiods, align="right")
             # minv <- -RcppRoll::rolregmodax(-retp, n=nperiods, align="right")
             # meanv <- RcppRoll::roll_mean(retp, n=nperiods, align="right")
             medianv <- RcppRoll::roll_median(retp, n=nperiods, align="right")
             # Calculate difference between upside minus downside volatility
             # core_data <- coredata(retp)
             # upsd <- RcppRoll::roll_sd(ifelse(core_data>0, core_data, 0), n=nperiods, align="right")
             # downsd <- RcppRoll::roll_sd(ifelse(core_data<0, core_data, 0), n=nperiods, align="right")
             # Calculate rolling skew using Rcpp
             # First compile this file in R by running this command:
             # Rcpp::sourceCpp(file="/Users/jerzy/Develop/R/Rcpp/roll_skew.cpp")
             # rolling_skew <- roll_kurtosis(retp, lookb=nperiods)
             # rolling_skew <- roll_skew(retp, lookb=nperiods)
             # rolling_skew <- roll_skew(retp, typev="quantile", alpha=confl, lookb=nperiods)
             # rolling_skew[!is.finite(rolling_skew)] <- 0
             # rolling_skew <- na.locf(rolling_skew)

             # Best performing stats so far
             retx <- maxv / medianv
             # retx <- rolling_skew
             # retx <- (upsd - downsd)
             # retx <- (maxv - medianv)
             # retx <- maxv/meanv^exponent
             # retx <- maxv - minv
             # retx <- (maxv + minv - 2*medianv) / (maxv - minv)
             # retx <- (maxv - medianv) / (medianv - minv)
             # retx <- (meanv - medianv)
             retx[is.infinite(retx)] <- 0
             retx[is.na(retx)] <- 0
             # Pad zeros up-front
             retx <- rbind(matrix(1:((nperiods-1)*globals$ncols), ncol=globals$ncols), retx)
           }
    )  # end switch
    
    # Filter the excess returns using an exponential weighted filter - doesn't provide significant improvement
    # Calculate the weights
    # weights <- exp(-lambda*(1:lookb))
    # weights <- weights/sum(weights)
    # weights <- matrix(weights, nc=1)
    # Calculate smoothed excess returns
    # retx <- HighFreq::roll_conv(retx, weightv=weights)
    
    retx

  })  # end Calculate the excess returns
  
  
  ## Calculate the end points
  roll_points <- shiny::reactive({
    cat("Calculating the end points", "\n")
    
    interval <- input$interval
    lookb <- input$lookb
    retp <- datav()$retp
    
    # Define end points
    endd <- rutils::calc_endpoints(retp, interval=interval)
    # endd <- ifelse(endd< ncols+1), ncols+1, endd)
    endd <- endd[endd > (globals$ncols+1)]
    nrows <- NROW(endd)
    # Define startp
    # Rolling window
    startp <- c(rep_len(1, lookb-1), endd[1:(nrows-lookb+1)])
    # Expanding window
    # startp <- rep_len(1, nrows)
    
    ## Calculate the number of days in the lookb interval
    # nperiods <- rutils::diffit(endd)
    # which_periods <- which.max(table(nperiods))
    # nperiods <- nperiods[which_periods]
    nperiods <- (endd[nrows] - endd[nrows-1])
    nperiods <- nperiods*lookb
    globals$nperiods <- nperiods
    
    list(startp=startp, endd=endd)
    
  })  # end Calculate the end points
  
  
  # Recalculate the strategy
  pnls <- shiny::reactive({
    
    lookb <- input$lookb
    dimax <- input$dimax
    lambda <- input$lambda
    method <- input$method
    alpha <- input$alpha
    confl <- input$confl
    trend <- as.numeric(input$trend)
    
    # Create a named list of model parameters
    controll <- HighFreq::param_portf(method=method, dimax=dimax, alpha=alpha, confl=confl)

    # Model is recalculated when the recalcb variable is updated
    # input$recalcb
    
    retp <- datav()$retp
    retx <- retx()
    startp <- roll_points()$startp
    endd <- roll_points()$endd
    
    if (method == "ranksimple") {
      cat("Rank simple model \n")
      # Run rank model
      # posv <- matrix(rep(NA_integer_, nrows*ncols), ncol=ncols)
      # posv[1, ] <- 0
      # Reset the positions according to the sort data in excess
      posv <- matrixStats::rowRanks(retx)
      # Reset the positions only at the endd and hold the position between the endd
      # posv[endd, ] <- retx[endd, ]
      # posv <- zoo::na.locf(posv, na.rm=FALSE)
      posv <- (posv - rowMeans(posv))
      posv <- HighFreq::lagit(posv, lagg=1)
      pnls <- trend*posv*retp
      pnls <- rowMeans(pnls)
    } else if (method == "rank_hold") {
      cat("Rank hold model \n")
      # Run rank and hold model
      # posv <- matrix(rep(NA_integer_, nrows*ncols), ncol=ncols)
      # posv[1, ] <- 0
      # Reset the positions according to the sort data in excess
      posv <- matrixStats::rowRanks(retx)
      # Reset the positions only at the endd and hold the position between the endd
      # posv[endd, ] <- retx[endd, ]
      # posv <- zoo::na.locf(posv, na.rm=FALSE)
      posv <- (posv - rowMeans(posv))
      # Average the past posv to reflect holding the position for some time
      posv <- HighFreq::roll_sum(posv, lookb=lookb)
      posv <- HighFreq::lagit(posv, lagg=1)
      pnls <- trend*posv*retp
      pnls <- rowMeans(pnls)
    } else if (input$interval == "days") {
      cat("Daily HighFreq::roll_portf() \n")
      # Rerun the strategy with fixed start date
      pnls <- HighFreq::roll_portf(retx=retx,
                                  retp=retp,
                                  startp=startp-1,
                                  endd=endd-1,
                                  lambda=lambda,
                                  controll=controll,
                                  coeff=trend)
      pnls[which(is.na(pnls)), ] <- 0
    } else {
      # Rerun the strategy with multiple start dates
      # endpoint <- endd[nrows]
      # nperiods <- (endpoint - endd[nrows-1] - 1)
      # 
      # pnls <- lapply(1:nperiods, function(shiftv) {
      #   ep_new <- c(endd-shiftv, endpoint)
      #   sp_new <- c(rep_len(1, lookb-1), ep_new[1:(nrows-lookb+2)])
      #   pnls <- HighFreq::roll_portf(retx=retx, 
      #                                retp=retp,
      #                                startp=sp_new-1,
      #                                endd=ep_new-1,
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
      
      cat("HighFreq::roll_portf() \n")
      # Rerun the strategy with fixed start date
      pnls <- HighFreq::roll_portf(retx=retx,
                                  retp=retp,
                                  startp=startp-1,
                                  endd=endd-1,
                                  lambda=lambda,
                                  controll=controll,
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
    list(captiont=captiont, pnls=pnls[c(1, endd), ])
    
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    captiont <- pnls()$captiont
    pnls <- pnls()$pnls
    colnamev <- colnames(pnls)
    dygraphs::dygraph(pnls, main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", strokeWidth=2, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", strokeWidth=2, col="blue") %>%
      dyLegend(width=300)
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
