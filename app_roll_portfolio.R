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
# lambdav <- 0.01
# model_type <- "max_sharpe"
# conf_lev <- 0.25
# eigen_max <- 11
# alpha <- 0.01


## End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  # titlePanel(cap_tion),
  # titlePanel("Rolling Portfolio Optimization Strategy for ETF Portfolio or for Sub-Portfolio of S&P500 Stocks"),
  titlePanel("Rolling Portfolio Optimization Strategy"),
  
  # fluidRow(
  #   # The Shiny App is recalculated when the actionButton is clicked and the re_calculate variable is updated
  #   column(width=12, 
  #          h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
  #          actionButton("re_calculate", "Recalculate the Model"))
  # ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input choice of data
    column(width=2, selectInput("data_name", label="Data",
                                choices=c("etf", "sp500"), selected="sp500")),
    # Input choice of model
    column(width=2, selectInput("model_type", label="Model type",
                                choices=c("ranksharpe", "max_sharpe", "max_sharpe_median", "min_var", "min_varpca", "ranksimple", "rank_hold", "rank", "rankrob", "quan_tile"), selected="ranksimple")),
    # Input end points interval
    column(width=2, selectInput("interval", label="End points Interval",
                                choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Lookback interval",
                                min=2, max=40, value=3, step=1)),
    # Input decay factor for averaging the portfolio weights
    column(width=2, sliderInput("lambdav", label="Decay factor:",
                                min=0.01, max=0.99, value=0.01, step=0.05)),
    # Input exponent for variance
    column(width=2, sliderInput("expo_nent", label="Variance exponent:",
                                min=0.25, max=1.5, value=1.0, step=0.05)),
    # Input number of eigenvalues for regularized matrix inverse
    column(width=2, numericInput("eigen_max", "Number of eigenvalues", value=11)),
    # Input the shrinkage intensity
    column(width=2, sliderInput("alpha", label="Shrinkage intensity",
                                min=0.01, max=0.99, value=0.01, step=0.05)),
    # Input the quantile
    column(width=2, sliderInput("conf_lev", label="Confidence level",
                                min=0.01, max=0.49, value=0.25, step=0.01)),
    # Input choice of excess returns
    column(width=2, selectInput("returns_scaling", label="Excess returns scaling",
                                choices=c("none", "sum", "rescaled", "volatility", "sharpe", "skew"), selected="none")),
    # If trend=1 then trending, If trend=(-1) then contrarian
    column(width=2, selectInput("trend", label="Trend coefficient",
                                choices=c(1, -1), selected=(-1)))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot", width="100%", height="600px"), height=10, width=12)
  # Create output plot panel
  # mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
  
)  # end fluidPage interface


## Define the server code
servfunc <- function(input, output) {
  
  ## Create an empty list of reactive values.
  globals <- reactiveValues()
  
  # Load the data
  datav <- reactive({
    # Get model parameters from input argument
    data_name <- input$data_name
    
    # Load data if needed
    switch(data_name,
           "etf" = {
             cat("Loading ETF data \n")
             cap_tion <- "Rolling Portfolio Optimization Strategy for ETF Portfolio"
             # cap_tion <- paste("Contrarian Strategy for", symbol, "Using the Hampel Filter Over Prices")
             # Load PCA data
             # load("/Users/jerzy/Develop/data/pcarets.RData")
             # ret_s <- pcarets[, 1:9]
             # Load ETF data
             # symbolv <- c("VXX", "VEU", "GLD", "EEM", "IEF", "DBC", "TLT", "SVXY", "VYM", "USO", "MTUM", "IWB", "IWD", "VTI")
             # symbolv <- c("VEU", "GLD", "EEM", "DBC", "VYM", "USO", "IWB", "IWD", "VTI")
             # Select all the ETF symbols except "VXX", "SVXY" and "MTUM"
             symbolv <- rutils::etfenv$symbolv
             symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "TLT", "IEF", "MTUM", "QUAL", "VLUE", "USMV"))]
             # symbolv <- symbolv[!(symbolv %in% c("TLT", "IEF", "VXX", "SVXY", "MTUM"))]
             # symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM"))]
             ret_s <- rutils::etfenv$returns[, symbolv]
             # model_type <- "max_sharpe"
             # look_back <- 8
             # look_back_max <- 71
             # eigen_max <- 5
             # alpha <- 0.01
             # ret_s <- ret_s["2001-06-02/"]
           },
           "sp500" = {
             cat("Loading S&P500 data \n")
             cap_tion <- "Rolling Portfolio Optimization Strategy for Sub-Portfolio of S&P500 Stocks"
             # Load S&P500 stock returns
             # cat("sp500 init load \n")
             # load("/Users/jerzy/Develop/lecture_slides/data/returns100.RData")
             load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
             # Select data after 2000
             ret_s <- returns["2000/"]
             # Copy over NA values
             ret_s[1, is.na(ret_s[1, ])] <- 0
             ret_s <- zoo::na.locf(ret_s, na.rm=FALSE)
            .n_rows <- NROW(ret_s)
            .n_cols <- NCOL(ret_s)
             # Select the columns with non-zero returns
             ret_s <- ret_s[, !(ret_s.n_cols %/% 10, ] == 0)]
             # Select 100 columns to reduce computations
             # set.seed(1121)  # Reset random number generator
             # sam_ple <- sample(1:NCOL(ret_s), 100)
             # ret_s <- ret_s[, sam_ple]
             # ret_s <- cbind(ret_s, rutils::etfenv$ret_s$SVXY, rutils::etfenv$ret_s$VXX)
             # model_type <- "rank"
             # look_back <- 5
             # look_back_max <- 50
             # eigen_max <- 35
             # alpha <- 0.01
           }
    )  # end switch
    
    # Copy over NA values
    ret_s[1, is.na(ret_s[1, ])] <- 0
    ret_s <- zoo::na.locf(ret_s, na.rm=FALSE)
   .n_rows <- NROW(ret_s)
   .n_cols <- NCOL(ret_s)
    globals.n_rows <-.n_rows
    globals.n_cols <-.n_cols
    
    # Random data
    # coredata(ret_s) <- matrix(rnorm(prod(dim(ret_s)))/100, nc.n_cols)
    
    # Calculate returns on equal weight portfolio
    # indeks <- xts(cumprod(1 + rowMeans(ret_s)), index(ret_s))
    indeks <- rowMeans(ret_s)
    stdev <- sd(indeks[indeks<0])
    globals$stdev <- stdev
    # sharp_e <- sqrt(252)*mean(indeks)/stdev
    indeks <- xts(indeks, index(ret_s))

    list(ret_s=ret_s, indeks=indeks)
    
  })  # end Load the data
  
  
  ## Calculate the excess
  excess <- reactive({
    cat("Calculating the excess", "\n")
    
    returns_scaling <- input$returns_scaling
    expo_nent <- input$expo_nent
    interval <- input$interval
    
    ret_s <- datav()$ret_s
    n_periods <- globals$n_periods
    
    # Scale the returns and call them excess
    switch(returns_scaling,
           "none" = {
             cat("No returns scaling \n")
             excess <- (ret_s - riskf)
             # excess <- returns100
           },
           "sum" = {
             # Calculate trailing sum
             excess <- HighFreq::roll_sum(ret_s, look_back=n_periods)
           },
           "rescaled" = {
             # Scale the ret_s by the trailing volatility
             excess <- ret_s/(HighFreq::roll_var(ret_s, look_back=n_periods))^expo_nent
             excess[!is.finite(excess)] <- 0.1
           },
           "volatility" = {
             # Calculate trailing volatilities
             excess <- HighFreq::roll_var(ret_s, look_back=n_periods)^expo_nent
             excess[!is.finite(excess)] <- 0.1
           },
           "sharpe" = {
             # Calculate trailing Sharpe ratios
             # excess <- HighFreq::roll_sum(ret_s, look_back=n_periods)/n_periods/(HighFreq::roll_var(ret_s, look_back=n_periods))^expo_nent
             # excess[!is.finite(excess)] <- 0.1
             vo_l <- HighFreq::roll_var(ret_s, look_back=n_periods)^expo_nent
             vo_l[vo_l == 0] <- 1
             excess <- HighFreq::roll_sum(ret_s, look_back=n_periods)/n_periods
             excess <- excess/vo_l
           },
           "skew" = {
             ## Calculate the skew-like stats
             ma_x <- RcppRoll::roll_max(ret_s, n=n_periods, align="right")
             # mi_n <- -RcppRoll::roll_max(-ret_s, n=n_periods, align="right")
             # me_an <- RcppRoll::roll_mean(ret_s, n=n_periods, align="right")
             medi_an <- RcppRoll::roll_median(ret_s, n=n_periods, align="right")
             # Calculate difference between upside minus downside volatility
             # core_data <- coredata(ret_s)
             # up_sd <- RcppRoll::roll_sd(ifelse(core_data>0, core_data, 0), n=n_periods, align="right")
             # down_sd <- RcppRoll::roll_sd(ifelse(core_data<0, core_data, 0), n=n_periods, align="right")
             # Calculate rolling skew using Rcpp
             # First compile this file in R by running this command:
             # Rcpp::sourceCpp(file="/Users/jerzy/Develop/R/Rcpp/roll_skew.cpp")
             # rolling_skew <- roll_kurtosis(ret_s, look_back=n_periods)
             # rolling_skew <- roll_skew(ret_s, look_back=n_periods)
             # rolling_skew <- roll_skew(ret_s, typev="quantile", alpha=conf_lev, look_back=n_periods)
             # rolling_skew[!is.finite(rolling_skew)] <- 0
             # rolling_skew <- na.locf(rolling_skew)

             # Best performing stats so far
             excess <- ma_x / medi_an
             # excess <- rolling_skew
             # excess <- (up_sd - down_sd)
             # excess <- (ma_x - medi_an)
             # excess <- ma_x/me_an^expo_nent
             # excess <- ma_x - mi_n
             # excess <- (ma_x + mi_n - 2*medi_an) / (ma_x - mi_n)
             # excess <- (ma_x - medi_an) / (medi_an - mi_n)
             # excess <- (me_an - medi_an)
             excess[is.infinite(excess)] <- 0
             excess[is.na(excess)] <- 0
             # Pad zeros up-front
             excess <- rbind(matrix(1:((n_periods-1)*globals.n_cols), ncol=globals.n_cols), excess)
           }
    )  # end switch
    
    # Filter the excess returns using an exponential weighted filter - doesn't provide significant improvement
    # Calculate the weights
    # weights <- exp(-lambdav*(1:look_back))
    # weights <- weights/sum(weights)
    # weights <- matrix(weights, nc=1)
    # Calculate smoothed excess returns
    # excess <- HighFreq::roll_conv(excess, weights=weights)
    
    excess

  })  # end Calculate the excess
  
  
  ## Calculate the end points
  roll_points <- reactive({
    cat("Calculating the end points", "\n")
    
    interval <- input$interval
    look_back <- input$look_back
    ret_s <- datav()$ret_s
    
    # Define end points
    endpoints <- rutils::calc_endpoints(ret_s, interval=interval)
    # endpoints <- ifelse(endpoints<.n_cols+1),.n_cols+1, endpoints)
    endpoints <- endpoints[endpoints > (globals.n_cols+1)]
   .n_rows <- NROW(endpoints)
    # Define startpoints
    startpoints <- c(rep_len(1, look_back-1), endpoints[1:.n_rows-look_back+1)])
    
    ## Calculate the number of days in the look_back interval
    # n_periods <- rutils::diffit(endpoints)
    # which_periods <- which.max(table(n_periods))
    # n_periods <- n_periods[which_periods]
    n_periods <- (endpoints.n_rows] - endpoints.n_rows-1])
    n_periods <- n_periods*look_back
    globals$n_periods <- n_periods
    
    list(startpoints=startpoints, endpoints=endpoints)
    
  })  # end Calculate the end points
  
  
  # Recalculate the strategy
  pnls <- reactive({
    
    look_back <- input$look_back
    eigen_max <- input$eigen_max
    lambdav <- input$lambdav
    model_type <- input$model_type
    alpha <- input$alpha
    conf_lev <- input$conf_lev
    trend <- as.numeric(input$trend)
    
    # Model is recalculated when the re_calculate variable is updated
    # input$re_calculate
    
    ret_s <- datav()$ret_s
    excess <- excess()
    startpoints <- roll_points()$startpoints
    endpoints <- roll_points()$endpoints
    
    if (model_type == "ranksimple") {
      cat("Rank simple model \n")
      # Run rank model
      # position_s <- matrix(rep(NA_integer_,.n_rows.n_cols), ncol.n_cols)
      # position_s[1, ] <- 0
      # Reset the positions according to the sort data in excess
      position_s <- matrixStats::rowRanks(excess)
      # Reset the positions only at the endpoints and hold the position between the endpoints
      # position_s[endpoints, ] <- excess[endpoints, ]
      # position_s <- zoo::na.locf(position_s, na.rm=FALSE)
      position_s <- (position_s - rowMeans(position_s))
      position_s <- HighFreq::lagit(position_s, lagg=1)
      pnls <- trend*position_s*ret_s
      pnls <- rowMeans(pnls)
    } else if (model_type == "rank_hold") {
      cat("Rank hold model \n")
      # Run rank and hold model
      # position_s <- matrix(rep(NA_integer_,.n_rows.n_cols), ncol.n_cols)
      # position_s[1, ] <- 0
      # Reset the positions according to the sort data in excess
      position_s <- matrixStats::rowRanks(excess)
      # Reset the positions only at the endpoints and hold the position between the endpoints
      # position_s[endpoints, ] <- excess[endpoints, ]
      # position_s <- zoo::na.locf(position_s, na.rm=FALSE)
      position_s <- (position_s - rowMeans(position_s))
      # Average the past position_s to reflect holding the position for some time
      position_s <- HighFreq::roll_sum(position_s, look_back=look_back)
      position_s <- HighFreq::lagit(position_s, lagg=1)
      pnls <- trend*position_s*ret_s
      pnls <- rowMeans(pnls)
    } else if (input$interval == "days") {
      cat("Daily HighFreq::back_test() \n")
      # Rerun the strategy with fixed start date
      pnls <- HighFreq::back_test(excess=excess,
                                   returns=ret_s,
                                   startp=startpoints-1,
                                   endp=endpoints-1,
                                   lambda=lambdav,
                                   conf_lev=conf_lev,
                                   eigen_max=eigen_max,
                                   alpha=alpha,
                                   method=model_type,
                                   coeff=trend)
      pnls[which(is.na(pnls)), ] <- 0
    } else {
      # Rerun the strategy with multiple start dates
      # endpoint <- endpoints.n_rows]
      # n_periods <- (endpoint - endpoints.n_rows-1] - 1)
      # 
      # pnls <- lapply(1:n_periods, function(shiftv) {
      #   ep_new <- c(endpoints-shiftv, endpoint)
      #   sp_new <- c(rep_len(1, look_back-1), ep_new[1:.n_rows-look_back+2)])
      #   pnls <- HighFreq::back_test(excess=excess, 
      #                                returns=ret_s,
      #                                startpoints=sp_new-1,
      #                                endpoints=ep_new-1,
      #                                conf_lev=conf_lev,
      #                                eigen_max=eigen_max, 
      #                                alpha=alpha, 
      #                                model_type=model_type,
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
                                   returns=ret_s,
                                   startp=startpoints-1,
                                   endp=endpoints-1,
                                   lambda=lambdav,
                                   conf_lev=conf_lev,
                                   eigen_max=eigen_max,
                                   alpha=alpha,
                                   method=model_type,
                                   coeff=trend)
      pnls[which(is.na(pnls)), ] <- 0
    }  # end if
  
    pnls <- globals$stdev*pnls/sd(pnls[pnls<0])
    pnls <- cbind(pnls, datav()$indeks)
    # sharp_e <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x))
    sharp_e <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    sharp_e <- round(sharp_e, 3)
    pnls <- cumsum(pnls)
    # pnls <- cumprod(1 + pnls)
    colnames(pnls) <- c("Strategy", "Index")
    cap_tion <- paste0(c("Strategy SR = ", "Index SR = "), sharp_e)
    cap_tion <- paste("Rolling Portfolio Strategy: ", paste(cap_tion, collapse=" and "))
    list(cap_tion=cap_tion, pnls=pnls[c(1, endpoints), ])
    
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    cap_tion <- pnls()$cap_tion
    pnls <- pnls()$pnls
    colnamev <- colnames(pnls)
    dygraphs::dygraph(pnls, main=cap_tion) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue") %>%
      dyLegend(width=300)
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfunc)
