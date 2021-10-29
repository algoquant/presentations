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
# source("C:/Develop/lecture_slides/scripts/roll_portf.R")


# Load S&P500 stock returns
# load("C:/Develop/data/returns_100.RData")
# cat("sp500 init load \n")
data_name_old <- NULL
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")



## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  # titlePanel(cap_tion),
  # titlePanel("Rolling Portfolio Optimization Strategy for ETF Portfolio or for Sub-Portfolio of S&P500 Stocks"),
  titlePanel("Rolling Portfolio Optimization Strategy"),
  
  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the re_calculate variable is updated
    column(width=12, 
           h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
           actionButton("re_calculate", "Recalculate the Model"))
  ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input choice of data
    column(width=2, selectInput("data_name", label="Data",
                                choices=c("etf", "sp500"), selected="etf")),
    # Input choice of model
    column(width=2, selectInput("model_type", label="Model type",
                                choices=c("rank_sharpe", "max_sharpe", "max_sharpe_median", "min_var", "min_varpca", "rank_simple", "rank_hold", "rank", "rankrob", "quan_tile"), selected="rank_sharpe")),
    # Input end points interval
    column(width=2, selectInput("inter_val", label="End points Interval",
                                choices=c("days", "weeks", "months", "years"), selected="weeks")),
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Lookback interval",
                                min=2, max=40, value=5, step=1)),
    # Input Weight decay for filtering returns
    column(width=2, sliderInput("lamb_da", label="Weight decay:",
                                min=0.01, max=0.99, value=0.99, step=0.05)),
    # Input exponent for variance
    column(width=2, sliderInput("expo_nent", label="Variance exponent:",
                                min=0.25, max=1.5, value=1.0, step=0.05)),
    # Input number of eigenvalues for regularized matrix inverse
    column(width=2, numericInput("eigen_max", "Number of eigenvalues", value=11)),
    # Input the shrinkage intensity
    column(width=2, sliderInput("al_pha", label="Shrinkage intensity",
                                min=0.01, max=0.99, value=0.01, step=0.05)),
    # Input the quantile
    column(width=2, sliderInput("pro_b", label="Confidence level",
                                min=0.01, max=0.49, value=0.25, step=0.01)),
    # Input choice of ex_cess returns
    column(width=2, selectInput("returns_scaling", label="Excess returns scaling",
                                choices=c("none", "sum", "re_scaled", "volatility", "sharpe", "skew"), selected="none")),
    # If co_eff=1 then trending, If co_eff=(-1) then contrarian
    # column(width=2, numericInput("co_eff", "Trend coefficient:", value=1)),
    column(width=2, selectInput("co_eff", label="Trend coefficient",
                                choices=c(1, -1), selected=(-1)))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph"), width=12)
)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {
  
  # Recalculate the data and rerun the model
  da_ta <- reactive({
    # get model parameters from input argument
    data_name <- isolate(input$data_name)
    inter_val <- isolate(input$inter_val)
    eigen_max <- isolate(input$eigen_max)
    look_back <- isolate(input$look_back)
    lamb_da <- isolate(input$lamb_da)
    expo_nent <- isolate(input$expo_nent)
    model_type <- isolate(input$model_type)
    al_pha <- isolate(input$al_pha)
    pro_b <- isolate(input$pro_b)
    returns_scaling <- isolate(input$returns_scaling)
    co_eff <- as.numeric(isolate(input$co_eff))
    # Model is recalculated when the re_calculate variable is updated
    input$re_calculate
    
    # Load data if needed
    switch(data_name,
           "etf" = {
             cap_tion <- "Rolling Portfolio Optimization Strategy for ETF Portfolio"
             # cap_tion <- paste("Contrarian Strategy for", sym_bol, "Using the Hampel Filter Over Prices")
             # Load PCA data
             # load("C:/Develop/data/pca_rets.RData")
             # ret_s <- pca_rets[, 1:9]
             # Load ETF data
             # sym_bols <- c("VXX", "VEU", "GLD", "EEM", "IEF", "DBC", "TLT", "SVXY", "VYM", "USO", "MTUM", "IWB", "IWD", "VTI")
             # sym_bols <- c("VEU", "GLD", "EEM", "DBC", "VYM", "USO", "IWB", "IWD", "VTI")
             # Select all the ETF symbols except "VXX", "SVXY" and "MTUM"
             sym_bols <- rutils::etf_env$sym_bols
             sym_bols <- sym_bols[!(sym_bols %in% c("TLT", "IEF"))]
             # sym_bols <- sym_bols[!(sym_bols %in% c("TLT", "IEF", "VXX", "SVXY", "MTUM"))]
             # sym_bols <- sym_bols[!(sym_bols %in% c("VXX", "SVXY", "MTUM"))]
             ret_s <- rutils::etf_env$re_turns[, sym_bols]
             # model_type <- "max_sharpe"
             # look_back <- 8
             # look_back_max <- 71
             # eigen_max <- 5
             # al_pha <- 0.01
             # ret_s <- ret_s["2001-06-02/"]
           },
           "sp500" = {
             cap_tion <- "Rolling Portfolio Optimization Strategy for Sub-Portfolio of S&P500 Stocks"
             # Select data after 2000
             ret_s <- returns_100["2000/"]
             # Copy over NA values
             ret_s[1, is.na(ret_s[1, ])] <- 0
             ret_s <- zoo::na.locf(ret_s, na.rm=FALSE)
             n_rows <- NROW(ret_s)
             n_cols <- NCOL(ret_s)
             # Select the columns with non-zero returns
             ret_s <- ret_s[, !(ret_s[n_cols %/% 10, ] == 0)]
             # Select 100 columns to reduce computations
             # set.seed(1121)  # Reset random number generator
             # sam_ple <- sample(1:NCOL(ret_s), 100)
             # ret_s <- ret_s[, sam_ple]
             # ret_s <- cbind(ret_s, rutils::etf_env$ret_s$SVXY, rutils::etf_env$ret_s$VXX)
             # model_type <- "rank"
             # look_back <- 5
             # look_back_max <- 50
             # eigen_max <- 35
             # al_pha <- 0.01
           }
    )  # end switch
    
    # Copy over NA values
    ret_s[1, is.na(ret_s[1, ])] <- 0
    ret_s <- zoo::na.locf(ret_s, na.rm=FALSE)
    n_rows <- NROW(ret_s)
    n_cols <- NCOL(ret_s)
    
    # Random data
    # coredata(ret_s) <- matrix(rnorm(prod(dim(ret_s)))/100, nc=n_cols)
    
    risk_free <- 0.03/260
    
    # Calculate returns on equal weight portfolio
    # in_dex <- xts(cumprod(1 + rowMeans(ret_s)), index(ret_s))
    in_dex <- rowMeans(ret_s)
    std_dev <- sd(in_dex[in_dex<0])
    # sharp_e <- sqrt(252)*mean(in_dex)/std_dev
    in_dex <- xts(in_dex, index(ret_s))
    
    
    # Define end points
    end_points <- rutils::calc_endpoints(ret_s, inter_val=inter_val)
    # end_points <- ifelse(end_points<(n_cols+1), n_cols+1, end_points)
    end_points <- end_points[end_points > (n_cols+1)]
    len_gth <- NROW(end_points)
    # Define start_points
    start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
    
    ## Calculate the number of days in the look_back interval
    # n_periods <- rutils::diff_it(end_points)
    # which_periods <- which.max(table(n_periods))
    # n_periods <- n_periods[which_periods]
    end_point <- end_points[len_gth]
    n_periods <- (end_point - end_points[len_gth-1])
    n_periods <- n_periods*look_back
    
    # Scale the returns and call them ex_cess
    switch(returns_scaling,
           "none" = {
             ex_cess <- (ret_s - risk_free)
             # ex_cess <- returns_100
           },
           "sum" = {
             # Calculate trailing sum
             ex_cess <- HighFreq::roll_sum(ret_s, look_back=n_periods)
           },
           "re_scaled" = {
             # Scale the ret_s by the trailing volatility
             ex_cess <- ret_s/(HighFreq::roll_var(ret_s, look_back=n_periods))^expo_nent
             ex_cess[!is.finite(ex_cess)] <- 0.1
           },
           "volatility" = {
             # Calculate trailing volatilities
             ex_cess <- HighFreq::roll_var(ret_s, look_back=n_periods)^expo_nent
             ex_cess[!is.finite(ex_cess)] <- 0.1
           },
           "sharpe" = {
             # Calculate trailing Sharpe ratios
             # ex_cess <- HighFreq::roll_sum(ret_s, look_back=n_periods)/n_periods/(HighFreq::roll_var(ret_s, look_back=n_periods))^expo_nent
             # ex_cess[!is.finite(ex_cess)] <- 0.1
             vo_l <- HighFreq::roll_var(ret_s, look_back=n_periods)^expo_nent
             vo_l[vo_l == 0] <- 1
             ex_cess <- HighFreq::roll_sum(ret_s, look_back=n_periods)/n_periods
             ex_cess <- ex_cess/vo_l
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
             # Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/roll_skew.cpp")
             # rolling_skew <- roll_kurtosis(ret_s, look_back=n_periods)
             # rolling_skew <- roll_skew(ret_s, look_back=n_periods)
             # rolling_skew <- roll_skew(ret_s, typ_e="quantile", al_pha=pro_b, look_back=n_periods)
             # rolling_skew[!is.finite(rolling_skew)] <- 0
             # rolling_skew <- na.locf(rolling_skew)

             # Best performing stats so far
             ex_cess <- ma_x / medi_an
             # ex_cess <- rolling_skew
             # ex_cess <- (up_sd - down_sd)
             # ex_cess <- (ma_x - medi_an)
             # ex_cess <- ma_x/me_an^expo_nent
             # ex_cess <- ma_x - mi_n
             # ex_cess <- (ma_x + mi_n - 2*medi_an) / (ma_x - mi_n)
             # ex_cess <- (ma_x - medi_an) / (medi_an - mi_n)
             # ex_cess <- (me_an - medi_an)
             ex_cess[is.infinite(ex_cess)] <- 0
             ex_cess[is.na(ex_cess)] <- 0
             # Pad zeros up-front
             ex_cess <- rbind(matrix(1:((n_periods-1)*n_cols), ncol=n_cols),
                              ex_cess)
           }
    )  # end switch
    
    # Filter the ex_cess returns using an exponential weighted filter - doesn't provide significant improvement
    # Calculate the weight_s
    # weight_s <- exp(-lamb_da*(1:look_back))
    # weight_s <- weight_s/sum(weight_s)
    # weight_s <- matrix(weight_s, nc=1)
    # Calculate smoothed ex_cess returns
    # ex_cess <- HighFreq::roll_conv(ex_cess, weight_s=weight_s)

    
    if (model_type == "rank_simple") {
      # Run rank model
      # position_s <- matrix(rep(NA_integer_, n_rows*n_cols), ncol=n_cols)
      # position_s[1, ] <- 0
      # Reset the positions according to the sort data in ex_cess
      position_s <- matrixStats::rowRanks(ex_cess)
      position_s <- HighFreq::lag_it(position_s, lagg=1)
      # Reset the positions only at the end_points and hold the position between the end_points
      # position_s[end_points, ] <- ex_cess[end_points, ]
      # position_s <- zoo::na.locf(position_s, na.rm=FALSE)
      position_s <- (position_s - rowMeans(position_s))
      pnl_s <- co_eff*position_s*ret_s
      pnl_s <- rowMeans(pnl_s)
    } else if (model_type == "rank_hold") {
      # Run rank and hold model
      # position_s <- matrix(rep(NA_integer_, n_rows*n_cols), ncol=n_cols)
      # position_s[1, ] <- 0
      # Reset the positions according to the sort data in ex_cess
      position_s <- matrixStats::rowRanks(ex_cess)
      position_s <- HighFreq::lag_it(position_s, lagg=1)
      # Reset the positions only at the end_points and hold the position between the end_points
      # position_s[end_points, ] <- ex_cess[end_points, ]
      # position_s <- zoo::na.locf(position_s, na.rm=FALSE)
      position_s <- (position_s - rowMeans(position_s))
      # Average the past position_s to reflect holding the position for some time
      position_s <- HighFreq::roll_sum(position_s, look_back=look_back)
      pnl_s <- co_eff*position_s*ret_s
      pnl_s <- rowMeans(pnl_s)
    } else if (inter_val == "days") {
      # Rerun the strategy with fixed start date
      pnl_s <- HighFreq::back_test(excess=ex_cess,
                                   returns=ret_s,
                                   startp=start_points-1,
                                   endp=end_points-1,
                                   con_fi=pro_b,
                                   eigen_max=eigen_max,
                                   alpha=al_pha,
                                   method=model_type,
                                   coeff=co_eff)
      pnl_s[which(is.na(pnl_s)), ] <- 0
    } else {
      # Rerun the strategy with multiple start dates
      # end_point <- end_points[len_gth]
      # n_periods <- (end_point - end_points[len_gth-1] - 1)
      # 
      # pnl_s <- lapply(1:n_periods, function(shif_t) {
      #   ep_new <- c(end_points-shif_t, end_point)
      #   sp_new <- c(rep_len(1, look_back-1), ep_new[1:(len_gth-look_back+2)])
      #   pnl_s <- HighFreq::back_test(ex_cess=ex_cess, 
      #                                re_turns=ret_s,
      #                                start_points=sp_new-1,
      #                                end_points=ep_new-1,
      #                                pro_b=pro_b,
      #                                eigen_max=eigen_max, 
      #                                al_pha=al_pha, 
      #                                model_type=model_type,
      #                                co_eff=co_eff)
      #   pnl_s[which(is.na(pnl_s)), ] <- 0
      #   pnl_s
      # })  # end lapply
      # 
      # # Calculate the average of pnls of strategies with different start dates
      # pnl_s <- do.call(cbind, pnl_s)
      # pnl_s <- rowMeans(pnl_s)
      
      # cat("HighFreq::back_test() \n")
      # Rerun the strategy with fixed start date
      pnl_s <- HighFreq::back_test(excess=ex_cess,
                                   returns=ret_s,
                                   startp=start_points-1,
                                   endp=end_points-1,
                                   con_fi=pro_b,
                                   eigen_max=eigen_max,
                                   alpha=al_pha,
                                   method=model_type,
                                   coeff=co_eff)
      pnl_s[which(is.na(pnl_s)), ] <- 0
    }  # end if
  
    pnl_s <- std_dev*pnl_s/sd(pnl_s[pnl_s<0])
    pnl_s <- cbind(pnl_s, in_dex)
    # sharp_e <- sqrt(252)*sapply(pnl_s, function(x) mean(x)/sd(x))
    sharp_e <- sqrt(252)*sapply(pnl_s, function(x) mean(x)/sd(x[x<0]))
    sharp_e <- round(sharp_e, 3)
    pnl_s <- cumsum(pnl_s)
    # pnl_s <- cumprod(1 + pnl_s)
    colnames(pnl_s) <- c("Strategy", "Index")
    cap_tion <- paste0(c("Strategy SR = ", "Index SR = "), sharp_e)
    cap_tion <- paste("Rolling Portfolio Strategy: ", paste(cap_tion, collapse=" and "))
    list(cap_tion=cap_tion, pnl_s=pnl_s[c(1, end_points), ])
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph({
    cap_tion <- da_ta()$cap_tion
    pnl_s <- da_ta()$pnl_s
    col_names <- colnames(pnl_s)
    dygraphs::dygraph(pnl_s, main=cap_tion) %>%
      dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
      dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
      dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue") %>%
      dyLegend(width=300)
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
