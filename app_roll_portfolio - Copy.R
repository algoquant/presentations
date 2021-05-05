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

# Select the data: "etf" or "sp500"
which_data <- "sp500"
switch(which_data,
       "etf" = {
         cap_tion <- "Rolling Portfolio Optimization Strategy for ETF Portfolio"
         # cap_tion <- paste("Contrarian Strategy for", sym_bol, "Using the Hampel Filter Over Prices")
         # Load PCA data
         # load("C:/Develop/data/pca_rets.RData")
         # re_turns <- pca_rets[, 1:9]
         # Load ETF data
         # sym_bols <- c("VXX", "VEU", "GLD", "EEM", "IEF", "DBC", "TLT", "SVXY", "VYM", "USO", "MTUM", "IWB", "IWD", "VTI")
         # sym_bols <- c("VEU", "GLD", "EEM", "DBC", "VYM", "USO", "IWB", "IWD", "VTI")
         # Select all the ETF symbols except "VXX", "SVXY" and "MTUM"
         sym_bols <- rutils::etf_env$sym_bols
         sym_bols <- sym_bols[!(sym_bols %in% c("TLT", "IEF"))]
         # sym_bols <- sym_bols[!(sym_bols %in% c("TLT", "IEF", "VXX", "SVXY", "MTUM"))]
         # sym_bols <- sym_bols[!(sym_bols %in% c("VXX", "SVXY", "MTUM"))]
         re_turns <- rutils::etf_env$re_turns[, sym_bols]
         model_type <- "max_sharpe"
         look_back <- 8
         look_back_max <- 71
         max_eigen <- 5
         al_pha <- 0.01
         # re_turns <- re_turns["2001-06-02/"]
       },
       "sp500" = {
         cap_tion <- "Rolling Portfolio Optimization Strategy for Sub-Portfolio of S&P500 Stocks"
         # Load S&P500 stocks
         # load("C:/Develop/data/returns_100.RData")
         load("C:/Develop/lecture_slides/data/sp500_returns.RData")
         # Select the columns with non-zero returns
         re_turns <- re_turns["1999-01-01/"]
         re_turns <- re_turns[, !(re_turns[NROW(re_turns) %/% 10, ] == 0)]
         # Select 100 columns to reduce computations
         set.seed(1121)  # Reset random number generator
         sam_ple <- sample(1:NCOL(re_turns), 100)
         re_turns <- re_turns[, sam_ple]
         # re_turns <- returns_100
         # re_turns <- cbind(re_turns, rutils::etf_env$re_turns$SVXY, rutils::etf_env$re_turns$VXX)
         model_type <- "rank"
         look_back <- 5
         look_back_max <- 50
         max_eigen <- 35
         al_pha <- 0.01
       }
)  # end switch

# Copy over NA values
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
n_weights <- NCOL(re_turns)

# Random data
# coredata(re_turns) <- matrix(rnorm(prod(dim(re_turns)))/100, nc=n_weights)


risk_free <- 0.03/260

# Calculate returns on equal weight portfolio
# in_dex <- xts(cumprod(1 + rowMeans(re_turns)), index(re_turns))
in_dex <- rowMeans(re_turns)
std_dev <- sd(in_dex)
# sharp_e <- sqrt(252)*mean(in_dex)/std_dev
in_dex <- xts(in_dex, index(re_turns))


## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(cap_tion),
  # titlePanel("Rolling Portfolio Optimization Strategy for ETF Portfolio or for Sub-Portfolio of S&P500 Stocks"),
  
  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the re_calculate variable is updated
    column(width=12, 
           h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
           actionButton("re_calculate", "Recalculate the Model"))
  ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input choice of model
    column(width=3, selectInput("model_type", label="Model type",
                                choices=c("max_sharpe", "max_sharpe_median", "min_var", "min_varpca", "rank", "rankrob", "quan_tile", "skew"), selected=model_type)),
    # Input end points interval
    column(width=3, selectInput("inter_val", label="End points Interval",
                choices=c("days", "weeks", "months", "years"), selected="weeks")),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback interval",
                                min=1, max=look_back_max, value=look_back, step=1)),
    # Input Weight decay for filtering returns
    column(width=3, sliderInput("lamb_da", label="Weight decay:",
                                min=0.01, max=0.99, value=0.99, step=0.05)),
    # Input exponent for variance
    column(width=3, sliderInput("expo_nent", label="Variance exponent:",
                                min=0.25, max=1.5, value=1.0, step=0.05)),
    # Input number of eigenvalues for regularized matrix inverse
    column(width=3, numericInput("max_eigen", "Number of eigenvalues", value=max_eigen)),
    # Input the shrinkage intensity
    column(width=3, sliderInput("al_pha", label="Shrinkage intensity",
                                min=0.01, max=0.99, value=al_pha, step=0.05)),
    # Input the quantile
    column(width=3, sliderInput("pro_b", label="Confidence level",
                                min=0.01, max=0.49, value=0.25, step=0.01)),
    # Input choice of ex_cess returns
    column(width=3, selectInput("returns_scaling", label="Excess returns scaling",
                                choices=c("none", "re_scaled", "volatility", "sharpe"), selected="none")),
    # If fac_tor=1 then trending, If fac_tor=(-1) then contrarian
    # column(width=3, numericInput("fac_tor", "Trend coefficient:", value=1)),
    column(width=3, selectInput("fac_tor", label="Trend coefficient",
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
    inter_val <- isolate(input$inter_val)
    max_eigen <- isolate(input$max_eigen)
    look_back <- isolate(input$look_back)
    lamb_da <- isolate(input$lamb_da)
    expo_nent <- isolate(input$expo_nent)
    model_type <- isolate(input$model_type)
    al_pha <- isolate(input$al_pha)
    pro_b <- isolate(input$pro_b)
    returns_scaling <- isolate(input$returns_scaling)
    fac_tor <- as.numeric(isolate(input$fac_tor))
    # Model is recalculated when the re_calculate variable is updated
    input$re_calculate
    
    switch(returns_scaling,
           "none" = {
             ex_cess <- (re_turns - risk_free)
             # ex_cess <- returns_100
           },
           "re_scaled" = {
             # Scale the re_turns by the trailing volatility
             ex_cess <- re_turns/(HighFreq::roll_var(re_turns, look_back=look_back))^expo_nent
             ex_cess[!is.finite(ex_cess)] <- 0.1
           },
           "volatility" = {
             # Calculate trailing volatilities
             ex_cess <- HighFreq::roll_var(re_turns, look_back=look_back)^expo_nent
             ex_cess[!is.finite(ex_cess)] <- 0.1
           },
           "sharpe" = {
             # Calculate trailing Sharpe ratios
             ex_cess <- HighFreq::roll_sum(re_turns, look_back=look_back)/(HighFreq::roll_var(re_turns, look_back=look_back))^expo_nent
             ex_cess[!is.finite(ex_cess)] <- 0.1
           }
    )  # end switch
    
    # Filter the ex_cess returns using an exponential weighted filter - doesn't provide significant improvement
    # Calculate the weight_s
    weight_s <- exp(-lamb_da*(1:look_back))
    weight_s <- weight_s/sum(weight_s)
    weight_s <- matrix(weight_s, nc=1)
    # Calculate smoothed ex_cess returns
    ex_cess <- HighFreq::roll_conv(ex_cess, weight_s=weight_s)
    ex_cess <- HighFreq::lag_it(ex_cess, lagg=1)
    

    # Define end points
    end_points <- rutils::calc_endpoints(re_turns, inter_val=inter_val)
    # end_points <- ifelse(end_points<(n_weights+1), n_weights+1, end_points)
    end_points <- end_points[end_points > (n_weights+1)]
    len_gth <- NROW(end_points)
    # Define start_points
    start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
    

    if (model_type == "skew") {
      # Run skew model
      n_rows <- NROW(ex_cess)
      n_cols <- NCOL(ex_cess)
      # "days", "weeks", "months", "years"
      # Multiply the look_back by number of days in inter_val
      n_periods <- rutils::diff_it(end_points)
      which_periods <- which.max(table(n_periods))
      n_periods <- n_periods[which_periods]
      look_back <- n_periods*look_back
      # Calculate the skew-like stats
      ma_x <- RcppRoll::roll_max(ex_cess, n=look_back, align="right")
      # mi_n <- -RcppRoll::roll_max(-ex_cess, n=look_back, align="right")
      # me_an <- RcppRoll::roll_mean(ex_cess, n=look_back, align="right")
      medi_an <- RcppRoll::roll_median(ex_cess, n=look_back, align="right")
      ## Calculate the skew-like stats
      # stat_s <- ma_x - mi_n
      # Best so far
      stat_s <- ma_x / medi_an^expo_nent
      # stat_s <- ma_x / me_an
      # stat_s <- (ma_x + mi_n - 2*medi_an) / (ma_x - mi_n)
      # stat_s <- (ma_x - medi_an) / (medi_an - mi_n)
      # stat_s <- (medi_an - me_an) / (medi_an)
      stat_s[is.infinite(stat_s)] <- 0
      stat_s[is.na(stat_s)] <- 0
      stat_s <- rbind(matrix(1:((look_back-1)*n_cols), ncol=n_cols),
                    stat_s)
      stat_s <- matrixStats::rowRanks(stat_s)
      position_s <- matrix(rep(NA_integer_, n_rows*n_cols), ncol=n_cols)
      position_s[1, ] <- 0
      # Reset the positions according to the sort stat_s
      position_s <- stat_s
      # Reset the positions only at the end_points and hold the position between the end_points
      # position_s[end_points, ] <- stat_s[end_points, ]
      position_s <- zoo::na.locf(position_s, na.rm=FALSE)
      position_s <- (position_s - rowMeans(position_s))
      # Average the past position_s to reflect holding the position for some time
      position_s <- HighFreq::roll_sum(position_s, look_back=look_back)
      pnl_s <- fac_tor*position_s*re_turns
      pnl_s <- rowMeans(pnl_s)
    } else {
      # Run HighFreq::back_test()
      if (inter_val == "days") {
        # Rerun the strategy with fixed start date
        pnl_s <- HighFreq::back_test(ex_cess=ex_cess,
                                     re_turns=re_turns,
                                     start_points=start_points-1,
                                     end_points=end_points-1,
                                     pro_b=pro_b,
                                     max_eigen=max_eigen,
                                     al_pha=al_pha,
                                     typ_e=model_type,
                                     co_eff=fac_tor)
        pnl_s[which(is.na(pnl_s)), ] <- 0
      } else {
        # Rerun the strategy with multiple start dates
        end_point <- end_points[len_gth]
        ste_p <- (end_point - end_points[len_gth-1] - 1)
        
        pnl_s <- lapply(1:ste_p, function(shif_t) {
          ep_new <- c(end_points-shif_t, end_point)
          sp_new <- c(rep_len(1, look_back-1), ep_new[1:(len_gth-look_back+2)])
          pnl_s <- HighFreq::back_test(ex_cess=ex_cess, 
                                       re_turns=re_turns,
                                       start_points=sp_new-1,
                                       end_points=ep_new-1,
                                       pro_b=pro_b,
                                       max_eigen=max_eigen, 
                                       al_pha=al_pha, 
                                       typ_e=model_type,
                                       co_eff=fac_tor)
          pnl_s[which(is.na(pnl_s)), ] <- 0
          pnl_s
        })  # end lapply
        
        # Calculate the average of pnls of strategies with different start dates
        pnl_s <- do.call(cbind, pnl_s)
        pnl_s <- rowMeans(pnl_s)
        
      }  # end if
    }  # end if
    
    pnl_s <- std_dev*pnl_s/sd(pnl_s)
    pnl_s <- cbind(pnl_s, in_dex)
    # sharp_e <- sqrt(252)*sapply(pnl_s, function(x) mean(x)/sd(x))
    sharp_e <- sqrt(252)*sapply(pnl_s, function(x) mean(x)/sd(x[x<0]))
    sharp_e <- round(sharp_e, 3)
    pnl_s <- cumsum(pnl_s)
    # pnl_s <- cumprod(1 + pnl_s)
    colnames(pnl_s) <- paste0(c("Strategy SR=", "Index SR="), sharp_e)
    pnl_s[c(1, end_points), ]
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph({
    col_names <- colnames(da_ta())
    dygraphs::dygraph(da_ta(), main="Rolling Portfolio Optimization Strategy") %>%
      dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
      dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
      dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue") %>%
      dyLegend(width=500)
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
