##############################
# This is a shiny app for simulating a VWAP moving 
# average crossover strategy, with dygraphs plot.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

## Load packages
library(HighFreq)
library(shiny)
library(dygraphs)


## Set up ETF data
# if (!("etf_env" %in% search()))
#   attach(etf_env)
# if (!("etf_env" %in% ls()))
#   load(file="C:/Develop/lecture_slides/data/etf_data.RData")
# data_env <- "etf_env"
# sym_bols <- etf_env$sym_bols
# sym_bol <- "SVXY"
# re_turns <- etf_env$re_turns

data_env <- rutils::etf_env
sym_bols <- sort(get("sym_bols", data_env))
sym_bol <- "SVXY"
# sym_bols <- rutils::etf_env$sym_bols


## Set up S&P500 data
# if (!("sp500_env" %in% search()))
#   attach(sp500_env)
# if (!("sp500_env" %in% ls())) {
#   load(file="C:/Develop/lecture_slides/data/sp500.RData")
# }  # end if
# data_env <- sp500_env
# sym_bols <- names(data_env)
# sym_bols <- c("PG", "CDNS", "YUM", "YUMC", "KHC", "SNPS", "ODFL", "CHRW", "AWK", "SO", "EA", "FIS", "DG", "BAX", "HRL", "MSFT", "XOM", "BSX", "JNJ", "CLX", "CL", "MCD", "WMT", "SBUX", "LLY", "ADM", "BIO", "XLNX", "ATVI", "DISH", "K", "SHW", "SIG", "CSCO", "INTU", "VRTX", "FB", "ORCL", "DUK", "KSS", "ROP", "AKAM", "MXIM", "TXN", "NEM", "COST", "EL", "JWN", "ACN", "FISV", "KLAC", "PFE", "TYL", "BIIB", "MCHP", "BBBY", "DRE", "PEP", "LIN", "NKE", "TROW", "LEN", "HOLX", "NVR", "UDR", "WEC", "DHI", "NI")
# sym_bol <- "YUM"


## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("VWAP Crossover Strategy app_vwap_strat.R"),
  
  # create single row with four slider inputs
  fluidRow(
    # Input stock symbol
    column(width=2, selectInput("sym_bol", label="Symbol",
                                choices=sym_bols, selected=sym_bol)),
    # Input look-back interval
    # column(width=2, sliderInput("look_back", label="Lookback interval", min=1, max=150, value=4, step=1)),
    # Input lambda parameter
    column(width=3, sliderInput("lamb_da", label="lamb_da:", min=0.01, max=0.99, value=0.37, step=0.01)),
    # Input lag trade parameter
    column(width=2, sliderInput("lagg", label="Confirmation signals", min=1, max=5, value=2, step=1)),
    # Input trend or revert
    column(width=2, selectInput("co_eff", label="Trend (1) Revert (-1)",
                                choices=c(1, -1), selected=(1)))
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph", width="100%", height="700px"), height=10, width=12)
)  # end fluidPage interface


## Define the server code
ser_ver <- shiny::shinyServer(function(input, output) {

  ## Create an empty list of reactive values.
  globals <- reactiveValues()
  
  # Recalculate the data and rerun the model
  da_ta <- reactive({
    # Get model parameters from input argument
    sym_bol <- input$sym_bol
    # look_back <- input$look_back
    lamb_da <- input$lamb_da
    lagg <- input$lagg
    co_eff <- as.numeric(input$co_eff)

    # Prepare data
    # sym_bol <- "SVXY"
    # sym_bol2 <- "VXX"
    oh_lc <- get(sym_bol, data_env)
    # oh_lc2 <- get(sym_bol2, data_env)
    price_s <- log(quantmod::Cl(oh_lc))
    star_t <- as.numeric(price_s[1])
    # re_turns <- na.omit(get(sym_bol, re_turns))
    re_turns <- rutils::diff_it(price_s)
    # re_turns2 <- na.omit(get(sym_bol2, re_turns))
    # com_bined <- cbind(re_turns, -na.omit(re_turns2))
    # which_na <- which(is.na(com_bined$VXX))
    # com_bined$VXX[which_na] <- com_bined$SVXY[which_na]
    # clos_e <- cumprod(1+rowMeans(com_bined))
    # clos_e <- quantmod::Cl(oh_lc)
    
    # sum(is.na(com_bined))
    # head(com_bined)
    # vol_ume <- cbind(quantmod::Vo(oh_lc), quantmod::Vo(etf_env$VXX))
    # vol_ume$VXX.Volume[which_na] <- vol_ume$SVXY.Volume[which_na]
    # vol_ume <- rowMeans(vol_ume)
    vol_ume <- quantmod::Vo(oh_lc)
    
    # Simulate strategy
    v_wap <- HighFreq::run_mean(price_s, lambda=lamb_da, weights=vol_ume)

    # Calculate VWAP indicator
    in_dic <- sign(price_s - v_wap)
    # indic_lag <- rutils::lag_it(in_dic, lagg=1)
    # Flip position only if the in_dic and its recent past values are the same.
    # Otherwise keep previous position.
    # This is designed to prevent whipsaws and over-trading.
    # position_s <- ifelse(in_dic == indic_lag, in_dic, position_s)
    indic_sum <- HighFreq::roll_sum(tseries=matrix(in_dic), look_back=lagg)
    indic_sum[1:lagg] <- 0
    position_s <- rep(NA_integer_, NROW(price_s))
    position_s[1] <- 0
    position_s <- ifelse(indic_sum == lagg, 1, position_s)
    position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
    position_s <- zoo::na.locf(position_s, na.rm=FALSE)
    # position_s[1:lagg] <- 0
    # Calculate indicator of flipping the positions
    in_dic <- rutils::diff_it(position_s)
    # Calculate number of trades
    globals$n_trades <- sum(abs(in_dic)>0)
    
    # Add buy/sell indicators for annotations
    indic_buy <- (in_dic > 0)
    indic_sell <- (in_dic < 0)
    # Lag the positions to trade in next period
    position_s <- rutils::lag_it(position_s, lagg=1)
    # Calculate log strategy returns
    # re_turns <- rutils::diff_it(price_s)
    # Calculate strategy profits and losses
    pnl_s <- co_eff*re_turns*position_s
    # Scale the pnl_s so they have same SD as re_turns
    pnl_s <- pnl_s*sd(re_turns[re_turns<0])/sd(pnl_s[pnl_s<0])
    
    pnl_s <- cbind(re_turns, pnl_s)
    colnames(pnl_s) <- c(sym_bol, "Strategy")
    
    # Calculate Sharpe ratios
    sharp_e <- sqrt(252)*sapply(pnl_s, function(x) mean(x)/sd(x[x<0]))
    globals$sharp_e <- round(sharp_e, 3)

    pnl_s <- cumsum(pnl_s)
    # pnl_s <- cbind(pnl_s, v_wap)
    # colnames(pnl_s)[3] <- "VWAP"
    
    # pnl_s <- cumprod(1+pnl_s)
    pnl_s
  })  # end reactive code
  
  # return the dygraph plot to output argument
  output$dy_graph <- dygraphs::renderDygraph({
    
    col_names <- colnames(da_ta())
    
    # Get Sharpe ratios
    sharp_e <- globals$sharp_e
    # Get number of trades
    n_trades <- globals$n_trades
    
    cap_tion <- paste(input$sym_bol, "VWAP strategy / \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharp_e, collapse=" / "), "/ \n",
                      "Number of trades=", n_trades)
    
    dygraphs::dygraph(da_ta(), main=cap_tion) %>%
      dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
      dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
      # dySeries(name=col_names[3], axis="y", label=col_names[3], strokeWidth=2, col="green") %>%
      dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
  })  # end output plot

})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
