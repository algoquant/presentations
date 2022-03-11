##############################
# This is a shiny app for simulating a contrarian strategy 
# using the Hampel filter over prices.
# It flips the position only if the indicator persists over several 
# consecutive periods equal to lagg.
# It uses reactive code to avoid unnecessary calculations.
# This is the best performing strategy.
# 
# Just press the "Run App" button on upper right of this panel.
##############################


## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Model and data setup

# symbolv <- names(data_env)
symbolv <- c("SPY", "LODE", "GME")
symbol <- "LODE"

cap_tion <- paste("Contrarian Strategy for", symbol, "Using the Hampel Filter Over Prices")

## End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel(cap_tion),
  
  # fluidRow(
  # The Shiny App is recalculated when the actionButton is clicked and the add_annotations variable is updated
  #   column(width=12,
  #          h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
  #          actionButton("add_annotations", "Recalculate the Model"))
  # ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input end points interval
    # column(width=2, selectInput("interval", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input stock symbol
    column(width=2, selectInput("symbol", label="Symbol",
                                choices=symbolv, selected=symbol)),
    # Input short look-back interval
    column(width=2, sliderInput("short_back", label="Short lookback", min=3, max=30, value=8, step=1)),
    # Input long look-back interval
    column(width=2, sliderInput("long_back", label="Long lookback", min=10, max=100, value=20, step=1)),
    # Input lag trade parameter
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1)),
    # Input threshold interval
    column(width=2, sliderInput("threshold", label="threshold", min=0.5, max=3.0, value=2.0, step=0.1)),
    # Input add annotations Boolean
    column(width=2, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False"))
    # Input the weight decay parameter
    # column(width=2, sliderInput("lambdav", label="Weight decay:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input model weights type
    # column(width=2, selectInput("typev", label="Portfolio weights type",
    #                             choices=c("max_sharpe", "min_var", "min_varpca", "rank"), selected="rank")),
    # Input number of eigenvalues for regularized matrix inverse
    # column(width=2, sliderInput("max_eigen", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
    # Input the shrinkage intensity
    # column(width=2, sliderInput("alpha", label="Shrinkage intensity",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input the percentile
    # column(width=2, sliderInput("percen_tile", label="percentile:", min=0.01, max=0.45, value=0.1, step=0.01)),
    # Input the strategy coefficient: coeff=1 for momentum, and coeff=-1 for contrarian
    # column(width=2, selectInput("coeff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    # column(width=2, numericInput("bid_offer", label="bid-offer:", value=0.001, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
  
)  # end fluidPage interface


## Define the server code
servfunc <- function(input, output) {
  
  # Recalculate the data and rerun the model
  # datav <- reactive({
  # Get model parameters from input argument
  # max_eigen <- isolate(input$max_eigen)
  # look_lag <- isolate(input$look_lag
  # lambdav <- isolate(input$lambdav)
  # typev <- isolate(input$typev)
  # alpha <- isolate(input$alpha)
  # percen_tile <- isolate(input$percen_tile)
  # coeff <- as.numeric(isolate(input$coeff))
  # bid_offer <- isolate(input$bid_offer)
  # Model is recalculated when the add_annotations variable is updated
  # input$add_annotations
  
  
  # short_back <- 11
  # half_window <- short_back %/% 2

  # Create an empty list of reactive values.
  value_s <- reactiveValues()
  
  # Load data
  closep <- reactive({
    cat("Loading data\n")
    symbol <- input$symbol
    
    # Select the data: "SPY", "LODE" or "GME"
    switch(symbol,
           "SPY" = {
             ## SPY ETF 1-minute bars
             ohlc <- HighFreq::SPY["2011"]["T09:31:00/T15:59:00"]
             #.n_rows <- NROW(ohlc)
             log(Cl(ohlc))
           },
           "LODE" = {
             ## LODE 1-minute bars
             ohlc <- data.table::fread(file="C:/Develop/predictive/data/lode_oneminutebars.csv", sep=",")
            .n_rows <- NROW(ohlc)
             dates <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out.n_rows)
             closep <- log(ohlc$close)
             xts::xts(closep, dates)
           },
           "GME" = {
             ## GME 1-minute bars
             ohlc <- data.table::fread(file="C:/Develop/predictive/data/gme_oneminutebars.csv", sep=",")
            .n_rows <- NROW(ohlc)
             dates <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out.n_rows)
             closep <- log(ohlc$close)
             xts::xts(closep, dates)
           }
    )  # end switch

  })  # end reactive
  
  # Calculate z_scores if new there's short_back value
  z_scores <- reactive({
    cat("Calculating z_scores\n")
    short_back <- input$short_back
    long_back <- input$long_back
    closep <- closep()
    # Calculate the z_scores
    medi_an <- roll::roll_median(closep, width=short_back)
    medi_an[1:short_back, ] <- 1
    # Don't divide z_scores by the ma_d because it's redundant since z_scores is divided by the mad_zscores.
    # Old code:
    # ma_d <- TTR::runMAD(returns, n=short_back)
    # ma_d[1:short_back, ] <- 1
    # z_scores <- ifelse(ma_d != 0, (closep-medi_an)/ma_d, 0)
    # Calculate cumulative return
    z_scores <- (closep - medi_an)
    # Standardize the z_scores
    # Old code:
    # z_scores[1:short_back, ] <- 0
    # med_zscores <- TTR::runMedian(z_scores, n=long_back)
    # med_zscores[1:(long_back), ] <- 0
    # mad_zscores <- TTR::runMAD(z_scores, n=long_back)
    # mad_zscores[1:(long_back), ] <- 0
    # ifelse(mad_zscores != 0, (z_scores - med_zscores)/mad_zscores, 0)
    # Standardize the z_scores - HighFreq::roll_scale() is fastest
    z_scores <- HighFreq::roll_scale(z_scores, look_back=long_back, use_median=TRUE)
    z_scores[is.na(z_scores)] <- 0
    z_scores[is.infinite(z_scores)] <- 0
    z_scores
  })  # end reactive
  
  # Plot histogram of z_scores
  # range(z_scores)
  # z_scores <- z_scores[z_scores > quantile(z_scores, 0.05)]
  # z_scores <- z_scores[z_scores < quantile(z_scores, 0.95)]
  # x11(width=6, height=5)
  # hist(z_scores, xlim=c(quantile(z_scores, 0.05), quantile(z_scores, 0.95)), breaks=50, main=paste("Z-scores for", "short_back =", short_back))
  
  # Calculate position_s and pnls if there's new threshold value
  pnls <- reactive({
    cat("Calculating position_s and pnls\n")
    threshold <- input$threshold
    lagg <- input$lagg
    returns <- rutils::diffit(closep())
   .n_rows <- NROW(closep())
    # Determine if the z_scores have exceeded the threshold
    indic <- rep(0,.n_rows)
    # indic[1] <- 0
    indic <- ifelse(z_scores() > threshold, -1, indic)
    indic <- ifelse(z_scores() < (-threshold), 1, indic)
    # Calculate number of consecutive indicators in same direction.
    # This is designed to avoid trading on microstructure noise.
    # indic <- ifelse(indic == indic_lag, indic, indic)
    indic_sum <- HighFreq::roll_vec(tseries=matrix(indic), look_back=lagg)
    indic_sum[1:lagg] <- 0
    
    # Calculate position_s and pnls from indic_sum.
    # position_s <- rep(NA_integer_,.n_rows)
    # position_s[1] <- 0
    # threshold <- 3*mad(z_scores)
    # Flip position only if the indic_sum is at least equal to lagg.
    # Otherwise keep previous position.
    position_s <- rep(NA_integer_,.n_rows)
    position_s[1] <- 0
    position_s <- ifelse(indic_sum >= lagg, 1, position_s)
    position_s <- ifelse(indic_sum <= (-lagg), -1, position_s)
    # position_s <- ifelse(z_scores > threshold, -1, position_s)
    # position_s <- ifelse(z_scores < (-threshold), 1, position_s)
    position_s <- zoo::na.locf(position_s, na.rm=FALSE)
    # Calculate indicator of flipping the positions
    indic <- rutils::diffit(position_s)
    # Lag the positions to reflect trade on next day
    position_s <- rutils::lagit(position_s, lagg=1)

    # Calculate pnls
    pnls <- cumsum(position_s*returns)
    cumsumv <- cumsum(returns)
    pnls <- cbind(cumsumv, pnls)
    colnames(pnls) <- c("Index", "Strategy")
    
    # Calculate number of trades
    value_s$n_trades <- sum(abs(indic)>0)
    
    # Add buy/sell indicators
    indic_buy <- (indic > 0)
    indic_sell <- (indic < 0)

    pnls <- cbind(pnls, cumsumv[indic_buy], cumsumv[indic_sell])
    colnames(pnls)[3:4] <- c("Buy", "Sell")
    pnls
    # list(caption=cap_tion, pnls=pnls)
  })  # end reactive
  

  # Calculate position_s if there's new threshold value
  dyplot <- reactive({
    cat("Plotting pnls\n")
    # Model is recalculated when the add_annotations variable is updated
    add_annotations <- input$add_annotations
    # cap_tion <- pnls()$caption
    pnls <- pnls()
    colnamev <- colnames(pnls)
    # cat(paste("colnamev\n", colnamev, "\n"))
    # cat(paste("pnls\n", tail(pnls), "\n"))
    
    # Number of trades
    # n_trades <- sum(abs(rutils::diffit(position_s)))# /.n_rows
    # cap_tion <- paste("Number of trades =", n_trades)
    # Calculate Sharpe ratios
    sharp_e <- sqrt(252)*sapply(rutils::diffit(pnls[, 1:2]), function(x) mean(x)/sd(x[x<0]))
    sharp_e <- round(sharp_e, 3)
    
    # cap_tion <- paste("Contrarian Strategy for", input$symbol, "Using the Hampel Filter Over Prices")
    cap_tion <- paste("Strategy for", input$symbol, "Trading Volumes, \n", 
                      paste0(c("Index SR=", "Strategy SR="), sharp_e, collapse=", "), ", \n",
                      "Number of trades=", value_s$n_trades)

    if (add_annotations == "True") {
      dygraphs::dygraph(pnls, main=cap_tion) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red") %>%
        dySeries(name=colnamev[3], axis="y", label=colnamev[3], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=colnamev[4], axis="y", label=colnamev[4], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == "False") {
        dygraphs::dygraph(pnls[, 1:2], main=cap_tion) %>%
          dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
          dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
          dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
          dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red")
    }  # end if
        
  })  # end reactive

  # Render the dyplot object
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph(dyplot())
    
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfunc)
