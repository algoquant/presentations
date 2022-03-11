##############################
# This is a shiny app for simulating a contrarian strategy based 
# on the z-scores from regressions of returns, using function 
# HighFreq::run_zscores(). 
# The model flips the position only if the indicator persists over 
# several consecutive periods equal to lagg.
# It uses reactive code to avoid unnecessary calculations.
# This is the best performing univariate strategy.
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
symbolv <- c("SPY", "VXX", "LODE", "GME")
symbol <- "SPY"

cap_tion <- paste("Regression Z-score of SVXY Versus VXX")

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
    # column(width=2, sliderInput("short_back", label="Short lookback", min=3, max=30, value=3, step=1)),
    # Input long look-back interval
    # column(width=2, sliderInput("long_back", label="Long lookback", min=10, max=100, value=100, step=1)),
    column(width=2, sliderInput("lambdav", label="lambdav:", min=0.01, max=0.9, value=0.25, step=0.01)),
    # Input lag trade parameter
    column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=1, step=1)),
    # Input threshold level
    column(width=2, sliderInput("threshold", label="threshold", min=0.01, max=2.0, value=0.1, step=0.05)),
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
  
  # Create an empty list of reactive values.
  value_s <- reactiveValues()
  
  # Load data
  ohlc <- reactive({
    cat("Loading data\n")
    symbol <- input$symbol
    
    # Select the data: "SPY", "VXX", "LODE" or "GME"
    switch(symbol,
           "SPY" = {
             ## SPY ETF 1-minute bars
             # ohlc <- HighFreq::SPY["2012"]["T09:31:00/T15:59:00"]
             load(file="/Volumes/external/Develop/data/polygon/spy_minutes.RData")
             #.n_rows <- NROW(ohlc)
             # log(Cl(ohlc))
             ohlc["T09:00:00/T16:30:00"]
           },
           "VXX" = {
             ## SPY ETF 1-minute bars
             # ohlc <- HighFreq::SPY["2012"]["T09:31:00/T15:59:00"]
             load(file="/Volumes/external/Develop/data/polygon/vxx_minutes.RData")
             #.n_rows <- NROW(ohlc)
             # log(Cl(ohlc))
             ohlc["T09:00:00/T16:30:00"]
           },
           "LODE" = {
             ## LODE 1-minute bars
             ohlc <- data.table::fread(file="/Volumes/external/Develop/predictive/data/lode_oneminutebars.csv", sep=",")
             ohlc <- ohlc[, c(4, 6, 7, 5, 2)]
             colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume")
            .n_rows <- NROW(ohlc)
             dates <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out.n_rows)
             xts::xts(as.matrix(ohlc), dates)
           },
           "GME" = {
             ## GME 1-minute bars
             ohlc <- data.table::fread(file="/Volumes/external/Develop/predictive/data/gme_oneminutebars.csv", sep=",")
             ohlc <- ohlc[, c(4, 6, 7, 5, 2)]
             colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume")
            .n_rows <- NROW(ohlc)
             dates <- seq.POSIXt(from=as.POSIXct("2021-03-10 09:30:00", origin="1970-01-01"), by="min", length.out.n_rows)
             xts::xts(as.matrix(ohlc), dates)
           }
    )  # end switch
    
  })  # end reactive
  
  # Calculate log close prices
  # closep <- reactive({
  #   cat("Calculating log close prices\n")
  #   log(Cl(ohlc()))
  # })  # end reactive
  
  # Calculate log returns
  returns <- reactive({
    cat("Calculating log returns\n")
    rutils::diffit(log(Cl(ohlc())))
  })  # end reactive
  
  # Calculate z_scores if there are new short_back and long_back values
  z_scores <- reactive({
    cat("Calculating z_scores\n")
    # short_back <- input$short_back
    # long_back <- input$long_back
    lambdav <- input$lambdav
    
    # Calculate EWMA weights
    # weights <- exp(-lambdav*1:long_back)
    # weights <- weights/sum(weights)

    predictor <- matrix(rep(1, NROW(returns())))
    # This need update
    z_scores <- HighFreq::run_zscores(returns(), predictor, lambda=lambdav, demean=FALSE)
    z_scores <- z_scores[, 1, drop=FALSE]
    z_scores <- HighFreq::lagit(z_scores, pad_zeros=TRUE)
    
    # highp <- Hi(ohlc())
    # lowp <- Lo(ohlc())
    # closep <- Cl(ohlc())
    # volat <- HighFreq::roll_conv(highp-lowp, matrix(weights))
    # volat <- rutils::lagit(volat, pad_zeros=FALSE)
    # Calculate the rolling prices
    # ohlc <- HighFreq::roll_sum(ohlc(), look_back=short_back)/short_back
    # close_w <- HighFreq::roll_conv(closep, matrix(weights))
    # close_w <- rutils::lagit(close_w, pad_zeros=FALSE)

    # Calculate EWMA prices by filtering with the weights
    # cum_scaled <- cumsum(rets_scaled)

    # Calculate the rolling volume
    # volumes <- Vo(ohlc())
    # Scale the volume by the rolling volume
    # volumes <- short_back*volumes/HighFreq::roll_sum(volumes, look_back=short_back)
    # returns <- rutils::diffit(closep())
    # Calculate the cumulative returns scaled by the rolling volume
    # volumes <- rutils::lagit(volumes, pad_zeros=FALSE)
    # returns <- ifelse(volumes > 0, returns()/volumes, 0)
    # returns[is.na(returns) | is.infinite(returns)] <- 0
    # returns <- rutils::diffit(closep)
    # cum_scaled <- cumsum(returns)

    # Calculate the rolling median of the cumulative returns
    # mi_n <- roll::roll_min(lowp, width=short_back)
    # ma_x <- roll::roll_max(highp, width=short_back)
    # mi_n[1:short_back, ] <- 0
    # ma_x[1:short_back, ] <- 1
    # mi_n <- rutils::lagit(mi_n, pad_zeros=FALSE)
    # ma_x <- rutils::lagit(ma_x, pad_zeros=FALSE)
    # Don't divide z_scores by the ma_d because it's redundant since z_scores is divided by the mad_zscores.
    # Old code:
    # ma_d <- TTR::runMAD(returns, n=short_back)
    # ma_d[1:short_back, ] <- 1
    # z_scores <- ifelse(ma_d != 0, (closep-mi_n)/ma_d, 0)
    # Calculate the z_scores as the rolling cumulative returns
    # z_scores <- ifelse(ma_x > mi_n, (2*closep - mi_n - ma_x)/(ma_x - mi_n), 0)
    
    # z_scores <- ifelse(volat > 0, (closep - close_w)/volat, 0)
    # zscores_w <- HighFreq::roll_conv(matrix(z_scores), matrix(weights))
    # zscores_w <- rutils::lagit(zscores_w, pad_zeros=FALSE)
    # z_scores <- ifelse(zscores_w > 0, z_scores/zscores_w, z_scores)
    
    # z_scores[is.na(z_scores) | is.infinite(z_scores)] <- 0
    # Standardize the z_scores
    # Old code:
    # z_scores[1:short_back, ] <- 0
    # med_zscores <- TTR::runMedian(z_scores, n=long_back)
    # med_zscores[1:(long_back), ] <- 0
    # mad_zscores <- TTR::runMAD(z_scores, n=long_back)
    # mad_zscores[1:(long_back), ] <- 0
    # ifelse(mad_zscores != 0, (z_scores - med_zscores)/mad_zscores, 0)
    # Standardize the z_scores - HighFreq::roll_scale() is fastest
    # z_scores <- HighFreq::roll_scale(z_scores, look_back=long_back, use_median=TRUE)
    z_scores[is.na(z_scores) | is.infinite(z_scores)] <- 0
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
    # returns <- rutils::diffit(closep())
   .n_rows <- NROW(returns())
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
    position_s <- rutils::lagit(position_s, lagg=1)
    
    # Number of trades
    value_s$n_trades <- sum(abs(rutils::diffit(position_s)))# /.n_rows
    
    pnls <- cbind(position_s*returns(), returns())
    
    # Sharpe
    sharp_e <- sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    value_s$sharp_e <- round(sqrt(252)*sharp_e, 3)

    # pnls <- cumsum(position_s*returns())
    # cum_scaled <- cumsum(returns())
    pnls <- cumsum(pnls)
    cum_scaled <- pnls[, 2]
    colnames(pnls) <- c("Strategy", "Index")
    
    # Add buy/sell indicators
    indic <- rutils::diffit(position_s)
    indic_buy <- (indic > 0)
    indic_sell <- (indic < 0)
    
    pnls <- cbind(pnls, cum_scaled[indic_buy], cum_scaled[indic_sell])
    colnames(pnls)[3:4] <- c("Buy", "Sell")
    pnls
    # list(caption=cap_tion, pnls=pnls)
  })  # end reactive
  
  
  # Plot dygraph of pnls if the add_annotations variable is updated
  dyplot <- reactive({
    cat("Plotting pnls\n")
    add_annotations <- input$add_annotations
    # cap_tion <- pnls()$caption
    pnls <- pnls()
    colnamev <- colnames(pnls)
    # cat(paste("colnamev\n", colnamev, "\n"))
    # cat(paste("pnls\n", tail(pnls), "\n"))
    
    cap_tion <- paste(paste("Number of trades =", value_s$n_trades), ",",
                      paste(paste(colnamev[1:2], "Sharpe =", value_s$sharp_e), collapse=", "))

    if (add_annotations == "True") {
      # Create a dygraph object with annotations (no plot is created)
      dygraphs::dygraph(pnls, main=cap_tion) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
        # Add second y-axis
        dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue") %>%
        dySeries(name=colnamev[3], axis="y2", label=colnamev[3], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
        dySeries(name=colnamev[4], axis="y2", label=colnamev[4], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")
    } else if (add_annotations == "False") {
      # Create a dygraph object without annotations (no plot is created)
      dygraphs::dygraph(pnls[, 1:2], main=cap_tion) %>%
        dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
        # Add second y-axis
        dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
        dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
        dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue")
    }  # end if
    
  })  # end reactive
  
  # Render (plot) the dygraph object and return it to the output argument
  output$dyplot <- dygraphs::renderDygraph(dyplot())
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfunc)
