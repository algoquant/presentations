##############################
# This is a shiny app for plotting returns in trading time 
# scaled by the trading volumes.
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

# sym_bols <- names(data_env)
# sym_bols <- c("SPY", "LODE", "GME")
sym_bols <- rutils::etf_env$sym_bols
sym_bol <- "VTI"

cap_tion <- paste("Histogram of Returns Scaled by the Trading Volumes")

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(cap_tion),
  
  # fluidRow(
  # The Shiny App is re-calculated when the actionButton is clicked and the add_annotations variable is updated
  #   column(width=12,
  #          h4("Click the button 'Recalculate the Model' to re-calculate the Shiny App."),
  #          actionButton("add_annotations", "Recalculate the Model"))
  # ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input end points interval
    # column(width=3, selectInput("inter_val", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input stock symbol
    column(width=3, selectInput("sym_bol", label="Symbol", choices=sym_bols, selected=sym_bol)),
    # Input umber of histogram bins
    column(width=3, sliderInput("n_bins", "Number of bins:", min=1, max=50, value=100)),
    # Input short look-back interval
    column(width=3, sliderInput("short_back", label="Short lookback", min=3, max=150, value=50, step=1)),
    # Input long look-back interval
    # column(width=3, sliderInput("long_back", label="Long lookback", min=10, max=200, value=100, step=2)),
    # Input lag trade parameter
    # column(width=3, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1)),
    # Input threshold level
    column(width=3, sliderInput("expo_nent", label="Exponent of Volume", min=0.25, max=2.0, value=1.0, step=0.1)),
    # column(width=3, sliderInput("thresh_old", label="threshold", min=0.5, max=3.0, value=0.9, step=0.1)),
    # Input add annotations Boolean
    # column(width=3, selectInput("add_annotations", label="Add buy/sell annotations?", choices=c("True", "False"), selected="False"))
    # Input the weight decay parameter
    # column(width=3, sliderInput("lamb_da", label="Weight decay:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input model weights type
    # column(width=3, selectInput("typ_e", label="Portfolio weights type",
    #                             choices=c("max_sharpe", "min_var", "min_varpca", "rank"), selected="rank")),
    # Input number of eigenvalues for regularized matrix inverse
    # column(width=3, sliderInput("max_eigen", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
    # Input the shrinkage intensity
    # column(width=3, sliderInput("al_pha", label="Shrinkage intensity",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input the percentile
    # column(width=3, sliderInput("percen_tile", label="percentile:", min=0.01, max=0.45, value=0.1, step=0.01)),
    # Input the strategy coefficient: co_eff=1 for momentum, and co_eff=-1 for contrarian
    # column(width=3, selectInput("co_eff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    # column(width=3, numericInput("bid_offer", label="bid-offer:", value=0.001, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(plotOutput("histo_gram"))

)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {
  
  # Re-calculate the data and rerun the model
  # da_ta <- reactive({
  # Get model parameters from input argument
  # max_eigen <- isolate(input$max_eigen)
  # look_lag <- isolate(input$look_lag
  # lamb_da <- isolate(input$lamb_da)
  # typ_e <- isolate(input$typ_e)
  # al_pha <- isolate(input$al_pha)
  # percen_tile <- isolate(input$percen_tile)
  # co_eff <- as.numeric(isolate(input$co_eff))
  # bid_offer <- isolate(input$bid_offer)
  # Model is re-calculated when the add_annotations variable is updated
  # input$add_annotations
  
  # Create an empty list of reactive values.
  # value_s <- reactiveValues()
  
  ## Calculate log returns
  re_turns <- reactive({
    sym_bol <- input$sym_bol
    cat("Loading data for ", sym_bol, "\n")
    oh_lc <- rutils::etf_env[[sym_bol]]
    re_turns <- cbind(rutils::diff_it(log(Cl(oh_lc))),
                      Vo(oh_lc))
    colnames(re_turns) <- c("returns", "volume")
    re_turns
  })  # end reactive
  
  ## Calculate scale_d if there are new short_back and long_back values
  scale_d <- reactive({
    cat("Calculating scaled returns in trading time \n")
    short_back <- input$short_back
    # long_back <- input$long_back
    
    # Calculate the rolling volume
    re_turns <- re_turns()$returns
    vol_ume <- (re_turns()$volume)^input$expo_nent
    # Scale the volume by the rolling volume
    vol_ume <- short_back*vol_ume/HighFreq::roll_sum(vol_ume, look_back=short_back)
    # re_turns <- rutils::diff_it(clos_e())
    # Calculate the cumulative returns scaled by the rolling volume
    scale_d <- re_turns/vol_ume
    scale_d[is.na(scale_d) | is.infinite(scale_d)] <- 0

    scale_d <- cbind(re_turns, scale_d)
    colnames(scale_d) <- c("returns", "scaled")
    scale_d
    
  })  # end reactive
  
  # Plot the histogram of the simulated data
  output$histo_gram <- shiny::renderPlot({
    # isolate() prevents automatic re-calculation when n_bins is updated
    sym_bol <- input$sym_bol
    cat("Plotting data for ", sym_bol, "\n")
    
    n_bins <- input$n_bins
    re_turns <- scale_d()$returns
    scale_d <- scale_d()$scaled
    
    # Calculate kurtosis of the returns
    n_rows <- NROW(re_turns)
    kurto_sis <- sum((re_turns/sd(re_turns))^4)/n_rows
    kurtosis_scaled <- sum((scale_d/sd(scale_d))^4)/n_rows
    pac_f <- pacf(re_turns, lag=10, plot=FALSE)
    pac_f <- sum(drop(pac_f$acf))
    pacf_scaled <- pacf(scale_d, lag=10, plot=FALSE)
    pacf_scaled <- sum(drop(pacf_scaled$acf))
    
    # Calculate breaks based on input$bins from ui.R
    ma_d <- mad(re_turns)
    break_s <- seq(min(re_turns), max(re_turns), length.out=n_bins+1)
    # Calculate the kernel density using density()
    # b_w <- mad(rutils::diff_it(re_turns, lagg=10))/10
    den_sity <- density(re_turns, bw=ma_d/10)
    
    # Plot the histogram with the specified number of breaks
    cap_tion <- paste("Histogram of", sym_bol, "Returns Scaled by the Trading Volumes \n", 
                      "kurtosis=", round(kurto_sis, 2), "kurtosis scaled=", round(kurtosis_scaled, 2), "\n", 
                      "pacf=", round(pac_f, 2), "pacf scaled=", round(pacf_scaled, 2))
    hist(re_turns, breaks=break_s, xlim=c(-5*ma_d, 5*ma_d), xlab="returns", ylab="", 
         freq=FALSE, col="darkgray", border="white", main=cap_tion)
    # Draw kernel density of re_turns
    lines(den_sity, col="blue", lwd=3)
    # Draw kernel density of scale_d
    lines(density(scale_d, bw=ma_d/10), col="red", lwd=3)
    # Add density of normal distribution
    curve(expr=dnorm(x, mean=mean(re_turns), sd=sd(re_turns)),
          add=TRUE, lwd=2, col="green")
    
  })  # end renderPlot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
