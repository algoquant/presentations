##############################
# This is a shiny app for simulating a risk parity strategy.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Model and data setup

# Calculate dollar and percentage returns for VTI and IEF
price_s <- rutils::etf_env$price_s[, c("VTI", "IEF")]
price_s <- na.omit(price_s)
rets_dollar <- rutils::diff_it(price_s)
rets_percent <- rets_dollar/rutils::lag_it(price_s, lagg=1, pad_zeros=FALSE)

cap_tion <- "Risk Parity Strategy"

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(cap_tion),
  
  # fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the re_calculate variable is updated
    # column(width=12,
    #        h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
    #        actionButton("re_calculate", "Recalculate the Model"))
  # ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback", min=11, max=130, value=21, step=1)),
    # Input exponent for variance
    column(width=3, sliderInput("expo_nent", label="Std Dev exponent:",
                                min=0.25, max=2.5, value=1.0, step=0.05)),
    # Input weight_s
    column(width=3, sliderInput("weight_s", label="VTI weight:",
                                min=0.01, max=0.99, value=0.5, step=0.05))
    # Input lag trade parameter
    # column(width=3, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1)),
    # Input threshold interval
    # column(width=3, sliderInput("thresh_old", label="threshold", min=0.5, max=3.0, value=1.0, step=0.1))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph"), width=12)
  
)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {
  
  # Calculate rolling percentage volatility
  vo_l <- reactive({
    cat("Calculating the volatilities\n")
    
    # Get model parameters from input argument
    look_back <- input$look_back
    expo_nent <- input$expo_nent

    # Calculate rolling percentage volatility
    vo_l <- roll::roll_sd(rets_percent, width=look_back)
    vo_l <- zoo::na.locf(vo_l, na.rm=FALSE)
    vo_l <- zoo::na.locf(vo_l, fromLast=TRUE)
    vo_l^expo_nent

  })  # end reactive code
  
  
  # Calculate wealths
  weal_th <- reactive({
    cat("Calculating the wealths\n")
    
    # Get model parameters from input argument
    weight_s <- input$weight_s

    # Calculate wealth of fixed ratio of dollar amounts
    weight_s <- c(weight_s, 1-weight_s)
    rets_weighted <- rets_percent %*% weight_s
    wealth_fixed_ratio <- cumprod(1 + rets_weighted)
    
    # Calculate standardized prices and portfolio allocations
    vo_l <- vo_l()
    allocation_s <- lapply(1:NCOL(price_s), 
                           function(x) weight_s[x]/vo_l[, x])
    allocation_s <- do.call(cbind, allocation_s)
    # Scale allocations to 1 dollar total
    allocation_s <- allocation_s/rowSums(allocation_s)
    # Lag the allocations
    allocation_s <- rutils::lag_it(allocation_s)
    # Calculate wealth of risk parity
    rets_weighted <- rowSums(rets_percent*allocation_s)
    wealth_risk_parity <- cumprod(1 + rets_weighted)
    
    # Calculate log wealths
    weal_th <- log(cbind(wealth_fixed_ratio, wealth_risk_parity))
    weal_th <- xts(weal_th, index(price_s))
    # Calculate Sharpe ratios
    sharp_e <- sqrt(252)*sapply(rutils::diff_it(weal_th), function (x) mean(x)/sd(x))
    colnames(weal_th) <- paste(c("Fixed Ratio", "Risk Parity"), 
                               round(sharp_e, 3), sep="=")
    weal_th
    
  })  # end reactive code
  
  
  # Return to the output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph({
    weal_th <- weal_th()
    # Plot log wealths
    dygraphs::dygraph(weal_th, main="Log Wealth of Risk Parity vs Fixed Dollar Ratios") %>%
      dyOptions(colors=c("blue","red"), strokeWidth=2) %>%
      dyLegend(show="always", width=500)
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
