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
prices <- rutils::etfenv$prices[, c("VTI", "IEF")]
prices <- na.omit(prices)
rets_dollar <- rutils::diffit(prices)
rets_percent <- rets_dollar/rutils::lagit(prices, lagg=1, pad_zeros=FALSE)

cap_tion <- "Risk Parity Strategy"

## End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
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
    column(width=2, sliderInput("look_back", label="Lookback", min=11, max=130, value=21, step=1)),
    # Input exponent for variance
    column(width=2, sliderInput("expo_nent", label="Std Dev exponent:",
                                min=0.25, max=2.5, value=1.0, step=0.05)),
    # Input weights
    column(width=2, sliderInput("weights", label="VTI weight:",
                                min=0.01, max=0.99, value=0.5, step=0.05))
    # Input lag trade parameter
    # column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1)),
    # Input threshold interval
    # column(width=2, sliderInput("threshold", label="threshold", min=0.5, max=3.0, value=1.0, step=0.1))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
  
)  # end fluidPage interface


## Define the server code
servfunc <- function(input, output) {
  
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
  wealth <- reactive({
    cat("Calculating the wealths\n")
    
    # Get model parameters from input argument
    weights <- input$weights

    # Calculate wealth of fixed ratio of dollar amounts
    weights <- c(weights, 1-weights)
    rets_weighted <- rets_percent %*% weights
    wealth_fixed_ratio <- cumprod(1 + rets_weighted)
    
    # Calculate standardized prices and portfolio allocations
    vo_l <- vo_l()
    allocation_s <- lapply(1:NCOL(prices), 
                           function(x) weights[x]/vo_l[, x])
    allocation_s <- do.call(cbind, allocation_s)
    # Scale allocations to 1 dollar total
    allocation_s <- allocation_s/rowSums(allocation_s)
    # Lag the allocations
    allocation_s <- rutils::lagit(allocation_s)
    # Calculate wealth of risk parity
    rets_weighted <- rowSums(rets_percent*allocation_s)
    wealth_risk_parity <- cumprod(1 + rets_weighted)
    
    # Calculate log wealths
    wealth <- log(cbind(wealth_fixed_ratio, wealth_risk_parity))
    wealth <- xts(wealth, index(prices))
    # Calculate Sharpe ratios
    sharp_e <- sqrt(252)*sapply(rutils::diffit(wealth), function (x) mean(x)/sd(x))
    colnames(wealth) <- paste(c("Fixed Ratio", "Risk Parity"), 
                               round(sharp_e, 3), sep="=")
    wealth
    
  })  # end reactive code
  
  
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    wealth <- wealth()
    # Plot log wealths
    dygraphs::dygraph(wealth, main="Log Wealth of Risk Parity vs Fixed Dollar Ratios") %>%
      dyOptions(colors=c("blue","red"), strokeWidth=2) %>%
      dyLegend(show="always", width=500)
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfunc)
