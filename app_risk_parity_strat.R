##############################
# This is a shiny app for simulating a risk parity strategy.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(rutils)
library(shiny)
library(dygraphs)

# Model and data setup

# Calculate dollar and percentage returns for VTI and IEF
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
retd <- rutils::diffit(pricev)
retp <- retd/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)

captiont <- "Log Wealth of Risk Parity vs Fixed Dollar Allocations for VTI and IEF"

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  # fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
    # column(width=12,
    #        h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
    #        actionButton("recalcb", "Recalculate the Model"))
  # ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input look-back interval
    column(width=2, sliderInput("look_back", label="Lookback", min=11, max=130, value=21, step=1)),
    # Input exponent for variance
    # column(width=2, sliderInput("exponent", label="Std Dev exponent:",
    #                             min=0.25, max=2.5, value=1.0, step=0.05)),
    # Input weights
    column(width=2, sliderInput("weights", label="VTI weight", min=0.4, max=0.6, value=0.5, step=0.01))
    # Input lag trade parameter
    # column(width=2, sliderInput("lagg", label="lagg", min=1, max=5, value=2, step=1)),
    # Input threshold interval
    # column(width=2, sliderInput("threshold", label="threshold", min=0.5, max=3.0, value=1.0, step=0.1))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="80%", height="600px")
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # Create an empty list of reactive values.
  values <- reactiveValues()
  
  # Calculate rolling percentage volatility
  volat <- reactive({
    cat("Calculating the volatilities\n")
    
    # Get model parameters from input argument
    look_back <- input$look_back
    # exponent <- input$exponent

    # Calculate rolling percentage volatility
    volat <- HighFreq::roll_var(retp, look_back=look_back)
    iszero <- (rowSums(volat) == 0)
    volat[iszero, ] <- 1
    # volat^exponent
    volat
    
  })  # end reactive code
  
  
  # Calculate wealths
  wealth <- reactive({
    cat("Calculating the wealths\n")
    
    # Get model parameters from input argument
    weights <- input$weights

    # Calculate wealth of proportional dollar allocation (fixed ratio of dollar amounts)
    weights <- c(weights, 1-weights)
    rets_weighted <- retp %*% weights
    wealth_pda <- cumprod(1 + rets_weighted)
    
    # Calculate standardized prices and portfolio allocations
    volat <- volat()
    alloc <- lapply(1:NCOL(pricev), function(x) weights[x]/volat[, x])
    alloc <- do.call(cbind, alloc)
    # Scale allocations to 1 dollar total
    alloc <- alloc/rowSums(alloc)
    # Lag the allocations
    alloc <- rutils::lagit(alloc)
    # Calculate wealth of risk parity
    rets_weighted <- rowSums(retp*alloc)
    wealth_risk_parity <- cumprod(1 + rets_weighted)
    
    # Calculate log wealths
    wealth <- log(cbind(wealth_pda, wealth_risk_parity))
    wealth <- xts(wealth, index(pricev))
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(rutils::diffit(wealth), function (x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    colnames(wealth) <- c("Fixed Ratio", "Risk Parity")
    wealth
    
  })  # end reactive code
  
  
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    wealth <- wealth()
    colnamev <- colnames(wealth)
    
    captiont <- paste0("Sortino ratios: ", paste0(paste0(colnamev, " = ", values$sharper), collapse=" / "))

    # Plot log wealths
    dygraphs::dygraph(wealth, main=captiont) %>%
      dyOptions(colors=c("blue","red"), strokeWidth=2) %>%
      dyLegend(show="always", width=500)
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
