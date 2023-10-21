##############################
# This is a shiny app for simulating a risk parity strategy 
# for VTI and TLT.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(rutils)
library(shiny)
library(dygraphs)

# Model and data setup

# Calculate the dollar and percentage returns for stocks and bonds
pricev <- rutils::etfenv$prices[, c("VTI", "TLT")]
pricev <- na.omit(pricev)
retd <- rutils::diffit(pricev)
retp <- retd/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)

captiont <- "Log Wealth of Risk Parity vs Proportional Dollar Allocations for VTI and TLT"

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
    # Input lambda decay factor
    column(width=2, sliderInput("lambda", label="Lambda decay factor:", min=0.01, max=0.9, value=0.2, step=0.01)),
    # Input VTI weight
    column(width=2, sliderInput("weightv", label="VTI weight", min=0.4, max=0.6, value=0.5, step=0.01))
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
    lambda <- input$lambda

    # Calculate rolling percentage volatility
    volat <- HighFreq::run_var(retd, lambda=lambda)

    sqrt(volat)

  })  # end reactive code
  
  
  # Calculate wealths
  wealthv <- reactive({
    cat("Calculating the wealths\n")
    
    # Get model parameters from input argument
    weightv <- input$weightv

    # Calculate wealth of proportional dollar allocation (fixed ratio of dollar amounts)
    weightv <- c(weightv, 1-weightv)
    retw <- retp %*% weightv
    wealthpd <- cumprod(1 + retw)

    # Calculate standardized prices and portfolio weights
    volat <- volat()
    volat <- rutils::lagit(volat)
    volat[1:2, ] <- 1
    
    # Calculate the standardized prices with unit dollar volatility
    pricerp <- pricev/volat
    # Scale the sum of stock prices to $2
    pricerp <- 2*pricerp/rowSums(pricerp)
    # Calculate the risk parity returns
    retsd <- retp*pricerp
    # Calculate the wealth of risk parity
    wealthrp <- 1 + cumsum(retsd %*% weightv)
    
    # Calculate log wealths
    wealthv <- log(cbind(wealthpd, wealthrp))
    wealthv <- xts::xts(wealthv, zoo::index(pricev))
    colnames(wealthv) <- c("PropDollars", "Risk Parity")
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(rutils::diffit(wealthv), function (x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    wealthv
    
  })  # end reactive code
  
  
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    wealthv <- wealthv()
    colnamev <- colnames(wealthv)
    
    captiont <- paste0("Sortino ratios: ", paste0(paste0(colnamev, " = ", values$sharper), collapse=" / "))

    # Plot log wealths
    endd <- rutils::calc_endpoints(wealthv, interval="weeks")
    dygraphs::dygraph(wealthv[endd], main=captiont) %>%
      dyOptions(colors=c("blue","red"), strokeWidth=2) %>%
      dyLegend(show="always", width=500)
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
