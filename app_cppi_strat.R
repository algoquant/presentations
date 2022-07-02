##############################
# This is a shiny app for simulating a CPPI strategy.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(rutils)
library(shiny)
library(dygraphs)

# Model and data setup

# Calculate VTI returns
returns <- na.omit(rutils::etfenv$returns$VTI["2007/2012"])
dates <- zoo::index(returns)
nrows <- NROW(returns)
returns <- drop(zoo::coredata(returns))

captiont <- "CPPI Strategy"

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
    # Input bond floor
    column(width=2, sliderInput("bfloor", label="Bond floor", min=0, max=100, value=60, step=1)),
    # Input exponent for variance
    # column(width=2, sliderInput("exponent", label="Std Dev exponent:",
    #                             min=0.25, max=2.5, value=1.0, step=0.05)),
    # Input CPPI multiplier
    column(width=2, sliderInput("coeff", label="CPPI multiplier", min=0.1, max=3.0, value=2.0, step=0.1))
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
  
  # Calculate CPPI strategy
  datav <- shiny::reactive({
    cat("Calculating the CPPI\n")
    
    # Get model parameters from input argument
    bfloor <- input$bfloor
    coeff <- input$coeff
    
    ## Initialize CPPI strategy
    # Portfolio market values
    portfv <- numeric(nrows)
    # Initial principal
    portfv[1] <- 100
    # Stock allocation
    stockv <- numeric(nrows)
    stockv[1] <- min(coeff*(portfv[1] - bfloor), portfv[1])
    # Bond allocation
    bondv <- numeric(nrows)
    bondv[1] <- (portfv[1] - stockv[1])
    
    # Simulate CPPI strategy
    for (t in 2:nrows) {
      portfv[t] <- portfv[t-1] + stockv[t-1]*returns[t]
      stockv[t] <- min(coeff*(portfv[t] - bfloor), portfv[t])
      bondv[t] <- (portfv[t] - stockv[t])
    }  # end for
    
    # Return the data
    vti <- 100*cumprod(1+returns)
    datav <- cbind(stockv, bondv, vti, portfv)
    datav <- xts::xts(datav, dates)
    colnames(datav) <- c("Stocks", "Bonds", "VTI", "CPPI")
    datav
  })  # end reactive code
  
  
  # Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    datav <- datav()

    # Calculate Sortino ratios
    sharper <- sqrt(252)*sapply(rutils::diffit(log(datav[, 3:4])), function (x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 3)

    colnamev <- colnames(datav[, 3:4])
    captiont <- paste0("Sortino ratios: ", paste0(paste0(colnamev, " = ", sharper), collapse=" / "))
    
    # dygraph plot of CPPI strategy
    dygraphs::dygraph(datav, main=captiont) %>%
      dyOptions(colors=c("red", "green", "blue", "orange"), strokeWidth=2) %>%
      dyLegend(show="always", width=300)
    
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
