##############################
# This is a shiny app for simulating a strategy using
# a simple AR(p) forecasting model.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(shiny)
library(dygraphs)
library(HighFreq)

# Model and data setup

symbol <- "VTI"
prices <- Cl(rutils::etfenv$VTI)
returns <- rutils::diffit(log(prices))

captiont <- paste("Strategy for", symbol, "Using Weekly and Monthly Returns")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
    # column(width=12, 
    #        h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
    #        actionButton("recalcb", "Recalculate the Model"))
  ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input end points interval
    # column(width=2, selectInput("interval", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input weight
    column(width=2, sliderInput("wei_ght", label="weight",
                                min=(-1), max=5, value=1.0, step=0.1)),
    # Input look-back lag interval
    column(width=2, sliderInput("look_back1", label="Lookback short", min=3, max=20, value=5, step=1)),
    # Input look-back lag interval
    column(width=2, sliderInput("look_back2", label="Lookback long", min=10, max=50, value=25, step=1))
    # Input the weight decay parameter
    # column(width=2, sliderInput("lambda", label="Weight decay:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input model weights type
    # column(width=2, selectInput("typev", label="Portfolio weights type",
    #                             choices=c("max_sharpe", "min_var", "min_varpca", "rank"), selected="rank")),
    # Input number of eigenvalues for regularized matrix inverse
    # column(width=2, sliderInput("dimax", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
    # Input the shrinkage intensity
    # column(width=2, sliderInput("alpha", label="Shrinkage intensity",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input the percentile
    # column(width=2, sliderInput("quant", label="percentile:", min=0.01, max=0.45, value=0.1, step=0.01)),
    # Input the strategy coefficient: coeff=1 for momentum, and coeff=-1 for contrarian
    # column(width=2, selectInput("coeff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    # column(width=2, numericInput("bid_offer", label="bid-offer:", value=0.001, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  # Recalculate the data and rerun the model
  datav <- shiny::reactive({
    # Get model parameters from input argument
    wei_ght <- input$wei_ght
    # dimax <- isolate(input$dimax)
    look_back1 <- input$look_back1
    look_back2 <- input$look_back2
    # look_lag <- isolate(input$look_lag
    # lambda <- isolate(input$lambda)
    # typev <- isolate(input$typev)
    # alpha <- isolate(input$alpha)
    # quant <- isolate(input$quant)
    # coeff <- as.numeric(isolate(input$coeff))
    # bid_offer <- isolate(input$bid_offer)
    # Model is recalculated when the recalcb variable is updated
    # input$recalcb

    
    week_ly <- rutils::diffit(log(prices), lagg=look_back1)
    week_ly <- as.numeric(week_ly)
    month_ly <- rutils::diffit(log(prices), lagg=look_back2)
    month_ly <- as.numeric(month_ly)
    
    # Rerun the model
    posit <- sign(wei_ght*week_ly + (1-wei_ght)*month_ly)
    positions_lag <- rutils::lagit(posit, lagg=2)
    pnls <- -cumsum(positions_lag*returns)
    pnls <- cbind(pnls, cumsum(returns))
    colnames(pnls) <- c("Strategy", "Index")
    # pnls[c(1, endp), ]
    pnls
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    colnamev <- colnames(datav())
    dygraphs::dygraph(datav(), main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue")
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
