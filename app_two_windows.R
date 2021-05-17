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

sym_bol <- "VTI"
price_s <- Cl(rutils::etf_env$VTI)
re_turns <- rutils::diff_it(log(price_s))

cap_tion <- paste("Strategy for", sym_bol, "Using Weekly and Monthly Returns")

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(cap_tion),

  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the re_calculate variable is updated
    # column(width=12, 
    #        h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
    #        actionButton("re_calculate", "Recalculate the Model"))
  ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input end points interval
    # column(width=2, selectInput("inter_val", label="End points Interval",
    #                             choices=c("days", "weeks", "months", "years"), selected="days")),
    # Input weight
    column(width=2, sliderInput("wei_ght", label="weight",
                                min=(-1), max=5, value=1.0, step=0.1)),
    # Input look-back lag interval
    column(width=2, sliderInput("look_back1", label="Lookback short", min=3, max=20, value=5, step=1)),
    # Input look-back lag interval
    column(width=2, sliderInput("look_back2", label="Lookback long", min=10, max=50, value=25, step=1))
    # Input the weight decay parameter
    # column(width=2, sliderInput("lamb_da", label="Weight decay:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input model weights type
    # column(width=2, selectInput("typ_e", label="Portfolio weights type",
    #                             choices=c("max_sharpe", "min_var", "min_varpca", "rank"), selected="rank")),
    # Input number of eigenvalues for regularized matrix inverse
    # column(width=2, sliderInput("max_eigen", "Number of eigenvalues", min=2, max=20, value=15, step=1)),
    # Input the shrinkage intensity
    # column(width=2, sliderInput("al_pha", label="Shrinkage intensity",
    #                             min=0.01, max=0.99, value=0.1, step=0.05)),
    # Input the percentile
    # column(width=2, sliderInput("percen_tile", label="percentile:", min=0.01, max=0.45, value=0.1, step=0.01)),
    # Input the strategy coefficient: co_eff=1 for momentum, and co_eff=-1 for contrarian
    # column(width=2, selectInput("co_eff", "Coefficient:", choices=c(-1, 1), selected=(-1))),
    # Input the bid-offer spread
    # column(width=2, numericInput("bid_offer", label="bid-offer:", value=0.001, step=0.001))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph"), width=12)
  
)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {
  
  # Recalculate the data and rerun the model
  da_ta <- reactive({
    # Get model parameters from input argument
    wei_ght <- input$wei_ght
    # max_eigen <- isolate(input$max_eigen)
    look_back1 <- input$look_back1
    look_back2 <- input$look_back2
    # look_lag <- isolate(input$look_lag
    # lamb_da <- isolate(input$lamb_da)
    # typ_e <- isolate(input$typ_e)
    # al_pha <- isolate(input$al_pha)
    # percen_tile <- isolate(input$percen_tile)
    # co_eff <- as.numeric(isolate(input$co_eff))
    # bid_offer <- isolate(input$bid_offer)
    # Model is recalculated when the re_calculate variable is updated
    # input$re_calculate

    
    week_ly <- rutils::diff_it(log(price_s), lagg=look_back1)
    week_ly <- as.numeric(week_ly)
    month_ly <- rutils::diff_it(log(price_s), lagg=look_back2)
    month_ly <- as.numeric(month_ly)
    
    # Rerun the model
    position_s <- sign(wei_ght*week_ly + (1-wei_ght)*month_ly)
    positions_lag <- rutils::lag_it(position_s, lagg=2)
    pnl_s <- -cumsum(positions_lag*re_turns)
    pnl_s <- cbind(pnl_s, cumsum(re_turns))
    colnames(pnl_s) <- c("Strategy", "Index")
    # pnl_s[c(1, end_points), ]
    pnl_s
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph({
    col_names <- colnames(da_ta())
    dygraphs::dygraph(da_ta(), main=cap_tion) %>%
      dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
      dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
      dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue")
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
