##############################
# This is a shiny app for simulating a contrarian strategy
# using returns scaled by the trading volume (volume clock).
#
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

oh_lc <- HighFreq::SPY
price_s <- drop(coredata(quantmod::Cl(oh_lc)))
vol_ume <- drop(coredata(quantmod::Vo(oh_lc)))
re_turns <- rutils::diff_it(log(price_s))
re_turns <- re_turns/sd(re_turns)


cap_tion <- "Cumulative Scaled Returns"

## End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(cap_tion),

  fluidRow(
    # The Shiny App is re-calculated when the actionButton is clicked and the re_calculate variable is updated
    column(width=12,
           h4("Click the button 'Recalculate the Model' to re-calculate the Shiny App."),
           actionButton("re_calculate", "Recalculate the Model"))
  ),  # end fluidRow

  # Create single row with two slider inputs
  fluidRow(
    column(width=3, sliderInput("expo_nent", label="exponent", min=0.05, max=2.0, value=0.5, step=0.05))
    
  ),  # end fluidRow

  # Create output plot panel
  mainPanel(dygraphOutput("dy_graph"), height=8, width=12)

)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {

  # Re-calculate the data and rerun the strategy
  da_ta <- reactive({
    # Get model parameters from input argument
    expo_nent <- isolate(input$expo_nent)
    input$re_calculate

    # Divide returns by the volume (volume clock).
    rets_scaled <- ifelse(vol_ume > 1e4, re_turns/(vol_ume^expo_nent), 0)
    rets_scaled <- rets_scaled/sd(rets_scaled)
    cum_returns <- cumsum(re_turns)
    cum_scaled <- cumsum(rets_scaled)
    da_ta <- cbind(cum_returns, cum_scaled)
    da_ta <- da_ta[60*(1:(NROW(da_ta) %/% 60)), ]
    da_ta <- xts::xts(da_ta, seq.POSIXt(from=index(first(oh_lc)), by="hour", length.out=NROW(da_ta)))
    col_names <- c("SPY Returns", "Scaled by Volume")
    colnames(da_ta) <- col_names
    da_ta
  })  # end reactive code

  # Plot the cumulative scaled returns
  # Return to the output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph({
    col_names <- colnames(da_ta())
    dygraphs::dygraph(da_ta(), main="SPY Prices") %>%
      dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
      dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
      dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue") %>%
      dyLegend(width=500)
  })  # end output plot

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
