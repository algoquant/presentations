##############################
# This is a shiny app for calculating the returns of 
# a portfolio of ETFs defined by the weights input
# by the user.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# Load packages
library(HighFreq)
library(shiny)
library(dygraphs)

## Model and data setup

# Find ETFs with largest variance ratios
price_s <- log(rutils::etf_env$price_s)
sym_bols <- colnames(price_s)
sym_bols <- sym_bols[!(sym_bols %in% c("VXX", "SVXY", "MTUM", "IEF"))]
price_s <- price_s[, sym_bols]
lagg <- 5
ratio_s <- sapply(price_s, function(x) {
  cat("x=", names(x), "\n")
  x <- na.omit(x)
  if (NROW(x) > 100)
    drop(HighFreq::calc_var_ag(x, lagg)/HighFreq::calc_var_ag(x)/lagg)
  else NULL
})  # end sapply
ratio_s <- sort(unlist(ratio_s), decreasing=TRUE)
sym_bols <- names(ratio_s)

# Select ETFs with largest variance ratios
n_cols <- 4
name_s <- names(ratio_s)[1:n_cols]
re_turns <- rutils::etf_env$re_turns[, name_s]
re_turns <- na.omit(re_turns)


# Calculate Hurst exponent from returns
end_p <- rutils::calc_endpoints(re_turns, inter_val=lagg)
calc_hurst_rets <- function(rets, end_p) {
  cum_sum <- cumsum(rets)
  range_ratios <- sapply(seq_along(end_p)[-1], function(it) {
    start_point <- end_p[it-1]
    end_point <- end_p[it]
    rets <- rets[start_point:end_point]
    cum_sum <- cum_sum[start_point:end_point]
    log((max(cum_sum) - min(cum_sum))/sd(rets))/log(end_point-start_point)
  })  # end sapply
  median(na.omit(range_ratios))
}  # end calc_hurst_rets

# End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel(paste("Portfolio Optimization for", n_cols, "ETFs")),
  
  # Create single row with two slider inputs
  fluidRow(
    # Input weights
    column(width=2, sliderInput("weight1", label=paste0("Weight for ", name_s[1], ":"),
                                min=-10, max=10, value=5, step=0.1)),
    column(width=2, sliderInput("weight2", label=paste0("Weight for ", name_s[2], ":"),
                                min=-10, max=10, value=0, step=0.1)),
    column(width=2, sliderInput("weight3", label=paste0("Weight for ", name_s[3], ":"),
                                min=-10, max=10, value=0, step=0.1)),
    column(width=2, sliderInput("weight4", label=paste0("Weight for ", name_s[4], ":"),
                                min=-10, max=10, value=0, step=0.1))
  ),  # end fluidRow
  
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph"), width=12)
)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {

  # Recalculate the data and rerun the model
  da_ta <- reactive({
    # get model parameters from input argument
    weight1 <- input$weight1
    weight2 <- input$weight2
    weight3 <- input$weight3
    weight4 <- input$weight4

    weight_s <- c(weight1, weight2, weight3, weight4)
    (re_turns %*% weight_s)
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dy_graph <- dygraphs::renderDygraph({
    pnl_s <- da_ta()
    # Variance ratio
    # tre_nd <- HighFreq::calc_var_ag(pnl_s, lagg)/HighFreq::calc_var_ag(pnl_s)/lagg
    # Hurst
    tre_nd <- calc_hurst_rets(pnl_s, end_p)
    # Autocorrelation
    # pnl_s <- (pnl_s - mean(pnl_s))
    # tre_nd <- mean(pnl_s*rutils::lag_it(pnl_s))/drop(var(pnl_s))
    pnl_s <- xts::xts(cumsum(pnl_s), zoo::index(re_turns))
    dygraphs::dygraph(pnl_s, main=paste("Portfolio Optimization for", n_cols, "ETFs", 
                                        "Trend indicator =", round(tre_nd, 4)))
    # col_names <- colnames(da_ta())
    # dygraphs::dygraph(da_ta(), main="ETF Portfolio Optimization") %>%
    #   dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
    #   dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
    #   dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
    #   dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue")
  })  # end output plot
  
  # output$dygraph <- dygraphs::renderDygraph({
  #   dygraph(da_ta(), main="Rolling Portfolio Optimization Strategy") %>%
  #     dySeries("strategy", label="strategy", strokeWidth=1, color="red")
  # })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
