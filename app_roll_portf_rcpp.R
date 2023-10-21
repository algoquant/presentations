##############################
# This is a shiny app for backtesting a minimum variance 
# rolling portfolio optimization strategy, which produces 
# an interactive dygraphs plot.
# 
# This is an old student version that compiles Rcpp code 
# from package HighFreq.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# load packages
library(shiny)
library(dygraphs)
library(HighFreq)
# Compile Rcpp code from package HighFreq
Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/assignments/rcpp_strat.cpp")
# Model and data setup
load("/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
retv <- returns100
nweights <- NCOL(retv)
riskf <- 0.03/260
excess <- (retv - riskf)
# calculate returns on equal weight portfolio
indeks <- xts(cumsum(retv %*% rep(1/sqrt(nweights), nweights)), index(retv))


# End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Rolling Portfolio Optimization Strategy for S&P500 Sub-portfolio"),
  
  fluidRow(
    # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
    column(width=12, 
           h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
           actionButton("recalcb", "Recalculate the Model"))
  ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input number of eigenvalues for regularized matrix inverse
    column(width=4, numericInput("dimax", "Number of eigenvalues:", value=45)),
    # Input end points interval
    column(width=4, selectInput("interval", label="End points Interval",
                choices=c("weeks", "months", "years"), selected="months")),
    # Input look-back interval
    column(width=4, sliderInput("look_back", label="Look-back interval:",
                                min=1, max=30, value=12, step=1)),
    # Input end_stub interval
    # column(width=4, sliderInput("end_stub", label="End_stub interval:",
    #                             min=1, max=90, value=30, step=1)),
    # Input the shrinkage intensity
    column(width=4, sliderInput("alpha", label="Shrinkage intensity:",
                                min=0.01, max=0.99, value=0.8, step=0.05))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="550px")
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Recalculate the data and rerun the model
  datav <- shiny::reactive({
    # Get model parameters from input argument
    interval <- isolate(input$interval)
    dimax <- isolate(input$dimax)
    alpha <- isolate(input$alpha)
    look_back <- isolate(input$look_back)
    # end_stub <- input$end_stub
    # Model is recalculated when the recalcb variable is updated
    input$recalcb
    
    # Define end points
    endp <- rutils::calc_endpoints(retv, interval=interval)
    # endp <- ifelse(endp<(nweights+1), nweights+1, endp)
    endp <- endp[endp > (nweights+1)]
    nrows <- NROW(endp)
    # Define startp
    startp <- c(rep_len(1, look_back-1), endp[1:(nrows-look_back+1)])
    # Rerun the model
    pnls <- back_test(excess=retv, 
                        returns=retv,
                        startp=startp-1,
                        endp=endp-1,
                        dimax=dimax, 
                        alpha=alpha)
    pnls[which(is.na(pnls)), ] <- 0
    # pnls <- back_test_r(excess, retv, startp, endp, alpha, dimax, end_stub)
    # pnls <- sd(rutils::diffit(indeks))*pnls/sd(rutils::diffit(pnls))
    pnls <- cumsum(pnls)
    pnls <- cbind(pnls, indeks)
    colnames(pnls) <- c("Strategy", "Index")
    pnls[c(1, endp), ]
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    colnamev <- colnames(datav())
    dygraphs::dygraph(datav(), main="Rolling Portfolio Optimization Strategy") %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="red") %>%
      dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="blue")
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
