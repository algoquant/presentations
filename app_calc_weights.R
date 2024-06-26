##############################
# This is a shiny app for visualizing calc_weights
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# load packages
library(HighFreq)
library(shiny)
library(dygraphs)
# Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/assignments/rcpp_strat.cpp")
# Model and data setup
# dimax <- 2
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# retv <- returns100
retv <- retv[, !is.na(retv[NROW(retv), ])]
retv <- retv[, !is.na(retv[NROW(retv)-1000, ])]
retv <- na.omit(retv)
nweights <- NCOL(retv)
# riskf <- 0.03/260
# excess <- (retv - riskf)
# calculate returns on equal weight portfolio
# indeks <- xts(cumsum(retv %*% rep(1/sqrt(nweights), nweights)), index(retv))


# End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Visualize Weights for S&P500 Portfolio"),
  
  # create single row with two slider inputs
  fluidRow(
    # Input number of eigenvalues for regularized matrix inverse
    column(width=4, numericInput("dimax", "Number of eigenvalues:", value=5)),
    # Input end points interval
    # column(width=4, selectInput("interval", label="End points Interval",
    #             choices=c("weeks", "months", "years"), selected="months")),
    # Input look-back interval
    # column(width=4, sliderInput("lookb", label="Lookback interval:",
    #                             min=1, max=30, value=12, step=1)),
    # Input end_stub interval
    # column(width=4, sliderInput("end_stub", label="End_stub interval:",
    #                             min=1, max=90, value=30, step=1)),
    # Input the shrinkage intensity
    # column(width=4, sliderInput("alpha", label="Shrinkage intensity:",
    #                             min=0.01, max=0.99, value=0.1, step=0.05))
  ),  # end fluidRow
  
  # create output plot panel
  plotOutput("plotobj")
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Recalculate the data and rerun the model
  weightv <- shiny::reactive({
    # get model parameters from input argument
    # interval <- input$interval
    dimax <- input$dimax
    # lookb <- input$lookb
    # end_stub <- input$end_stub
    # alpha <- input$alpha
    
    # Define end points
    # endp <- rutils::calc_endpoints(retv, interval=interval)
    # endp <- ifelse(endp<(nweights+1), nweights+1, endp)
    # endp <- endp[endp > (nweights+1)]
    # nrows <- NROW(endp)
    # Define startp
    # startp <- c(rep_len(1, lookb-1), endp[1:(nrows-lookb+1)])
    # rerun the model
    weightv <- HighFreq::calc_weights(retv, dimax=dimax);
    
    # pnls <- roll_portf_n(excess=retv, 
    #                               returns=retv,
    #                               startp=startp-1,
    #                               endp=endp-1,
    #                       dimax=dimax, 
    #                       alpha=alpha,
    #                       min_var=FALSE)
    # pnls[which(is.na(pnls)), ] <- 0
    # pnls <- sd(rutils::diffit(indeks))*pnls/sd(rutils::diffit(pnls))
    # pnls <- cumsum(pnls)
    # pnls <- cbind(pnls, indeks)
    # colnames(pnls) <- c("Strategy", "Index")
    # pnls[c(1, endp), ]
    sort(weightv)
  })  # end reactive code
  
  # return to output argument a dygraph plot with two y-axes
  output$plotobj <- shiny::renderPlot({
    plot(weightv())
  })  # end renderPlot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
