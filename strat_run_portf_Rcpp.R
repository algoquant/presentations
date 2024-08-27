##############################
# This is a shiny app for backtesting a running portfolio 
# strategy, which produces an interactive dygraphs plot.
# It runs compiled Rcpp code for either a portfolio 
# optimization strategy or a momentum strategy.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# load packages
library(shiny)
library(dygraphs)
library(HighFreq)
# Compile Rcpp code for the portfolio model
Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/back_test_run.cpp")


## Model and data setup

# Load S&P500 returns.
# load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# modelcap <- "S&P500 Sub-portfolio"
# retp <- returns100["2007/"]
# set.seed(1121)
# retp <- retp[, sample(NCOL(retp100), 30)]
# retp[1, is.na(retp[1, ])] <- 0
# retp <- zoo::na.locf(retp, na.rm=FALSE)

# Load ETF returns.
# Select all the ETF symbols except "VXX", "SVXY" "MTUM", "QUAL", "VLUE", and "USMV"
modelcap <- "ETF Portfolio"
# symbolv <- colnames(rutils::etfenv$returns)
# symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "IVE", "VTV", "AIEQ", "USMV"))]
# symbolv <- c("VXX", "SVXY", "USO", "VTI", "XLK", "TLT")
symbolv <- c("USO", "VTI", "XLK", "TLT")
retp <- na.omit(rutils::etfenv$returns[, symbolv])
# retp <- na.omit(rutils::etfenv$returns["2018-06/", symbolv])
datev <- index(retp)

nweights <- NCOL(retp)
riskf <- 0.03/260
# excess <- (retp - riskf)
# calculate returns on equal weight portfolio
indeks <- xts(retp %*% rep(1/sqrt(nweights), nweights), datev)

betav <- sapply(retp, function(x) {
  cov(retp$VTI, x)/var(retp$VTI)
}) # end sapply
# betav <- matrix(betav)

pcad <- prcomp(retp, center=TRUE, scale=TRUE)
retpca <- pcad$x


captiont <- paste("Running Portfolio Strategy for", paste(symbolv, collapse=", "))

# End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),
  
  # fluidRow(
  #   # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
  #   column(width=12, 
  #          h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
  #          actionButton("recalcb", "Recalculate the Model"))
  # ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input number of eigenvalues for regularized matrix inverse
    column(width=3, sliderInput("dimax", label="Number of eigenvalues::", min=2, max=20, value=4, step=1)),
    column(width=3, sliderInput("lambdaf", label="lambda:", min=0.9, max=0.999, value=0.9, step=0.001))
    # Input end points interval
    # column(width=4, selectInput("interval", label="End points Interval",
    #             choices=c("weeks", "months", "years"), selected="months")),
    # Input look-back interval
    # column(width=4, sliderInput("lookb", label="Look-back interval:",
    #                             min=1, max=30, value=12, step=1)),
    # Input end_stub interval
    # column(width=4, sliderInput("end_stub", label="End_stub interval:",
    #                             min=1, max=90, value=30, step=1)),
    # Input the shrinkage intensity
    # column(width=4, sliderInput("alpha", label="Shrinkage intensity:",
    #                             min=0.01, max=0.99, value=0.8, step=0.05))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="650px")
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Recalculate the data and rerun the model
  pnls <- shiny::reactive({
    # Get model parameters from input argument
    # interval <- isolate(input$interval)
    # dimax <- isolate(input$dimax)
    # alpha <- isolate(input$alpha)
    dimax <- input$dimax
    lambdaf <- input$lambdaf
    # end_stub <- input$end_stub
    # Model is recalculated when the recalcb variable is updated
    # input$recalcb
    
    # Define end points
    # endp <- ifelse(endp<(nweights+1), nweights+1, endp)
    # endp <- endp[endp > (nweights+1)]
    # nrows <- NROW(endp)
    # Define startp
    # startp <- c(rep_len(1, lookb-1), endp[1:(nrows-lookb+1)])
    # Rerun the model
    pnls <- back_testp(retp=retpca, dimax=dimax, lambda=lambdaf)
    pnls <- pnls[, 1]
    # pnls[which(is.na(pnls)), ] <- 0
    # pnls <- back_test_r(excess, retp, startp, endp, alpha, dimax, end_stub)
    pnls <- sd(indeks)*pnls/sd(pnls)
    pnls <- cbind(indeks, pnls)
    colnames(pnls) <- c("Index", "Strategy")
    pnls
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    # Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 4)
    
    # Bind with indicators
    captiont <- paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / ")

    endp <- rutils::calc_endpoints(pnls, interval="weeks")
    dygraphs::dygraph(cumsum(pnls)[endp], main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
      dyLegend(width=300)
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
