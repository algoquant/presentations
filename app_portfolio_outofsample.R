##############################
# This is a shiny app for simulating a portfolio 
# optimization strategy out-of-sample.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# load packages
library(HighFreq)
library(shiny)
library(dygraphs)
# Model and data setup

riskf <- 0.03/260

## End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  # titlePanel(captiont),
  # titlePanel("Portfolio Optimization Strategy Out-of-Sample for ETF Portfolio or for Sub-Portfolio of S&P500 Stocks"),
  titlePanel("Portfolio Optimization Strategy Out-of-Sample"),
  
  # Create single row with two slider inputs
  fluidRow(
    # Input choice of data
    column(width=2, selectInput("datas", label="Data",
                                choices=c("etf", "sp500"), selected="sp500")),
    column(width=2, sliderInput("eigen_max", label="Number of eigenvalues",
                                min=2, max=100, value=11, step=1)),
    # Input the shrinkage intensity
    column(width=2, sliderInput("alpha", label="Shrinkage intensity",
                                min=0.01, max=0.99, value=0.01, step=0.05))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="550px")
  # Create output plot panel
  # mainPanel(dygraphs::dygraphOutput("dyplot"), width=12)
  
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {
  
  ## Create an empty list of reactive values.
  globals <- reactiveValues()
  
  # Load the data
  rets <- shiny::reactive({
    # Get model parameters from input argument
    datas <- input$datas
    
    # Load data if needed
    switch(datas,
           "etf" = {
             cat("Loading ETF data \n")
             captiont <- "Portfolio Optimization Strategy Out-of-Sample for ETF Portfolio"
             # captiont <- paste("Contrarian Strategy for", symbol, "Using the Hampel Filter Over Prices")
             # Load PCA data
             # Select all the ETF symbols except "VXX", "SVXY" and "MTUM"
             symbolv <- rutils::etfenv$symbolv
             symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "TLT", "IEF", "MTUM", "QUAL", "VLUE", "USMV"))]
             rets <- rutils::etfenv$returns[, symbolv]
           },
           "sp500" = {
             cat("Loading S&P500 data \n")
             captiont <- "Portfolio Optimization Strategy Out-of-Sample for Sub-Portfolio of S&P500 Stocks"
             # Load S&P500 stock returns
             # cat("sp500 init load \n")
             load("/Users/jerzy/Develop/data/sp500_returns.RData")
             # Select data after 2000
             rets <- returns["2000/"]
           }
    )  # end switch
    
    # Copy over NA values
    rets[1, is.na(rets[1, ])] <- 0
    rets <- zoo::na.locf(rets, na.rm=FALSE)
    # Calculate eigen decomposition of covariance matrix
    eigend <- eigen(cov(rets["/2010"]))
    globals$eigenvec <- eigend$vectors
    globals$eigenval <- eigend$values
    
    rets
    
  })  # end Load the data


  # Recalculate the strategy
  pnls <- shiny::reactive({
    cat("Recalculating the strategy\n")
    
    rets <- rets()
    # Calculate in-sample and out-of-sample returns
    retsis <- rets["/2010"]
    retsos <- rets["2011/"]
    retsx <- (retsis - riskf)
    
    eigen_max <- input$eigen_max
    alpha <- input$alpha
    
    # Calculate regularized inverse of covariance matrix
    eigenvec <- globals$eigenvec
    eigenval <- globals$eigenval
    covinv <- eigenvec[, 1:eigen_max] %*%
      (t(eigenvec[, 1:eigen_max]) / eigenval[1:eigen_max])
    
    # Shrink the in-sample returns to their mean
    retsx <- colMeans(retsx)
    retsx <- (1-alpha)*retsx + alpha*mean(retsx)
    
    # Calculate portfolio weights
    weightv <- covinv %*% retsx
    weightv <- drop(weightv/sum(weightv))
    names(weightv) <- colnames(rets)
    
    # Maximum Sharpe weights in-sample interval
    # Calculate portfolio returns
    insample <- retsis %*% weightv
    outsample <- retsos %*% weightv
    # Calculate returns on equal weight portfolio
    indeks <- xts::xts(rowMeans(rets), zoo::index(rets))
    
    # Combine in-sample and out-of-sample returns
    pnls <- rbind(insample, outsample)
    pnls <- pnls*sd(indeks)/sd(pnls)
    pnls <- cbind(indeks, pnls)
    colnames(pnls) <- c("Equal Weight", "Optimal")
    
    sharper <- sqrt(252)*sapply(list(insample, outsample), function(x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 3)
    # pnls <- cumprod(1 + pnls)
    colnames(pnls) <- c("Equal Weight", "Optimal")
    captiont <- paste0(c("Insample SR = ", "Outsample SR = "), sharper)
    captiont <- paste("Optimal Portfolio Returns: ", paste(captiont, collapse=" / "))
    list(captiont=captiont, pnls=pnls)
    
  })  # end reactive code
  
  # Return to output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    captiont <- pnls()$captiont
    pnls <- pnls()$pnls
    endp <- rutils::calc_endpoints(pnls, interval="months")
    dygraphs::dygraph(cumsum(pnls)[endp], main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
      dyEvent(as.Date("2011-01-01"), label="in-sample", strokePattern="solid", color="red") %>%
      dyLegend(width=500)
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
