##############################
# This is a shiny app for backtesting a running PCA portfolio 
# strategy, which produces an interactive dygraphs plot.
# It performs the PCA in-sample and trades the principal 
# components out-of-sample.
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

## Load daily S&P500 stock returns
# load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# retp <- returns["2000/"]

## Load ETF returns
# retp <- rutils::etfenv$returns[, c("VTI", "IEF", "DBC")]
retp <- rutils::etfenv$returns[, c("VTI", "IEF", "DBC", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU", "USO")]
# symbolv <- colnames(rutils::etfenv$returns)
# symbolv <- symbolv[!(symbolv %in% c("TLT", "IEF", "MTUM", "QUAL", "VLUE", "USMV", "VXX", "SVXY", "IVE", "VTV"))]
# retp <- rutils::etfenv$returns[, symbolv]

# Overwrite NA values in returns
retp[is.na(retp)] <- 0
retp <- retp[!(rowSums(retp) == 0), ]
datev <- zoo::index(retp) # dates
nrows <- NROW(retp) # number of rows
nzeros <- colSums(retp == 0)
# Remove stocks with very little data
retp <- retp[, nzeros < nrows/4]
nstocks <- NCOL(retp) # number of stocks

indeks <- rowMeans(retp)
indeks <- xts::xts(indeks, order.by=datev)
colnames(indeks) <- "Index"
indeksd <- sd(indeks)


# Define in-sample and out-of-sample intervals
cutoff <- nrows %/% 2
insample <- 1:cutoff
outsample <- (cutoff + 1):nrows
# Calculate the PCA weights in-sample
retp <- retp[, !(colSums(retp[insample]) == 0)]
pcad <- prcomp(retp[insample], center=FALSE, scale=TRUE)
# Calculate the out-of-sample PCA time series
retscaled <- lapply(retp, function(x) x[outsample]/sd(x[insample]))
retscaled <- do.call(cbind, retscaled)
retspca <- xts::xts(retscaled %*% pcad$rotation, order.by=datev[outsample])
indeks <- indeks[outsample]

# pcad <- prcomp(retp, center=FALSE, scale=TRUE)
# retspca <- pcad$x


# End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Running Momentum Strategy"),
  
  # fluidRow(
  #   # The Shiny App is recalculated when the actionButton is clicked and the recalcb variable is updated
  #   column(width=12, 
  #          h4("Click the button 'Recalculate the Model' to Recalculate the Shiny App."),
  #          actionButton("recalcb", "Recalculate the Model"))
  # ),  # end fluidRow
  
  # Create single row with two slider inputs
  fluidRow(
    # Input number of eigenvalues for regularized matrix inverse
    column(width=3, sliderInput("dimax", label="Number of principal components:", min=2, max=nstocks, value=4, step=1)),
    column(width=3, sliderInput("lambda", label="Decay:", min=0.9, max=0.999, value=0.95, step=0.001)),
    column(width=3, sliderInput("lambdah", label="Weight decay:", min=0.7, max=0.99, value=0.85, step=0.01))
    # Input end points interval
    # column(width=4, selectInput("interval", label="End points Interval",
    #             choices=c("weeks", "months", "years"), selected="months")),
    # Input look-back interval
    # column(width=4, sliderInput("look_back", label="Look-back interval:",
    #                             min=1, max=30, value=12, step=1)),
    # Input end_stub interval
    # column(width=4, sliderInput("end_stub", label="End_stub interval:",
    #                             min=1, max=90, value=30, step=1)),
    # Input the shrinkage intensity
    # column(width=4, sliderInput("alpha", label="Shrinkage intensity:",
    #                             min=0.01, max=0.99, value=0.8, step=0.05))
  ),  # end fluidRow
  
  # Create output plot panel
  dygraphs::dygraphOutput("dyplot", width="90%", height="550px")
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Recalculate the data and rerun the model
  pnls <- shiny::reactive({
    # Get model parameters from input argument
    # interval <- isolate(input$interval)
    dimax <- input$dimax
    # alpha <- isolate(input$alpha)
    lambda <- input$lambda
    lambdah <- input$lambdah
    # end_stub <- input$end_stub
    # Model is recalculated when the recalcb variable is updated
    # input$recalcb
    
    # Define end points
    # endp <- ifelse(endp<(nweights+1), nweights+1, endp)
    # endp <- endp[endp > (nweights+1)]
    # nrows <- NROW(endp)
    # Define startp
    # startp <- c(rep_len(1, look_back-1), endp[1:(nrows-look_back+1)])

    retp <- retspca[, 1:dimax]
    
    # Rerun the model
    varm <- HighFreq::run_var(retp, lambda=lambda)
    perfstat <- HighFreq::run_mean(retp, lambda=lambda)
    weightv <- perfstat/varm
    weightv[varm == 0] <- 0
    weightv[1, ] <- 1
    weightv <- weightv/sqrt(rowSums(weightv^2))
    # Average the weights over holding period
    weightv <- HighFreq::run_mean(weightv, lambda=lambdah)
    weightv <- rutils::lagit(weightv)
    # Calculate the momentum profits and losses
    pnls <- as.numeric(rowSums(weightv*retp))

    # pnls <- back_test_r(excess, retv, startp, endp, alpha, dimax, end_stub)
    pnls <- indeksd*pnls/sd(pnls)
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
