##############################
# This is a shiny app for backtesting a running PCA momentum 
# strategy, which produces an interactive dygraphs plot.
# It updates the principal components using an online model
# and trades them.
# It uses separate decay factors for returns and for covariance. 
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
# retsp <- returns100["2000/"]

## Load ETF returns
retsp <- rutils::etfenv$returns[, c("VTI", "TLT", "DBC", "USO", "XLF", "XLK")]
# retsp <- rutils::etfenv$returns[, c("VTI", "IEF", "DBC", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU", "USO")]
# symbolv <- colnames(rutils::etfenv$returns)
# symbolv <- symbolv[!(symbolv %in% c("TLT", "IEF", "MTUM", "QUAL", "VLUE", "USMV", "VXX", "SVXY", "IVE", "VTV"))]
# retsp <- rutils::etfenv$returns[, symbolv]

retsp <- na.omit(retsp)


# Overwrite NA values in returns
retsp[is.na(retsp)] <- 0
retsp <- retsp[!(rowSums(retsp) == 0), ]
datev <- zoo::index(retsp) # dates
nrows <- NROW(retsp) # number of rows
nzeros <- colSums(retsp == 0)
# Remove stocks with very little data
retsp <- retsp[, nzeros < nrows/2]
nstocks <- NCOL(retsp) # number of stocks

indeks <- rowMeans(retsp)
indeks <- xts::xts(indeks, order.by=datev)
colnames(indeks) <- "Index"
indeksd <- sd(indeks)


# Define in-sample and out-of-sample intervals
# cutoff <- nrows %/% 2
# insample <- 1:cutoff
# outsample <- (cutoff + 1):nrows
# Calculate the PCA weights in-sample
# retsp <- retsp[, !(colSums(retsp[insample]) == 0)]
# pcad <- prcomp(retsp[insample], center=FALSE, scale=TRUE)
# Calculate the out-of-sample PCA time series
# retscaled <- lapply(retsp, function(x) x[outsample]/sd(x[insample]))
# retscaled <- do.call(cbind, retscaled)
# retspca <- xts::xts(retscaled %*% pcad$rotation, order.by=datev[outsample])
# indeks <- indeks[outsample]

# pcad <- prcomp(retsp, center=FALSE, scale=TRUE)
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
    # checkboxInput("scalit", label="Scale returns", value=TRUE),
    column(width=2, sliderInput("dimax", label="Number of eigen vectors:", min=2, max=nstocks, value=6, step=1)),
    column(width=3, sliderInput("lambda", label="Returns decay:", min=0.96, max=0.999, value=0.995, step=0.001)),
    column(width=3, sliderInput("lambdacov", label="Covariance decay:", min=0.96, max=0.999, value=0.992, step=0.001)),
    column(width=3, sliderInput("lambdaw", label="Weight decay:", min=0.3, max=0.9, value=0.7, step=0.1))
    # Scale returns
    # column(width=2, selectInput("scalit", label="Scale returns", choices=c("True", "False"), selected="TRUE")),
    # Flip signs of the principal components
    # column(width=2, selectInput("flipc", label="Flip PC", choices=c("True", "False"), selected="TRUE"))
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
    lambdacov <- input$lambdacov
    lambdaw <- input$lambdaw
    # scalit <- as.logical(input$scalit)
    # flipc <- as.logical(input$flipc)
    # end_stub <- input$end_stub
    # Model is recalculated when the recalcb variable is updated
    # input$recalcb
    
    # Define end points
    # endp <- ifelse(endp<(nweights+1), nweights+1, endp)
    # endp <- endp[endp > (nweights+1)]
    # nrows <- NROW(endp)
    # Define startp
    # startp <- c(rep_len(1, look_back-1), endp[1:(nrows-look_back+1)])

    # retsp <- retspca[, 1:dimax]
    
    # Rerun the model
    pnls <- run_portf(retsp, dimax, lambda, lambdacov, lambdaw)
    pnls <- pnls[, 1]
    
    
    # varm <- HighFreq::run_var(retsp, lambda=lambda)
    # perfstat <- HighFreq::run_mean(retsp, lambda=lambda)
    # weightv <- perfstat/varm
    # weightv[varm == 0] <- 0
    # weightv[1, ] <- 1
    # weightv <- weightv/sqrt(rowSums(weightv^2))
    # Average the weights over holding period
    # weightv <- HighFreq::run_mean(weightv, lambda=lambdacov)
    # weightv <- rutils::lagit(weightv)
    # Calculate the momentum profits and losses
    # pnls <- as.numeric(rowSums(weightv*retsp))

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
