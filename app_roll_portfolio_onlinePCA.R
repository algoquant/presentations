##############################
# This is a shiny app for backtesting a rolling portfolio 
# optimization strategy, which produces an interactive 
# dygraphs plot.
# It applies PCA dimension reduction of the inverse 
# covariance matrix.
# It calculates PCA using an online recursive algorithm 
# from package onlinePCA
# It performs the time loop in R not C++.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# load packages
library(shiny)
library(dygraphs)
library(rutils)

# Model and data setup
# source the model function
# source("/Users/jerzy/Develop/lecture_slides/scripts/back_test.R")
# dimax <- 2
# symbolv <- colnames(rutils::etfenv$returns)
# symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV"))]
# nstocks <- NROW(symbolv)
# rets <- rutils::etfenv$returns[, symbolv]

load("/Users/jerzy/Develop/data/sp500_returns.RData")
rets <- returns100["2000/"]
nstocks <- NCOL(rets)

rets[1, is.na(rets[1, ])] <- 0
rets <- zoo::na.locf(rets, na.rm=FALSE)
rets <- na.omit(rets)
dates <- zoo::index(rets)
riskf <- 0.03/260
# excess <- (rets - riskf)
# Calculate returns on equal weight portfolio
retsd <- zoo::coredata(rets)
indeks <- xts::xts(rowMeans(rets), dates)

# End setup code


## Create elements of the user interface
uiface <- shiny::fluidPage(
  titlePanel(paste0("Rolling Portfolio Optimization with PCA Dimension Reduction for ", nstocks, " Stocks")),
  
  # create single row with two slider inputs
  fluidRow(
    # Input end points interval
    column(width=2, selectInput("interval", label="End points Interval",
                                choices=c("weeks", "months", "years"), selected="months")),
    # Input number of eigenvalues for regularized matrix inverse
    column(width=2, sliderInput("dimax", label="Number of eigenvalues:",
                                min=2, max=(nstocks %/% 2), value=3, step=1)),
    # Input look-back interval
    # column(width=2, sliderInput("look_back", label="Lookback interval:",
    #                             min=1, max=30, value=12, step=1)),
    # Input forgetting rate for the mean
    column(width=2, sliderInput("lambdav", label="Forgetting factor:",
                                min=0.01, max=0.5, value=0.2, step=0.01)),
    # Input learning rate for the PCA
    column(width=2, sliderInput("gammav", label="Learning rate:",
                                min=0.9, max=0.999, value=0.99, step=0.001)),
    # Input the shrinkage intensity
    column(width=2, sliderInput("alpha", label="Shrinkage intensity:",
                                min=0.01, max=0.99, value=0.01, step=0.05))
  ),  # end fluidRow
  
  # create output plot panel
  dygraphs::dygraphOutput("dyplot", width="95%", height="600px")
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  # Recalculate the data and rerun the model
  pnls <- shiny::reactive({
    cat("Calculating the pnls", "\n")
    # get model parameters from input argument
    interval <- input$interval
    dimax <- input$dimax
    lambdav <- input$lambdav
    gammav <- input$gammav
    alpha <- input$alpha
    
    # Define end points
    endp <- rutils::calc_endpoints(rets, interval=interval)
    # endp <- ifelse(endp<(nstocks+1), nstocks+1, endp)
    endp <- endp[endp > 2*nstocks]
    npts <- NROW(endp)
    # Define startp
    # startp <- c(rep_len(1, look_back-1), endp[1:(npts-look_back+1)])
    # Rerun the model
    # pnls <- roll_portf(excess=excess, returns=rets, look_back=look_back, endp=endp, alpha=alpha, dimax=dimax)
    
    # Warmup
    warmup <- retsd[1:endp[1], ]
    pcav <- princomp(warmup)
    meanv <- pcav$center
    pcav <- list(values=pcav$sdev[1:nstocks]^2, vectors=pcav$loadings[, 1:nstocks])
    # Calculate the portfolio weights
    covinv <- pcav$vectors[, 1:dimax] %*% (t(pcav$vectors[, 1:dimax]) / pcav$values[1:dimax])
    colm <- colMeans(warmup)
    colmm <- mean(colm)
    colm <- (1-alpha)*colm + alpha*colmm
    weightv <- covinv %*% colm
    weightv <- weightv*sd(rowMeans(warmup))/sd(warmup %*% weightv)
    # pnls <- indeks
    
    pnls <- lapply(2:npts, function(ep) {
      # cat("weightv =", head(weightv), "\n")
      # Calculate the out-of-sample portfolio returns
      outsample <- retsd[(endp[ep-1]+1):endp[ep], ]
      pnls <- outsample %*% weightv
      # weightv <- drop(weightv/sqrt(sum(weightv^2)))
      # Update the PCA
      colm <- colMeans(outsample)
      meanv <- onlinePCA::updateMean(meanv, colm, ep-1, f=lambdav)
      pcav <- onlinePCA::ghapca(lambda=pcav$values, U=pcav$vectors, x=colm, gamma=gammav, center=meanv)
      covinv <- pcav$vectors[, 1:dimax] %*% (t(pcav$vectors[, 1:dimax]) / pcav$values[1:dimax])
      # Calculate the portfolio weights
      colmm <- mean(colm)
      colm <- (1-alpha)*colm + alpha*colmm
      weightv <<- covinv %*% colm
      weightv <<- weightv*sd(rowMeans(outsample))/sd(outsample %*% weightv)
      # Calculate the out-of-sample portfolio returns
      pnls
    })  # end lapply
    pnls <- do.call(rbind, pnls)
    
    # Plot dygraph of rolling ETF portfolio strategy
    pnls <- pnls*sd(indeks)/sd(pnls)
    pnls <- c(rowMeans(warmup), drop(pnls))
    # wealth <- cbind(indeks, pnls, (pnls+indeks)/2)
    # colnames(wealth) <- c("Index", "Strategy", "Combined")
    # Calculate the out-of-sample Sharpe and Sortino ratios
    # sqrt(252)*sapply(wealth,
    #                  function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
    # dygraphs::dygraph(cumsum(wealth)[endp], main="Monthly ETF Rolling Portfolio Strategy") %>%
    #   dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
    #   dyLegend(show="always", width=500)
    
    # pnls <- sd(rutils::diffit(indeks))*pnls/sd(rutils::diffit(pnls))
    pnls <- cbind(indeks, pnls)
    colnames(pnls) <- c("Index", "Strategy")
    pnls
  })  # end reactive code
  
  # return to output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    pnls <- pnls()
    colnamev <- colnames(pnls)
    endp <- rutils::calc_endpoints(pnls, interval=input$interval)
    
    # Calculate the Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    sharper <- round(sharper, 3)
    captiont <- paste(colnamev, "SR =", sharper, collapse=" ")
    captiont <- paste("Rolling Portfolio Optimization Strategy: ", captiont)

    dygraphs::dygraph(cumsum(pnls)[endp], main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
      dyLegend(show="always", width=500)
    # dygraphs::dygraph(cumsum(pnls)[endp], main="Rolling Portfolio Optimization Strategy") %>%
    #   dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
    #   dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
    #   dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokewidth=2, col="blue") %>%
    #   dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokewidth=2, col="red")
  })  # end output plot
  
  # output$dygraph <- dygraphs::renderDygraph({
  #   dygraph(pnls(), main="Rolling Portfolio Optimization Strategy") %>%
  #     dySeries("strategy", label="strategy", strokeWidth=1, color="red")
  # })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uiface, server=servfun)
