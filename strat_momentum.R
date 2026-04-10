##############################
# This is a shiny app for simulating a momentum strategy 
# for overnight stock returns.
#
# Just press the "Run App" button on upper right of this panel.
##############################


## Model and data setup
## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Load the overnight returns of SP500 stocks
if (!exists("reton") || !exists("retstock") || !exists("symboln")) {
  cat("Loading the S&P500 overnight returns.\n")
  load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
  # load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns_idiosyncratic.RData")
  # retstock <- retid
  load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns_overnight.RData")
  # symbolv <- sort(names(sp500env))
} # end if


# Calculate the index returns as the average of the overnight returns of all stocks
# reton[1:3, ] <- 0
datev <- index(reton)
nstocks <- NCOL(reton)
retm <- rowMeans(reton, na.rm=TRUE)
retm <- xts::xts(retm, order.by=datev)
symboln <- "Index"
volt <- 0.03 ##  Volatility target for scaling the strategy PnLs

retstock <- retstock[datev]
datev <- index(retstock)
nrows <- length(datev)
reton <- reton[datev]
nstocks <- NCOL(reton)
retm <- rowMeans(reton, na.rm=TRUE)
retm <- xts::xts(retm, order.by=datev)

stringv <- "Overnight"
captiont <- paste("Momentum With Overnight Stock Returns")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    ##  Input stock symboln
    # column(width=1, selectInput("symboln", label="Symbol", choices=symbolv, selected=symboln)),
    ##  Input lambda decay parameter
    column(width=2, sliderInput("lambdaf", label="Lambda", min=0.1, max=0.99, value=0.98, step=0.01)),
    # Input the bid-ask spread
    column(width=2, numericInput("bidask", label="Bid-ask:", value=0.001, step=0.0001)),
    # Input Kelly threshold
    column(width=2, sliderInput("threshv", label="Threshold:", min=0.7, max=11.0, value=3.8, step=0.1)),
  ),  ##  end fluidRow

  ##  Render the plot in a new row
  fluidRow(
    dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  ),  ##  end fluidRow
  
)  ##  end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  ##  Create an empty list of reactive values.
  values <- reactiveValues()

  ##  Recalculate the strategy
  pnls <- shiny::reactive({
    
    # symboln <- input$symboln
    # cat("Recalculating PnLs for", symboln, "\n")

    lambdaf <- input$lambdaf
    bidask <- input$bidask
    threshv <- input$threshv
    
    # Calculate the Kelly ratios
    varm <- HighFreq::run_var(reton, lambda=lambdaf)
    retsm <- varm[, 1:nstocks]
    varm <- varm[, (nstocks+1):(2*nstocks)]
    varm[1:3] <- 1.0
    varm <- varm + 1e-8; # Add small number to prevent division by zero
    kellyr <- retsm*volt^2/varm
    # kellyr[is.na(kellyr)] <- 0
    # Apply the Kelly threshold to the Kelly ratios
    madv <- matrixStats::rowMads(kellyr, na.rm=TRUE)
    # Set small Kelly ratios to zero
    kellyr[abs(kellyr) < threshv*madv] <- 0
    # No shorting, so set negative Kelly ratios to zero
    kellyr[kellyr < 0] <- 0
    # Lag the Kelly ratios to avoid look-ahead bias
    kellyr <- rutils::lagit(kellyr, lagg=2)
    # Scale the Kelly ratios so the portfolio has the target volatility
    kellyr <- kellyr*volt/sqrt(rowSums((kellyr^2)*varm, na.rm=TRUE))
    # Scale the Kelly ratios so their sum of squares is equal to one
    # kellyr <- kellyr/sqrt(rowSums(kellyr^2, na.rm=TRUE))
    # Calculate transaction costs
    flipi <- rutils::diffit(kellyr)
    costs <- 0.5*bidask*rowSums(abs(flipi), na.rm=TRUE)
    costs[(is.na(costs) | is.infinite(costs))] <- 0

    # Calculate PnLs as the daily returns times the Kelly ratios
    # pnls <- retstock*kellyr
    # Calculate PnLs as the overnight returns times the Kelly ratios
    pnls <- reton*kellyr
    pnls <- rowSums(pnls, na.rm=TRUE)
    pnls[is.na(pnls)] <- 0
    pnls <- (pnls - costs)
    pnls <- pnls*sd(retm[retm<0])/sd(pnls[pnls<0])

    ##  Bind together strategy pnls
    pnls <- cbind(retm, pnls)
    colnames(pnls) <- c(stringv, "Strategy")

    ##  Calculate Sharpe ratios
    pnls <- pnls["2024/"]
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    
    return(cumsum(pnls))

  })  ##  end Recalculate the strategy
  

  ##  Plot dygraph of the cumulative PnLs
  output$dyplot <- dygraphs::renderDygraph({
    
    ##  Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    ##  Get Sharpe ratios
    sharper <- values$sharper

    ##  Create the dygraph object
    captiont <- paste0(c(paste0(stringv, " SR="), "Momentum SR="), sharper, collapse=" / ")
    dyplot <- dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
      dyLegend(show="always", width=200)
    
    ##  Plot the dygraph object
    return(dyplot)


  })  ##  end output plot

}  ##  end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
