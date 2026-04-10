##############################
# This is a shiny app for simulating a momentum strategy 
# for overnight stock returns and plotting the strategy 
# PnLs and Kelly ratios in an interactive dygraphs plot.
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
if (!exists("reton")) {
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
reton <- reton[datev]
nstocks <- NCOL(reton)
retm <- rowMeans(reton, na.rm=TRUE)
retm <- xts::xts(retm, order.by=datev)

stringv <- "Overnight"
captiont <- paste("Momentum With Overnight Stock Returns")

# Calculate the Kelly ratios
lambdaf <- 0.9
varm <- HighFreq::run_var(reton, lambda=lambdaf)
retsm <- varm[, 1:nstocks]
varm <- varm[, (nstocks+1):(2*nstocks)]
varm[1:3] <- 1.0
varm <- varm + 1e-8; # Add small number to prevent division by zero
kellyr <- retsm*volt^2/varm
# kellyr[is.na(kellyr)] <- 0
# Apply the Kelly threshold to the Kelly ratios
# madv <- matrixStats::rowMads(kellyr, na.rm=TRUE)
# Set small Kelly ratios to zero
# kellyr[kellyr < threshv*madv] <- 0
# No shorting, so set negative Kelly ratios to zero
# kellyr[kellyr < 0] <- 0
# Lag the Kelly ratios by one day to avoid look-ahead bias
kellyr <- rutils::lagit(kellyr, lagg=1)
# Scale the Kelly ratios so the portfolio has the target volatility
kellyr <- kellyr*volt/sqrt(rowSums((kellyr^2)*varm, na.rm=TRUE))
colnames(kellyr) <- colnames(reton)
# Scale the Kelly ratios so their sum of squares is equal to one
# kellyr <- kellyr/sqrt(rowSums(kellyr^2, na.rm=TRUE))

# Calculate the strategy PnLs as the product of the returns times the Kelly ratios
# pnls <- retstock*kellyr
pnls <- reton*kellyr
pnlsum <- colSums(pnls["2010/"], na.rm=TRUE)
# names(pnlsum) <- colnames(retstock)
pnlsum <- sort(pnlsum, decreasing=TRUE)
symbolv <- names(pnlsum)

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    ##  Input stock symboln
    column(width=1, selectInput("symboln", label="Symbol", choices=symbolv, selected=symboln)),
    ##  Input lambda decay parameter
    # column(width=2, sliderInput("lambdaf", label="Lambda", min=0.1, max=0.99, value=0.1, step=0.01)),
    # Input the bid-ask spread
    # column(width=2, numericInput("bidask", label="Bid-ask:", value=0.0005, step=0.0001)),
    # Input Kelly threshold
    # column(width=2, sliderInput("threshv", label="Threshold:", min=0.1, max=9.0, value=1.0, step=0.1)),
  ),  ##  end fluidRow

  ##  Render the plot in a new row
  fluidRow(
    dygraphs::dygraphOutput("dyplot", width="90%", height="600px")
  ),  ##  end fluidRow
  
)  ##  end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  ##  Create an empty list of reactive values.
  # values <- reactiveValues()

  ##  Plot the cumulative scaled returns
  ##  Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    ##  Get the symbol name and the pnls
    symboln <- input$symboln
    cat("Recalculating PnLs for", symboln, "\n")
    pnls <- 
    ##  Get Sharpe ratios
    # sharper <- values$sharper

    pnlv <- get(symboln, pnls)
    colnamev <- colnames(pnls)
    kellyv <- kellyr[, symboln]
    datav <- na.omit(cbind(pnlv, kellyv))
    colnamev <- c("PnL", "Kelly")
    colnames(datav) <- colnamev
    datav <- datav["1998/"]
    datav[, 1] <- cumsum(datav[, 1])
    
    ##  Create the dygraph object
    captiont <- paste0(symboln, " Momentum Strategy PnLs and Kelly Ratios")
    dyplot <- dygraphs::dygraph(datav, main=captiont) %>%
      dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
      dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
      dySeries(axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
      dySeries(axis="y2", label=colnamev[2], strokeWidth=1, col="red") %>%
      dyLegend(show="always", width=400)
    
    # dyplot <- dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
    #   dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
    #   dyLegend(show="always", width=200)
    
    ##  Plot the dygraph object
    return(dyplot)


  })  ##  end output plot

}  ##  end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
