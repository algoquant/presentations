##############################
# This is a shiny app for simulating a momentum strategy 
# for overnight ETF returns.
#
# Just press the "Run App" button on upper right of this panel.
##############################


## Model and data setup
## Below is the setup code that runs once when the shiny app is started

# Load R packages
library(HighFreq)
library(shiny)
library(dygraphs)

# Load the overnight returns for ETFs
if (!exists("reton")) {
  cat("Loading the ETF overnight returns.\n")
  load("/Users/jerzy/Develop/lecture_slides/data/etf_returns_overnight.RData")
} # end if
# symbolv <- sort(names(sp500env))


# Select the sector ETFs starting with X
symbolv <- rutils::etfenv$symbolv
symbolv <- symbolv[grep("^X", symbolv)]
symbolv <- c("SPY", symbolv)
nstocks <- NROW(symbolv)
reton <- na.omit(reton[, symbolv])
# reton <- reton["2006/"]

# Calculate the index returns as the average of the overnight returns of all stocks
reton[1:3, ] <- 0
datev <- index(reton)
nstocks <- NCOL(reton)
retm <- rowMeans(reton, na.rm=TRUE)
retm <- xts::xts(retm, order.by=datev)
symboln <- "Index"
volt <- 0.03 ##  Volatility target for scaling the strategy PnLs

stringv <- "Overnight"
captiont <- paste("Momentum With Overnight ETF Returns")

## End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel(captiont),

  fluidRow(
    ##  Input stock symboln
    # column(width=1, selectInput("symboln", label="Symbol", choices=symbolv, selected=symboln)),
    ##  Input lambda decay parameter
    column(width=2, sliderInput("lambdaf", label="Lambda", min=0.1, max=0.99, value=0.99, step=0.01)),
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

  ##  Load the OHLC prices
  # ohlc <- shiny::reactive({
  #   
  #   symboln <- input$symboln
  #   cat("Loading data for", symboln, "\n")
  # 
  #   ohlc <- log(get(symboln, sp500env))
  #   return(ohlc)
  #   
  # })  ##  end Load the data
  
  ##  Calculate the returns
  # retp <- shiny::reactive({
  #   
  #   # symboln <- input$symboln
  #   cat("Recalculating returns\n")
  #   
  #   ohlc <- ohlc()
  #   openp <- quantmod::Op(ohlc)
  #   closep <- quantmod::Cl(ohlc)
  #   retp <- rutils::diffit(closep) ##  daily returns
  #   colnames(retp) <- "daily"
  #   retd <- (closep - openp) ##  daytime returns
  #   colnames(retd) <- "daytime"
  #   reton <- (openp - rutils::lagit(closep, lagg=1)) ##  overnight returns
  #   colnames(reton) <- "overnight"
  #   reton[1] <- 0
  #   highp <- quantmod::Hi(ohlc)
  #   lowp <- quantmod::Lo(ohlc)
  #   hilo <- (highp - lowp) ##  range of daily prices
  #   
  #   return(reton)
  #   
  # })  ##  end Calculate the returns
  
  ##  Recalculate the strategy
  pnls <- shiny::reactive({
    
    # symboln <- input$symboln
    # cat("Recalculating PnLs for", symboln, "\n")

    lambdaf <- input$lambdaf
    
    ##  Calculate the range variance estimator
    # ohlc <- ohlc()
    varm <- HighFreq::run_var(reton, lambda=lambdaf)
    retsm <- varm[, 1:nstocks]
    varm <- varm[, (nstocks+1):(2*nstocks)]
    varm[1:3] <- 1.0
    kellyr <- retsm*volt^2/varm
    kellyr <- rutils::lagit(kellyr, lagg=1)
    # No shorting, so set negative Kelly ratios to zero
    # kellyr[kellyr < 0] <- 0
    
    pnls <- reton*kellyr
    pnls <- rowMeans(pnls, na.rm=TRUE)
    pnls <- pnls*sd(retm[retm<0])/sd(pnls[pnls<0])

    ##  Bind together strategy pnls
    pnls <- cbind(retm, pnls)
    colnames(pnls) <- c(stringv, "Strategy")
    
    ##  Calculate Sharpe ratios
    sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
    values$sharper <- round(sharper, 3)
    
    return(cumsum(pnls))

  })  ##  end Recalculate the strategy
  

  ##  Plot the cumulative scaled returns
  ##  Return to the output argument a dygraph plot with two y-axes
  output$dyplot <- dygraphs::renderDygraph({
    
    ##  Get the pnls
    pnls <- pnls()
    colnamev <- colnames(pnls)
    
    ##  Get Sharpe ratios
    sharper <- values$sharper

    ##  Standard plot without shading
    captiont <- paste0(c(paste0(stringv, " SR="), "Timing SR="), sharper, collapse=" / ")
    ##  Plot dygraph without shading
    dyplot <- dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
      dyLegend(show="always", width=200)
    
    ##  Plot the dygraph object
    return(dyplot)


  })  ##  end output plot

}  ##  end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
