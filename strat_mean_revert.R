##############################
# This is a shiny app for simulating a mean reverting 
# strategy using static betas times OHLC technical indicators, 
# with dygraphs plot.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started


# load packages
library(shiny)
library(dygraphs)
library(rutils)

# Calculate indicator_s matrix of OHLC technical indicators
source(file="/Users/jerzy/Develop/R/scripts/load_technical_indicators.R")

indicator_s <- cbind(retv, zscores, volat, skew)

# End setup code


## Create elements of the user interface
uifun <- shiny::fluidPage(
  titlePanel("Strategy with Static Betas Times OHLC Technical Indicators"),
  
  # create single row with four slider inputs
  fluidRow(
    # input the returns beta
    column(width=4, sliderInput("beta_ret", label="returns beta:",
                                min=-20.0, max=20.0, value=0.0, step=0.1)),
    # input the zscore beta
    column(width=4, sliderInput("beta_zscore", label="zscore beta:",
                                min=-20.0, max=20.0, value=0.0, step=0.1)),
    # input the vol beta
    column(width=4, sliderInput("beta_vol", label="vol beta:",
                                min=-20.0, max=20.0, value=20.0, step=0.1)),
    # input the skew beta
    column(width=4, sliderInput("betaskew", label="skew beta:",
                                min=-20.0, max=20.0, value=-20.0, step=0.1)),
    # # input the momentum beta
    # column(width=4, sliderInput("betamoment", label="momentum beta:",
    #                             min=-5.0, max=5.0, value=-5.0, step=0.1)),
    # # input the openp-highp beta
    # column(width=4, sliderInput("beta_ophi", label="openp-highp beta:",
    #                             min=-5.0, max=5.0, value=-5.0, step=0.1)),
    # # input the closep-highp beta
    # column(width=4, sliderInput("beta_clhi", label="closep-highp beta:",
    #                             min=-5.0, max=5.0, value=-5.0, step=0.1)),
    # input the look-back interval
    column(width=4, sliderInput("lookb", label="look-back:",
                                min=2, max=50, value=10, step=1))
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(dygraphs::dygraphOutput("dygraph"), width=12)
)  # end fluidPage interface


## Define the server code
servfun <- function(input, output) {

  ## Recalculate the model with new parameters
  datav <- shiny::reactive({
    # get model parameters from input argument
    beta_ret <- input$beta_ret
    beta_zscore <- input$beta_zscore
    beta_vol <- input$beta_vol
    betaskew <- input$betaskew
    # betamoment <- input$betamoment
    # beta_ophi <- input$beta_ophi
    # beta_clhi <- input$beta_clhi
    lookb <- input$lookb
    weights <- c(beta_ret, beta_zscore, beta_vol, betaskew)
    # weights <- c(beta_ret, beta_vol, betaskew, betamoment, beta_ophi, beta_clhi)
    
    ## Simulate strategy
    # calculate signal
    score <- indicator_s %*% weights
    # scale score using roll_scale()
    score <- roll::roll_scale(score, width=lookb, min_obs=1)
    score[1,] <- 0
    score <- rutils::lagit(score)
    # calculate positions, either: -1, 0, or 1
    posv <- rep(NA_integer_, NROW(ohlc))
    posv[1] <- 0
    posv[score<(-1.5)] <- 1
    posv[score>1.5] <- (-1)
    posv <- na.locf(posv)
    # posv <- xts(posv, order.by=index(ohlc))
    pnls <- cumsum(posv*retv)
    colnames(pnls) <- "strategy"
    pnls
  })  # end reactive code
  
  # return the dygraph plot to output argument
  output$dygraph <- dygraphs::renderDygraph({
    dygraphs::dygraph(cbind(closep, datav()), main="OHLC Technicals Strategy") %>%
      dyAxis("y", label="VTI", independentTicks=TRUE) %>%
      dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
      dySeries("strategy", axis="y2", col="red")
  })  # end output plot
  
}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=uifun, server=servfun)
