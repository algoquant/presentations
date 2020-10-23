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
source(file="C:/Develop/R/scripts/technical_indicators.R")

indicator_s <- cbind(re_turns, z_scores, vol_at, sk_ew)

# End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
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
    column(width=4, sliderInput("beta_skew", label="skew beta:",
                                min=-20.0, max=20.0, value=-20.0, step=0.1)),
    # # input the momentum beta
    # column(width=4, sliderInput("beta_moment", label="momentum beta:",
    #                             min=-5.0, max=5.0, value=-5.0, step=0.1)),
    # # input the op_en-hi_gh beta
    # column(width=4, sliderInput("beta_ophi", label="op_en-hi_gh beta:",
    #                             min=-5.0, max=5.0, value=-5.0, step=0.1)),
    # # input the clos_e-hi_gh beta
    # column(width=4, sliderInput("beta_clhi", label="clos_e-hi_gh beta:",
    #                             min=-5.0, max=5.0, value=-5.0, step=0.1)),
    # input the look-back interval
    column(width=4, sliderInput("look_back", label="look-back:",
                                min=2, max=50, value=10, step=1))
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(dygraphOutput("dygraph"), width=12)
)  # end fluidPage interface


## Define the server code
ser_ver <- shiny::shinyServer(function(input, output) {

  ## Re-calculate the model with new parameters
  da_ta <- reactive({
    # get model parameters from input argument
    beta_ret <- input$beta_ret
    beta_zscore <- input$beta_zscore
    beta_vol <- input$beta_vol
    beta_skew <- input$beta_skew
    # beta_moment <- input$beta_moment
    # beta_ophi <- input$beta_ophi
    # beta_clhi <- input$beta_clhi
    look_back <- input$look_back
    weight_s <- c(beta_ret, beta_zscore, beta_vol, beta_skew)
    # weight_s <- c(beta_ret, beta_vol, beta_skew, beta_moment, beta_ophi, beta_clhi)
    
    ## Simulate strategy
    # calculate signal
    sig_nal <- indicator_s %*% weight_s
    # scale sig_nal using roll_scale()
    sig_nal <- roll::roll_scale(data=sig_nal, width=look_back, min_obs=1)
    sig_nal[1,] <- 0
    sig_nal <- rutils::lag_it(sig_nal)
    # calculate positions, either: -1, 0, or 1
    po_sit <- rep(NA_integer_, NROW(oh_lc))
    po_sit[1] <- 0
    po_sit[sig_nal<(-1.5)] <- 1
    po_sit[sig_nal>1.5] <- (-1)
    po_sit <- na.locf(po_sit)
    # po_sit <- xts(po_sit, order.by=index(oh_lc))
    pnl_s <- cumsum(po_sit*re_turns)
    colnames(pnl_s) <- "strategy"
    pnl_s
  })  # end reactive code
  
  # return the dygraph plot to output argument
  output$dygraph <- renderDygraph({
    dygraphs::dygraph(cbind(clos_e, da_ta()), main="OHLC Technicals Strategy") %>%
      dyAxis("y", label="VTI", independentTicks=TRUE) %>%
      dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
      dySeries("strategy", axis="y2", col=c("blue", "red"))
  })  # end output plot
  
})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
