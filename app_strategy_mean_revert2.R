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

vol_at[which.max(vol_at)] <- 0
vol_at[which.max(vol_at)] <- 0
# date_s <- xts::.index(oh_lc)
# de_sign <- matrix(as.numeric(xts::.index(oh_lc)), nc=1)
# indicator_s <- cbind(re_turns, z_scores, vol_at, sk_ew)

oh_lc <- log(HighFreq::SPY["2010"])
clo_se <- Cl(oh_lc)
re_turns <- rutils::diff_it(clo_se)
colnames(re_turns) <- "returns"
date_s <- 1:NROW(oh_lc)
# date_s <- xts::.index(oh_lc)
de_sign <- matrix(as.numeric(date_s), nc=1)

# End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("Strategy with Static Betas Times OHLC Technical Indicators"),
  
  # create single row with four slider inputs
  fluidRow(
    # input the look-back interval
    # column(width=4, sliderInput("look_long", label="look-back long:",
    #                             min=3, max=20, value=10, step=1)),
    # input the look-back interval
    column(width=4, sliderInput("look_short", label="look-back short:",
                                min=5, max=190, value=20, step=1)),
    # input the returns beta
    # column(width=4, sliderInput("beta_ret", label="returns beta:",
    #                             min=-20.0, max=20.0, value=0.0, step=0.1)),
    # input the trade entry level
    column(width=4, sliderInput("en_ter", label="trade enter level:",
                                min=0.1, max=9.0, value=3.5, step=0.1)),
    # input the trade exit level
    column(width=4, sliderInput("ex_it", label="trade exit level:",
                                min=0.1, max=3.0, value=1.0, step=0.1))
    # input the vol beta
    # column(width=4, sliderInput("beta_vol", label="vol beta:",
    #                             min=0.01, max=1.0, value=0.5, step=0.01)),
    # input the skew beta
    # column(width=4, sliderInput("beta_skew", label="skew beta:",
    #                             min=-20.0, max=20.0, value=-20.0, step=0.1)),
    # # input the momentum beta
    # column(width=4, sliderInput("beta_moment", label="momentum beta:",
    #                             min=-5.0, max=5.0, value=-5.0, step=0.1)),
    # # input the op_en-hi_gh beta
    # column(width=4, sliderInput("beta_ophi", label="op_en-hi_gh beta:",
    #                             min=-5.0, max=5.0, value=-5.0, step=0.1)),
    # # input the clo_se-hi_gh beta
    # column(width=4, sliderInput("beta_clhi", label="clo_se-hi_gh beta:",
    #                             min=-5.0, max=5.0, value=-5.0, step=0.1)),
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(dygraphOutput("dygraph"), width=12)
)  # end fluidPage interface


## Define the server code
ser_ver <- shiny::shinyServer(function(input, output) {

  ## Re-calculate the model with new parameters
  da_ta <- reactive({
    # get model parameters from input argument
    # beta_ret <- input$beta_ret
    en_ter <- input$en_ter
    ex_it <- input$ex_it
    # beta_vol <- input$beta_vol
    # beta_skew <- input$beta_skew
    # beta_moment <- input$beta_moment
    # beta_ophi <- input$beta_ophi
    # beta_clhi <- input$beta_clhi
    # look_long <- input$look_long
    look_short <- input$look_short
    # weight_s <- c(beta_ret, en_ter, beta_vol, beta_skew)
    # weight_s <- c(beta_ret, beta_vol, beta_skew, beta_moment, beta_ophi, beta_clhi)
    
    ## Simulate strategy
    # calculate signal
    # sig_nal <- clo_se
    # mean reverting signal
    sig_nal <- HighFreq::roll_zscores(res_ponse=as.numeric(clo_se), 
                            de_sign=de_sign, 
                            look_back=look_short)
    sig_nal[1:look_short, ] <- 0
    # scale sig_nal using HighFreq::roll_scale()
    # sig_nal <- roll::roll_scale(data=sig_nal, width=look_short, min_obs=1)
    # sig_nal <- HighFreq::roll_scale(mat_rix=sig_nal, look_back=look_short, use_median=TRUE)
    sig_nal[1:look_short, ] <- 0
    sig_nal[is.infinite(sig_nal), ] <- 0
    sig_nal <- rutils::lag_it(sig_nal, lagg=1)
    # calculate positions, either: -1, 0, or 1
    # po_sit <- -sign(sig_nal)
    # calculate positions, either: -1, 0, or 1
    po_sit <- rep(NA_integer_, NROW(oh_lc))
    po_sit[1] <- 0
    po_sit[sig_nal < (-en_ter)] <- 1
    po_sit[sig_nal > en_ter] <- (-1)
    po_sit[abs(sig_nal) < ex_it] <- 0
    po_sit <- na.locf(po_sit)
    po_sit <- po_sit + rutils::lag_it(po_sit, lagg=1)
    
    # trending signal
    # sig_nal <- HighFreq::roll_zscores(res_ponse=clo_se, 
    #                         de_sign=de_sign, 
    #                         look_back=look_long)
    # sig_nal[1:look_long, ] <- 0
    # sig_nal <- rutils::lag_it(sig_nal)
    # calculate positions, either: -1, 0, or 1
    # po_sit <- po_sit + sign(sig_nal)
    # po_sit <- rep(NA_integer_, NROW(oh_lc))
    # po_sit[1] <- 0
    # po_sit[sig_nal<beta_vol] <- (-1)
    # po_sit[sig_nal>beta_vol] <- 1
    # po_sit <- na.locf(po_sit)
    # po_sit <- xts(po_sit, order.by=index(oh_lc))
    pnl_s <- cumsum(po_sit*re_turns)
    colnames(pnl_s) <- "strategy"
    # po_sit <- xts(po_sit, index(oh_lc))
    # colnames(sig_nal) <- "strategy"
    pnl_s
  })  # end reactive code
  
  # return the dygraph plot to output argument
  output$dygraph <- renderDygraph({
    dygraphs::dygraph(cbind(clo_se, da_ta())[endpoints(clo_se, on="days")], main="OHLC Technicals Strategy") %>%
      dyAxis("y", label="VTI", independentTicks=TRUE) %>%
      dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
      dySeries("strategy", axis="y2", col=c("blue", "red"))
  })  # end output plot
  
})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
