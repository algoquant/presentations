##############################
# This is a shiny app for simulating the loss distribution of 
# a portfolio of credit assets, using Monte Carlo simulation.
# 
# Just press the "Run App" button on upper right of this panel.
##############################

##############################
# Below is the setup code that runs only once at startup 
# when the shiny app is started.
# In the setup code you can load packages, define functions 
# and variables, source files, and load data.

# Load packages here (if needed)


# Define portfolio parameters
n_assets <- 100; n_simu <- 1000
set.seed(1121)
# Simulate vector of systematic factors
system_atic <- rnorm(n_simu)
# Simulate matrix of idiosyncratic factors
idio_sync <- matrix(rnorm(n_simu*n_assets), ncol=n_simu)



# End setup code
##############################


##############################
## Define the user interface

inter_face <- shiny::fluidPage(
  titlePanel("Portfolio Loss Distribution"),
  
  # Create four slider inputs with parameters to portf_loss()
  fluidRow(
    column(width=4, sliderInput("rh_o", label="Correlation:",
                                min=0.0, max=0.9, value=0.2, step=0.01)),
    column(width=4, sliderInput("def_prob", label="Default probability:",
                                min=0.0, max=0.9, value=0.2, step=0.01)),
    column(width=4, sliderInput("l_gd", label="Loss severity:",
                                min=0.0, max=0.9, value=0.4, step=0.01)),
    column(width=4, sliderInput("conf_level", label="Confidence level:",
                                min=0.9, max=0.99, value=0.95, step=0.01))
  ),  # end fluidRow
  
  # Render plot in panel
  mainPanel(plotOutput("plot_portf", width="150%", height=500))
)  # end fluidPage interface


##############################
## Define the server code
# The function ser_ver() accepts the arguments "input" and "output".

ser_ver <- function(input, output) {
  
  ## The function reactive() accepts a block of expressions
  # which calculate the model, and returns the model output.
  
  ## Calculate the loss distribution with new parameters
  loss_es <- reactive({
    cat("Calculating the loss distribution\n")
    
    # Extract model parameters from the argument "input"
    def_prob <- input$def_prob
    rh_o <- input$rh_o
    l_gd <- input$l_gd
    
    # Calculate default probability and threshold
    # exp_loss <- l_gd*def_prob
    def_thresh <- qnorm(def_prob)
    
    # Define correlation parameters
    rho_sqrt <- sqrt(rh_o); rho_sqrtm <- sqrt(1-rh_o)
    
    # Calculate the portfolio losses
    asset_s <- t(rho_sqrt*system_atic + t(rho_sqrtm*idio_sync))
    loss_es <- l_gd*colSums(asset_s < def_thresh)/n_assets
    
    # Return the loss_es
    loss_es

  })  # end reactive code
  
  
  ## Calculate the VaR and CVaR
  tai_l <- reactive({
    cat("Calculating the VaR and CVaR\n")
    
    # Extract model parameters from the argument "input"
    conf_level <- input$conf_level
    
    loss_es <- loss_es()
    # Calculate VaR from confidence level
    va_r <- quantile(loss_es, conf_level)
    # Calculate the CVaR as the mean losses in excess of VaR
    c_var <- mean(loss_es[loss_es > va_r])
    
    # Return the VaR and CVaR
    c(va_r, c_var)
    
  })  # end reactive code


## Create plot and return it to the output argument
  output$plot_portf <- shiny::renderPlot({
    
    # Extract model parameters from the argument "input"
    conf_level <- input$conf_level
    def_prob <- input$def_prob
    rh_o <- input$rh_o
    l_gd <- input$l_gd
    exp_loss <- l_gd*def_prob
    # def_thresh <- qnorm(def_prob)
    
    # Extract the loss distribution
    loss_es <- loss_es()
    densi_ty <- density(loss_es)
    x_max <- max(densi_ty$x)
    y_max <- max(densi_ty$y)
    
    # Extract the VaR and CVaR
    va_r <- tai_l()[1]
    c_var <- tai_l()[2]
    
    # Plot density of portfolio losses
    plot(densi_ty, col="blue", lwd=3, main="Portfolio Loss Distribution")
    # Add vertical line for expected loss
    abline(v=exp_loss, col="orange", lwd=4)
    text(x=exp_loss, y=6*y_max/7, labels="expected loss", lwd=2, pos=4, cex=1.2)
    # Add vertical line for VaR
    abline(v=va_r, col="red", lwd=4)
    text(x=va_r, y=4*y_max/5, labels="VaR", lwd=2, pos=4, cex=1.2)
    
    # Draw shaded polygon for CVaR
    in_dex <- (densi_ty$x > va_r)
    x_var <- c(min(densi_ty$x[in_dex]), densi_ty$x[in_dex], max(densi_ty$x))
    polygon(x_var, c(-1, densi_ty$y[in_dex], -1), 
            col="red", border=NA, density=10)
    # Add text for CVaR
    text(x=5*va_r/4, y=(y_max/7), labels="CVaR", lwd=2, pos=4)
    
    # Text with CVaR attachment
    text(x_max-0.01, y_max, 
         lab=paste0(
           "Default probability = ", format(100*def_prob, digits=3), "%", "\n",
           "Loss severity = ", format(100*l_gd, digits=3), "%", "\n",
           "Correlation = ", format(100*rh_o, digits=3), "%", "\n",
           "VaR = ", format(100*va_r, digits=3), "%", "\n",
           "CVaR = ", format(100*c_var, digits=3), "%"), 
         adj=c(1, 1), cex=1.0, lwd=2)
    
  })  # end output plot
  
}  # end server code


##############################
## Return a Shiny app object

shiny::shinyApp(ui=inter_face, server=ser_ver)

