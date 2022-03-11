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
nassets <- 100; nsimu <- 1000
set.seed(1121)
# Simulate vector of systematic factors
sysv <- rnorm(nsimu)
# Simulate matrix of idiosyncratic factors
idio_sync <- matrix(rnorm(nsimu*nassets), ncol=nsimu)



# End setup code
##############################


##############################
## Define the user interface

uiface <- shiny::fluidPage(
  titlePanel("Portfolio Loss Distribution"),
  
  # Create four slider inputs with parameters to portf_loss()
  fluidRow(
    column(width=4, sliderInput("rho", label="Correlation:",
                                min=0.0, max=0.9, value=0.2, step=0.01)),
    column(width=4, sliderInput("def_prob", label="Default probability:",
                                min=0.0, max=0.9, value=0.2, step=0.01)),
    column(width=4, sliderInput("lgd", label="Loss severity:",
                                min=0.0, max=0.9, value=0.4, step=0.01)),
    column(width=4, sliderInput("confl", label="Confidence level:",
                                min=0.9, max=0.99, value=0.95, step=0.01))
  ),  # end fluidRow
  
  # Render plot in panel
  mainPanel(plotOutput("plot_portf", width="150%", height=500))
)  # end fluidPage interface


##############################
## Define the server code
# The function servfunc() accepts the arguments "input" and "output".

servfunc <- function(input, output) {
  
  ## The function reactive() accepts a block of expressions
  # which calculate the model, and returns the model output.
  
  ## Calculate the loss distribution with new parameters
  losses <- reactive({
    cat("Calculating the loss distribution\n")
    
    # Extract model parameters from the argument "input"
    def_prob <- input$def_prob
    rho <- input$rho
    lgd <- input$lgd
    
    # Calculate default probability and threshold
    # exp_loss <- lgd*def_prob
    def_thresh <- qnorm(def_prob)
    
    # Define correlation parameters
    rho_sqrt <- sqrt(rho); rho_sqrtm <- sqrt(1-rho)
    
    # Calculate the portfolio losses
    assets <- t(rho_sqrt*sysv + t(rho_sqrtm*idio_sync))
    losses <- lgd*colSums(assets < def_thresh)/nassets
    
    # Return the losses
    losses

  })  # end reactive code
  
  
  ## Calculate the VaR and CVaR
  tai_l <- reactive({
    cat("Calculating the VaR and CVaR\n")
    
    # Extract model parameters from the argument "input"
    confl <- input$confl
    
    losses <- losses()
    # Calculate VaR from confidence level
    va_r <- quantile(losses, confl)
    # Calculate the CVaR as the mean losses in excess of VaR
    c_var <- mean(losses[losses > va_r])
    
    # Return the VaR and CVaR
    c(va_r, c_var)
    
  })  # end reactive code


## Create plot and return it to the output argument
  output$plot_portf <- shiny::renderPlot({
    
    # Extract model parameters from the argument "input"
    confl <- input$confl
    def_prob <- input$def_prob
    rho <- input$rho
    lgd <- input$lgd
    exp_loss <- lgd*def_prob
    # def_thresh <- qnorm(def_prob)
    
    # Extract the loss distribution
    losses <- losses()
    densv <- density(losses)
    x_max <- max(densv$x)
    y_max <- max(densv$y)
    
    # Extract the VaR and CVaR
    va_r <- tai_l()[1]
    c_var <- tai_l()[2]
    
    # Plot density of portfolio losses
    plot(densv, col="blue", lwd=3, main="Portfolio Loss Distribution")
    # Add vertical line for expected loss
    abline(v=exp_loss, col="orange", lwd=4)
    text(x=exp_loss, y=6*y_max/7, labels="expected loss", lwd=2, pos=4, cex=1.2)
    # Add vertical line for VaR
    abline(v=va_r, col="red", lwd=4)
    text(x=va_r, y=4*y_max/5, labels="VaR", lwd=2, pos=4, cex=1.2)
    
    # Draw shaded polygon for CVaR
    indeks <- (densv$x > va_r)
    x_var <- c(min(densv$x[indeks]), densv$x[indeks], max(densv$x))
    polygon(x_var, c(-1, densv$y[indeks], -1), 
            col="red", border=NA, density=10)
    # Add text for CVaR
    text(x=5*va_r/4, y=(y_max/7), labels="CVaR", lwd=2, pos=4)
    
    # Text with CVaR attachment
    text(x_max-0.01, y_max, 
         lab=paste0(
           "Default probability = ", format(100*def_prob, digits=3), "%", "\n",
           "Loss severity = ", format(100*lgd, digits=3), "%", "\n",
           "Correlation = ", format(100*rho, digits=3), "%", "\n",
           "VaR = ", format(100*va_r, digits=3), "%", "\n",
           "CVaR = ", format(100*c_var, digits=3), "%"), 
         adj=c(1, 1), cex=1.0, lwd=2)
    
  })  # end output plot
  
}  # end server code


##############################
## Return a Shiny app object

shiny::shinyApp(ui=uiface, server=servfunc)

