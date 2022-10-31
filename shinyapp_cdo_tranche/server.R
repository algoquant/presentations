##############################
# Below is the setup code that runs only once at startup 
# when the shiny app is started.
# In the setup code you can load packages, define functions 
# and variables, source files, and load data.

# Load packages here (if needed)

library(shiny)


# Define Vasicek loss distribution density function 
# (vectorized version with error handling for x)

portf_loss <- function(x, def_thresh=-2, rho=0.1, lgd=0.4) {
  q_norm <- ifelse(x/lgd < 0.999, qnorm(x/lgd), 3.1)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*q_norm - def_thresh)^2/(2*rho) + q_norm^2/2)/lgd
}  # end portf_loss

# Define cumulative default probability function
cum_loss <- function(x, def_thresh=(-2), rho=0.2, lgd=0.4)
  pnorm((sqrt(1-rho)*qnorm(x/lgd) - def_thresh)/sqrt(rho))

# Define server logic required to draw a histogram
function(input, output) {
  
## Re-calculate the model with new parameters
# The function reactive() accepts a block of expressions
# which calculate the model, and returns the model output.
  da_ta <- reactive({
    
    # Extract model parameters from the argument "input"
    attachp <- input$attachp
    detachp <- input$detachp
    defprob <- input$defprob
    rho <- input$rho
    lgd <- input$lgd
    exp_loss <- lgd*defprob
    def_thresh <- qnorm(defprob)
    
    # Calculate tranche losses
    round(
      integrate(function(x, attachp) (x-attachp)*portf_loss(x, 
                                                            def_thresh=def_thresh, rho=rho, lgd=lgd), 
                low=attachp, up=detachp, attachp=attachp)$value / (detachp-attachp) + 
        (1-cum_loss(x=detachp, def_thresh=def_thresh, rho=rho, lgd=lgd)), 
      digits=5)
  })  # end reactive code
  
  ## Create plot and return it to the output argument
  # The function reactive() accepts a block of expressions
  # which calculate the model, and returns the model output.
  output$plot_portf <- renderPlot({
    
    # Extract model parameters from the argument "input"
    attachp <- input$attachp
    detachp <- input$detachp
    defprob <- input$defprob
    rho <- input$rho
    lgd <- input$lgd
    exp_loss <- lgd*defprob
    def_thresh <- qnorm(defprob)
    
    # Calculate max x-axis range
    x_max <- max(3*exp_loss, detachp)
    # Calculate max density of portfolio losses (for y-axis scale)
    y_max <- max(sapply(seq(fr=0.01, to=lgd/2, length.out=10), portf_loss, def_thresh=def_thresh, rho=rho, lgd=lgd))
    
    # Plot density of portfolio losses
    curve(expr=portf_loss(x, def_thresh=def_thresh, rho=rho, lgd=lgd),
          type="l", xlim=c(0, x_max), 
          xlab="loss percentage", ylab="density", lwd=3,
          col="orange", main="CDO Tranche Losses")
    # Add vertical line for expected loss
    abline(v=exp_loss, col="red", lwd=3)
    text(x=exp_loss-0.001, y=3*y_max/4, labels="expected loss",
         lwd=2, srt=90, pos=3)
    # Add vertical line for tranche attachment
    abline(v=attachp, col="blue", lwd=3)
    text(x=attachp-0.001, y=3*y_max/4, labels="tranche attachment",
         lwd=2, srt=90, pos=3)
    # Add vertical line for tranche detachment
    abline(v=detachp, col="blue", lwd=3)
    text(x=detachp-0.001, y=3*y_max/4, labels="tranche detachment",
         lwd=2, srt=90, pos=3)
    
    # Calculate tranche shading for CVaR
    va_r <- attachp; var_max <- detachp
    var_s <- seq(va_r, var_max, length=100)
    dens_ity <- sapply(var_s, portf_loss, def_thresh=def_thresh, rho=rho, lgd=lgd)
    # Draw shaded polygon
    polygon(c(va_r, var_s, var_max),
            c(-1, dens_ity, -1), col="red", border=NA, density=10)
    # text(x=0.045, y=0, labels="CVaR", lwd=2, pos=3)
    
    # Text with tranche attachment
    text(x_max-0.01, y_max, 
         lab=paste0(
           "Default probability = ", format(100*defprob, digits=3), "%", "\n",
           "Loss severity = ", format(100*lgd, digits=3), "%", "\n",
           "Correlation = ", format(100*rho, digits=3), "%", "\n",
           "Tranche attachment = ", format(100*attachp, digits=3), "%", "\n",
           "Tranche detachment = ", format(100*detachp, digits=3), "%", "\n",
           "Tranche loss = ", 100*da_ta(), "%"), 
         adj=c(1, 1), cex=1.2, lwd=2)
  })  # end output plot
  
}  # end server function
