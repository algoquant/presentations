##############################
# This is a shiny app for calculating CDO tranche losses 
# under the Vasicek model.
# 
# Just press the "Run App" button on upper right of this panel.
##############################


##############################
# Below is the setup code that runs only once at startup 
# when the shiny app is started.
# In the setup code you can load packages, define functions 
# and variables, source files, and load data.

# Load packages here (if needed)


# Define Vasicek loss distribution density function 
# (vectorized version with error handling for x)

portf_loss <- function(x, def_thresh=-2, rho=0.1, lgd=0.4) {
  q_norm <- ifelse(x/lgd < 0.999, qnorm(x/lgd), 3.1)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*q_norm - def_thresh)^2/(2*rho) + q_norm^2/2)/lgd
}  # end portf_loss

# Define cumulative default probability function
cum_loss <- function(x, def_thresh=(-2), rho=0.2, lgd=0.4)
  pnorm((sqrt(1-rho)*qnorm(x/lgd) - def_thresh)/sqrt(rho))


# End setup code
##############################


##############################
## Define the user interface

uiface <- shiny::fluidPage(
  titlePanel("CDO Tranche Losses"),
  
  # Create four slider inputs with parameters to portf_loss()
  fluidRow(
    column(width=4, sliderInput("rho", label="Correlation:",
                                min=0.0, max=0.9, value=0.2, step=0.01)),
    column(width=4, sliderInput("def_prob", label="Default probability:",
                                min=0.0, max=0.9, value=0.2, step=0.01)),
    column(width=4, sliderInput("lgd", label="Loss severity:",
                                min=0.0, max=0.9, value=0.4, step=0.01)),
    column(width=4, sliderInput("at_tach", label="Tranche attachment:",
                                min=0.0, max=0.5, value=0.15, step=0.01)),
    column(width=4, sliderInput("de_tach", label="Tranche detachment:",
                                min=0.0, max=0.5, value=0.2, step=0.01))
  ),  # end fluidRow
  
  # Render plot in panel
  mainPanel(plotOutput("plot_portf", width="150%", height=500))
)  # end fluidPage interface


##############################
## Define the server code
# The function servfunc() accepts the arguments "input" and "output".

servfunc <- function(input, output) {

  ## Recalculate the model with new parameters
  # The function reactive() accepts a block of expressions
  # which calculate the model, and returns the model output.
  datav <- reactive({
    
    # Extract model parameters from the argument "input"
    at_tach <- input$at_tach
    de_tach <- input$de_tach
    def_prob <- input$def_prob
    rho <- input$rho
    lgd <- input$lgd
    exp_loss <- lgd*def_prob
    def_thresh <- qnorm(def_prob)
    
    # Calculate tranche losses
    round(
      integrate(function(x, at_tach) (x-at_tach)*portf_loss(x, 
                def_thresh=def_thresh, rho=rho, lgd=lgd), 
                low=at_tach, up=de_tach, at_tach=at_tach)$value / (de_tach-at_tach) + 
        (1-cum_loss(x=de_tach, def_thresh=def_thresh, rho=rho, lgd=lgd)), 
      digits=5)
  })  # end reactive code
  
  ## Create plot and return it to the output argument
  # The function reactive() accepts a block of expressions
  # which calculate the model, and returns the model output.
  output$plot_portf <- shiny::renderPlot({
    
    # Extract model parameters from the argument "input"
    at_tach <- input$at_tach
    de_tach <- input$de_tach
    def_prob <- input$def_prob
    rho <- input$rho
    lgd <- input$lgd
    exp_loss <- lgd*def_prob
    def_thresh <- qnorm(def_prob)
    
    # Calculate max x-axis range
    x_max <- max(3*exp_loss, de_tach)
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
    abline(v=at_tach, col="blue", lwd=3)
    text(x=at_tach-0.001, y=3*y_max/4, labels="tranche attachment",
         lwd=2, srt=90, pos=3)
    # Add vertical line for tranche detachment
    abline(v=de_tach, col="blue", lwd=3)
    text(x=de_tach-0.001, y=3*y_max/4, labels="tranche detachment",
         lwd=2, srt=90, pos=3)
    
    # Calculate tranche shading for CVaR
    va_r <- at_tach; var_max <- de_tach
    var_s <- seq(va_r, var_max, length=100)
    densv <- sapply(var_s, portf_loss, def_thresh=def_thresh, rho=rho, lgd=lgd)
    # Draw shaded polygon
    polygon(c(va_r, var_s, var_max),
            c(-1, densv, -1), col="red", border=NA, density=10)
    # text(x=0.045, y=0, labels="CVaR", lwd=2, pos=3)
    
    # Text with tranche attachment
    text(x_max-0.01, y_max, 
         lab=paste0(
           "Default probability = ", format(100*def_prob, digits=3), "%", "\n",
           "Loss severity = ", format(100*lgd, digits=3), "%", "\n",
           "Correlation = ", format(100*rho, digits=3), "%", "\n",
           "Tranche attachment = ", format(100*at_tach, digits=3), "%", "\n",
           "Tranche detachment = ", format(100*de_tach, digits=3), "%", "\n",
           "Tranche loss = ", 100*datav(), "%"), 
         adj=c(1, 1), cex=1.2, lwd=2)
  })  # end output plot

}  # end server function


##############################
## Return a Shiny app object

shiny::shinyApp(ui=uiface, server=servfunc)
