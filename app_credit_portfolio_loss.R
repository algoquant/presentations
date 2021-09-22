##############################
# This is a shiny app for calculating the tail credit 
# portfolio losses under the Vasicek model.
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

portf_loss <- function(x, def_thresh=-2, rh_o=0.1, l_gd=0.4) {
  q_norm <- ifelse(x/l_gd < 0.999, qnorm(x/l_gd), 3.1)
  sqrt((1-rh_o)/rh_o)*exp(-(sqrt(1-rh_o)*q_norm - def_thresh)^2/(2*rh_o) + q_norm^2/2)/l_gd
}  # end portf_loss


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
    column(width=4, sliderInput("at_tach", label="Tranche attachment:",
                                min=0.0, max=0.5, value=0.15, step=0.01))
  ),  # end fluidRow
  
  # Render plot in panel
  mainPanel(plotOutput("plot_portf", width="150%", height=500))
)  # end fluidPage interface


##############################
## Define the server code
# The function ser_ver() accepts the arguments "input" and "output".

ser_ver <- function(input, output) {

  ## Recalculate the model with new parameters
  # The function reactive() accepts a block of expressions
  # which calculate the model, and returns the model output.
  da_ta <- reactive({
    
    # Extract model parameters from the argument "input"
    at_tach <- input$at_tach
    def_prob <- input$def_prob
    rh_o <- input$rh_o
    l_gd <- input$l_gd
    exp_loss <- l_gd*def_prob
    def_thresh <- qnorm(def_prob)
    
    # Calculate portfolio losses
    round(
      integrate(function(x) x*portf_loss(x, def_thresh=def_thresh, 
            rh_o=rh_o, l_gd=l_gd), low=at_tach, up=l_gd)$value, 
      digits=5)
  })  # end reactive code
  
  ## Create plot and return it to the output argument
  # The function reactive() accepts a block of expressions
  # which calculate the model, and returns the model output.
  output$plot_portf <- shiny::renderPlot({
    
    # Extract model parameters from the argument "input"
    at_tach <- input$at_tach
    def_prob <- input$def_prob
    rh_o <- input$rh_o
    l_gd <- input$l_gd
    exp_loss <- l_gd*def_prob
    def_thresh <- qnorm(def_prob)
    
    # Calculate max x-axis range
    x_max <- max(3*exp_loss, at_tach)
    # Calculate max density of portfolio losses (for y-axis scale)
    y_max <- max(sapply(seq(fr=0.01, to=l_gd/2, length.out=10), portf_loss, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd))
    
    # Plot density of portfolio losses
    curve(expr=portf_loss(x, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd),
          type="l", xlim=c(0, x_max), cex=1.0,
          xlab="loss percentage", ylab="density", lwd=3,
          col="orange", main="Distribution of Losses")
    # Add vertical line for expected loss
    abline(v=exp_loss, col="red", lwd=3)
    text(x=exp_loss-0.001, y=3*y_max/4, labels="expected loss",
         lwd=2, srt=90, pos=3)
    # Add vertical line for tranche attachment
    abline(v=at_tach, col="blue", lwd=3)
    text(x=at_tach-0.001, y=3*y_max/4, labels="tranche attachment",
         lwd=2, srt=90, pos=3)
    
    # Calculate tranche shading for CVaR
    va_r <- at_tach; var_max <- 0.99
    var_s <- seq(va_r, var_max, length=100)
    densi_ty <- sapply(var_s, portf_loss, def_thresh=def_thresh, rh_o=rh_o, l_gd=l_gd)
    # Draw shaded polygon
    polygon(c(va_r, var_s, var_max),
            c(-1, densi_ty, -1), col="red", border=NA, density=10)
    # text(x=0.045, y=0, labels="CVaR", lwd=2, pos=3)
    
    # Text with tranche attachment
    text(x_max-0.01, y_max, 
         lab=paste0(
           "Default probability = ", format(100*def_prob, digits=3), "%", "\n",
           "Loss severity = ", format(100*l_gd, digits=3), "%", "\n",
           "Correlation = ", format(100*rh_o, digits=3), "%", "\n",
           "Tranche attachment = ", format(100*at_tach, digits=3), "%", "\n",
           "Tranche loss = ", 100*da_ta(), "%"), 
         adj=c(1, 1), cex=1.2, lwd=2)
  })  # end output plot

}  # end server code


##############################
## Return a Shiny app object

shiny::shinyApp(ui=inter_face, server=ser_ver)
