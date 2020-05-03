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
    column(width=4, sliderInput("at_tach", label="CVaR attachment:",
                                min=0.0, max=0.5, value=0.15, step=0.01))
  ),  # end fluidRow
  
  # Render plot in panel
  mainPanel(plotOutput("plot_portf", width="150%", height=500))
)  # end fluidPage interface


##############################
## Define the server code
# The function ser_ver() accepts the arguments "input" and "output".

ser_ver <- function(input, output) {
  
  ## Re-calculate the model with new parameters
  # The function reactive() accepts a block of expressions
  # which calculate the model, and returns the model output.
  da_ta <- reactive({
    
    # Extract model parameters from the argument "input"
    at_tach <- input$at_tach
    def_prob <- input$def_prob
    rh_o <- input$rh_o
    l_gd <- input$l_gd
    
    # Calculate default probabilities and thresholds
    exp_loss <- l_gd*def_prob
    def_thresh <- qnorm(def_prob)
    
    # Define correlation parameters
    rho_sqrt <- sqrt(rh_o); rho_sqrtm <- sqrt(1-rh_o)
    
    # Calculate the portfolio losses
    asset_s <- t(rho_sqrt*system_atic + t(rho_sqrtm*idio_sync))
    loss_es <- l_gd*colSums(asset_s < def_thresh)/n_assets
    # Calculate the CVaR as a percentage of total portfolio losses
    c_var <- sum(loss_es[loss_es > at_tach])/sum(loss_es)
    
    # Return a named list
    list(losses=loss_es, c_var=c_var)
  })  # end reactive code
  
  ## Create plot and return it to the output argument
  # The function reactive() accepts a block of expressions
  # which calculate the model, and returns the model output.
  output$plot_portf <- renderPlot({
    
    # Extract model parameters from the argument "input"
    at_tach <- input$at_tach
    def_prob <- input$def_prob
    rh_o <- input$rh_o
    l_gd <- input$l_gd
    exp_loss <- l_gd*def_prob
    # def_thresh <- qnorm(def_prob)
    
    loss_es <- da_ta()$losses
    c_var <- da_ta()$c_var
    
    densi_ty <- density(loss_es)
    x_max <- max(densi_ty$x)
    y_max <- max(densi_ty$y)
    # Plot density of portfolio losses
    plot(densi_ty, col="blue", lwd=3,
         main="Portfolio Loss Distribution")
    # Add vertical line for expected loss
    abline(v=exp_loss, col="orange", lwd=4)
    text(x=exp_loss-0.001, y=3*y_max/4, labels="expected loss",
         lwd=2, srt=90, pos=3)
    # Add vertical line for CVaR attachment
    abline(v=at_tach, col="red", lwd=4)
    text(x=at_tach-0.001, y=3*y_max/4, labels="CVaR attachment",
         lwd=2, srt=90, pos=3)
    
    # Draw shaded polygon for CVaR
    in_dex <- (densi_ty$x > at_tach)
    x_var <- c(min(densi_ty$x[in_dex]), densi_ty$x[in_dex], max(densi_ty$x))
    polygon(x_var, c(-1, densi_ty$y[in_dex], -1), 
            col="red", border=NA, density=10)
    # text(x=0.045, y=0, labels="CVaR", lwd=2, pos=3)
    
    # Text with CVaR attachment
    text(x_max-0.01, y_max, 
         lab=paste0(
           "Default probability = ", format(100*def_prob, digits=3), "%", "\n",
           "Loss severity = ", format(100*l_gd, digits=3), "%", "\n",
           "Correlation = ", format(100*rh_o, digits=3), "%", "\n",
           "CVaR attachment = ", format(100*at_tach, digits=3), "%", "\n",
           "CVaR = ", 100*round(c_var, 4), "%"), 
         adj=c(1, 1), cex=1.2, lwd=2)
  })  # end output plot
  
}  # end server code


##############################
## Return a Shiny app object

shiny::shinyApp(ui=inter_face, server=ser_ver)

