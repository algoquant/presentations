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

lossdistr <- function(x, threshv=-2, rho=0.1, lgd=0.4) {
  qnormv <- ifelse(x/lgd < 0.999, qnorm(x/lgd), 3.1)
  sqrt((1-rho)/rho)*exp(-(sqrt(1-rho)*qnormv - threshv)^2/(2*rho) + qnormv^2/2)/lgd
}  # end lossdistr


# End setup code
##############################


##############################
## Define the user interface

uifun <- shiny::fluidPage(
  titlePanel("Portfolio Loss Distribution"),
  
  # Create four slider inputs with parameters to lossdistr()
  fluidRow(
    column(width=3, sliderInput("rho", label="Correlation:",
                                min=0.0, max=0.9, value=0.2, step=0.01)),
    column(width=3, sliderInput("defprob", label="Default probability:",
                                min=0.0, max=0.9, value=0.2, step=0.01)),
    column(width=3, sliderInput("lgd", label="Loss severity:",
                                min=0.0, max=0.9, value=0.4, step=0.01)),
    column(width=3, sliderInput("attachp", label="Tranche attachment:",
                                min=0.0, max=0.5, value=0.15, step=0.01))
  ),  # end fluidRow
  
  # Render plot in panel
  shiny::plotOutput("plotobj", width="100%", height=650)
)  # end fluidPage interface


##############################
## Define the server code
# The function servfun() accepts the arguments "input" and "output".

servfun <- function(input, output) {

  ## Recalculate the model with new parameters
  # The function shiny::reactive() accepts a block of expressions
  # which calculate the model, and returns the model output.
  datav <- shiny::reactive({
    
    # Extract model parameters from the argument "input"
    attachp <- input$attachp
    defprob <- input$defprob
    rho <- input$rho
    lgd <- input$lgd
    exploss <- lgd*defprob
    threshv <- qnorm(defprob)
    
    # Calculate portfolio losses
    round(
      integrate(function(x) x*lossdistr(x, threshv=threshv, 
            rho=rho, lgd=lgd), low=attachp, up=lgd)$value, 
      digits=5)
  })  # end reactive code
  
  ## Create plot and return it to the output argument
  # The function shiny::reactive() accepts a block of expressions
  # which calculate the model, and returns the model output.
  output$plotobj <- shiny::renderPlot({
    
    # Extract model parameters from the argument "input"
    attachp <- input$attachp
    defprob <- input$defprob
    rho <- input$rho
    lgd <- input$lgd
    exploss <- lgd*defprob
    threshv <- qnorm(defprob)
    
    # Calculate max x-axis range
    xmax <- max(3*exploss, attachp)
    # Calculate max density of portfolio losses (for y-axis scale)
    ymax <- max(sapply(seq(fr=0.01, to=lgd/2, length.out=10), lossdistr, threshv=threshv, rho=rho, lgd=lgd))
    
    # Plot density of portfolio losses
    par(mar=c(5.1, 5.1, 4.1, 2.1))
    curve(expr=lossdistr(x, threshv=threshv, rho=rho, lgd=lgd),
          cex.main=1.5, cex.lab=1.5, cex.axis=1.5, 
          type="l", xlim=c(0, xmax), 
          xlab="Percentage loss", ylab="Density", lwd=3,
          col="orange", main="Distribution of Losses")
    # Add vertical line for expected loss
    abline(v=exploss, col="red", lwd=3)
    text(x=exploss-0.001, y=3*ymax/4, labels="expected loss",
         lwd=2, srt=90, pos=3, cex=1.5)
    # Add vertical line for tranche attachment
    abline(v=attachp, col="blue", lwd=3)
    text(x=attachp-0.001, y=3*ymax/4, labels="tranche attachment",
         lwd=2, srt=90, pos=3, cex=1.5)
    
    # Calculate tranche shading for CVaR
    varisk <- attachp; var_max <- 0.99
    varv <- seq(varisk, var_max, length=100)
    densv <- sapply(varv, lossdistr, threshv=threshv, rho=rho, lgd=lgd)
    # Draw shaded polygon
    polygon(c(varisk, varv, var_max),
            c(-1, densv, -1), col="red", border=NA, density=10)
    # text(x=0.045, y=0, labels="CVaR", lwd=2, pos=3)
    
    # Text with tranche attachment
    text(xmax-0.01, ymax, 
         lab=paste0(
           "Default probability = ", format(100*defprob, digits=3), "%", "\n",
           "Loss severity = ", format(100*lgd, digits=3), "%", "\n",
           "Correlation = ", format(100*rho, digits=3), "%", "\n",
           "Tranche attachment = ", format(100*attachp, digits=3), "%", "\n",
           "Tranche loss = ", 100*datav(), "%"), 
         adj=c(1, 1), cex=1.5, lwd=2)
  })  # end output plot

}  # end server code


##############################
## Return a Shiny app object

shiny::shinyApp(ui=uifun, server=servfun)
