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
nbonds <- 100; nsimu <- 1000
set.seed(1121)
# Simulate vector of systematic factors
sysv <- rnorm(nsimu)
# Simulate matrix of idiosyncratic factors
isync <- matrix(rnorm(nsimu*nbonds), ncol=nsimu)



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
    column(width=3, sliderInput("confl", label="Confidence level:",
                                min=0.9, max=0.99, value=0.95, step=0.01))
  ),  # end fluidRow
  
  # Render plot in panel
  shiny::plotOutput("plot_portf", width="100%", height=650)
)  # end fluidPage interface


##############################
## Define the server code
# The function servfun() accepts the arguments "input" and "output".

servfun <- function(input, output) {
  
  ## The function shiny::reactive() accepts a block of expressions
  # which calculate the model, and returns the model output.
  
  ## Calculate the loss distribution with new parameters
  lossv <- shiny::reactive({
    cat("Calculating the loss distribution\n")
    
    # Extract model parameters from the argument "input"
    defprob <- input$defprob
    rho <- input$rho
    lgd <- input$lgd
    
    # Calculate default probability and threshold
    # exploss <- lgd*defprob
    defthresh <- qnorm(defprob)
    
    # Define correlation parameters
    rhos <- sqrt(rho); rhosm <- sqrt(1-rho)
    
    # Calculate the portfolio losses
    assetm <- t(rhos*sysv + t(rhosm*isync))
    lossv <- lgd*colSums(assetm < defthresh)/nbonds
    
    # Return the losses
    lossv

  })  # end reactive code
  
  
  ## Calculate the VaR and CVaR
  tailrisk <- shiny::reactive({
    cat("Calculating the VaR and CVaR\n")
    
    # Extract model parameters from the argument "input"
    confl <- input$confl
    
    lossv <- lossv()
    # Calculate VaR from confidence level
    varisk <- quantile(lossv, confl)
    # Calculate the CVaR as the mean losses in excess of VaR
    cvar <- mean(lossv[lossv > varisk])
    
    # Return the VaR and CVaR
    c(varisk, cvar)
    
  })  # end reactive code


## Create plot and return it to the output argument
  output$plot_portf <- shiny::renderPlot({
    
    # Extract model parameters from the argument "input"
    confl <- input$confl
    defprob <- input$defprob
    rho <- input$rho
    lgd <- input$lgd
    exploss <- lgd*defprob
    # defthresh <- qnorm(defprob)
    
    # Extract the loss distribution
    lossv <- lossv()
    densv <- density(lossv, from=0)
    xmax <- max(densv$x)
    ymax <- max(densv$y)
    
    # Extract the VaR and CVaR
    varisk <- tailrisk()[1]
    cvar <- tailrisk()[2]
    
    # Plot density of portfolio losses
    par(mar=c(5.1, 5.1, 4.1, 2.1))
    plot(densv, xlab="Percentage loss", col="blue", lwd=3, 
         cex.main=1.5, cex.lab=1.5, cex.axis=1.5, 
         main="Portfolio Loss Distribution")
    # Add vertical line for expected loss
    abline(v=exploss, col="orange", lwd=4)
    text(x=exploss, y=6*ymax/7, labels="expected loss", lwd=2, pos=4, cex=1.5)
    # Add vertical line for VaR
    abline(v=varisk, col="red", lwd=4)
    text(x=varisk, y=4*ymax/5, labels="VaR", lwd=2, pos=4, cex=1.5)
    
    # Draw shaded polygon for CVaR
    intail <- (densv$x > varisk)
    xvar <- c(min(densv$x[intail]), densv$x[intail], max(densv$x))
    polygon(xvar, c(-1, densv$y[intail], -1), 
            col="red", border=NA, density=10)
    # Add text for CVaR
    text(x=5*varisk/4, y=(ymax/7), labels="CVaR", lwd=2, pos=4, cex=1.5)
    
    # Text with CVaR attachment
    text(xmax-0.01, ymax, 
         lab=paste0(
           "Default probability = ", format(100*defprob, digits=3), "%", "\n",
           "Loss severity = ", format(100*lgd, digits=3), "%", "\n",
           "Correlation = ", format(100*rho, digits=3), "%", "\n",
           "VaR = ", format(100*varisk, digits=3), "%", "\n",
           "CVaR = ", format(100*cvar, digits=3), "%"), 
         adj=c(1, 1), cex=1.5, lwd=2)
    
  })  # end output plot
  
}  # end server code


##############################
## Return a Shiny app object

shiny::shinyApp(ui=uifun, server=servfun)

