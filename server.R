###:###:###:###:###:###:###:###:###
# R Code accompanying the article
# Exploring Bayesian Statistics with no Prior Knowledge
# Author: Marvin Schmitt
# SHINY WEB APP SERVER CODE FILE
###:###:###:###:###:###:###:###:###

# load packages 
library(tidyverse)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(gridExtra)

server = function(input, output, session) { 
  
  # Update k slider if n is changed
  observeEvent(input$likelihood1,  {
    updateSliderInput(session = session, inputId = "likelihood2", min = ceiling(input$likelihood1/2), max = input$likelihood1)
  })
  
  # Global non-reactive setup
  theme_set(theme_classic())
  PRIOR_COLOR = "#e75480"
  LIKELIHOOD_COLOR = "#0077be"
  POSTERIOR_COLOR = "darkgreen"
  MAP_COLOR = POSTERIOR_COLOR
  
  # create the plot
  output$bayesplot = renderPlot({
    
    # read sliders of likelihood
    n = input$likelihood1
    k = input$likelihood2
    
    # generate labels for likelihood and plot
    LIKELIHOOD_LABEL = paste("Likelihood\n(of ", k, "R ", n-k, "F)", sep="")
    PLOT_TITLE = paste("Bayesian Inference of Sidney's diagnostic power for ", n, " trials and ", k, " correct diagnoses.", sep="")
    
    # quantile vector x
    dx = .001
    x = seq(0.5,1,by=dx)
    
    # get prior and likelihood, compute posterior.
    prior = dbeta(x-0.48, input$prior.beta.p1, input$prior.beta.p2)
    prior = prior / (sum(prior) * dx) # normalize (not necessary but nice)
    
    likelihood = dbinom(x = k, size = n, prob = x) # get likelihood function depending on underlying success rate
    likelihood = likelihood / (sum(likelihood) * dx) # normalize
    
    pxl = prior * likelihood
    posterior = pxl/(sum(pxl) * dx)
    df = data.frame(x,prior, likelihood, posterior)
    
    # get maximum (x, y) of prior, likelihood and posterior
    Max_Prior = c(x = x[which.max(prior)], y = max(prior))
    Max_Likelihood = c(x = x[which.max(likelihood)], y = max(likelihood))
    MAP = c(x = x[which.max(posterior)], y = max(posterior))

    
    # plot
    YMAX = max(8, max(c(Max_Prior["y"], Max_Likelihood["y"], MAP["y"])))
    
    p = ggplot(df)+
      #prior
      geom_line(aes(x=x,y=prior), colour=PRIOR_COLOR)+ 
      geom_area(aes(x=x,y=prior), fill=PRIOR_COLOR, alpha=.15) +
      
      # likelihood
      geom_line(aes(x=x,y=likelihood), colour=LIKELIHOOD_COLOR)+ 
      geom_area(aes(x=x,y=likelihood), fill=LIKELIHOOD_COLOR, alpha=.15) +
      
      # posterior
      geom_line(aes(x=x,y=posterior), colour=POSTERIOR_COLOR)+ 
      geom_area(aes(x=x,y=posterior), fill=POSTERIOR_COLOR, alpha=.15) +  
      
      # axes and labels
      scale_x_continuous(expand=c(0,0))+
      scale_y_continuous(expand = c(0.000, 0), limits = c(0, 1.05*YMAX))+
      labs(title=PLOT_TITLE, y="density", x="Sidney's diagnostic power P")+
      
      # maximum prior/likelihood/posterior dashed lines
      geom_segment(aes(x = Max_Prior["x"] , y = 0, xend = Max_Prior["x"], yend = Max_Prior["y"]), colour=PRIOR_COLOR, lwd=0.6, linetype="dotted")+
      geom_segment(aes(x = Max_Likelihood["x"] , y = 0, xend = Max_Likelihood["x"], yend = Max_Likelihood["y"]), colour=LIKELIHOOD_COLOR, lwd=0.6, linetype="dotted")+
      geom_segment(aes(x = MAP["x"] , y = 0, xend = MAP["x"], yend = MAP["y"]), colour=POSTERIOR_COLOR, lwd=0.6, linetype="dotted")+
      
      # annotating the curve labels
      annotate("text", x=Max_Prior["x"], y=Max_Prior["y"] + 0.1, label = "Prior", colour=PRIOR_COLOR)+
      annotate("text", x=0.82, y=max(c(Max_Prior["y"], Max_Likelihood["y"], MAP["y"]))/2, label = LIKELIHOOD_LABEL, colour=LIKELIHOOD_COLOR)+
      annotate("text", x=MAP["x"], y=MAP["y"] + 0.1, label = "Posterior", colour=POSTERIOR_COLOR)
    
    p # show plot
  })
}