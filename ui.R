###:###:###:###:###:###:###:###:###
# R Code accompanying the article
# Exploring Bayesian Statistics with no Prior Knowledge
# Author: Marvin Schmitt
# SHINY WEB APP USER INTERFACE FILE
###:###:###:###:###:###:###:###:###

# load packages
library(tidyverse)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(gridExtra)

# Build UI
ui <- fluidPage(
  # set title and subtitles
  titlePanel("Bayesian Inference"),
  tags$h6("Web App accompanying the article 'Exploring Bayesian Statistics with no Prior Knowledge'"),
  tags$h6("Author: Marvin Schmitt"),
  
  sidebarLayout(
    sidebarPanel(
      # PRIOR SECTION
      tags$h3("Prior"),
       sliderInput(
         "prior.beta.p1",label=HTML("&alpha;"), 
         min=1, max=11, step=0.1, value=1.5,
         #animate = animationOptions(interval = 1000, loop = TRUE) #uncomment for animation functionality
       ),
       sliderInput(
         "prior.beta.p2",label=HTML("&beta;"), 
         min=1, max=11, step=0.1, value=10,
         #animate = animationOptions(interval = 1000, loop = TRUE) #uncomment for animation functionality
       ),
      hr(style = "border-top: 2px solid #000000;"),
      
      # LIKELIHOOD SECTION
      tags$h3("Likelihood"),
      sliderInput("likelihood1",label="Number of trials n:", 
                  min=4, max=10, step=1, value=5,
                  #animate = animationOptions(interval = 1000, loop = TRUE) #uncomment for animation functionality
      ),
      sliderInput("likelihood2",label="Number of correct diagnoses k:", 
                  min=0, max=10, step=1, value=4,
                  animate = animationOptions(interval = 5000, loop = TRUE)
      )),
    mainPanel(
      # PLOT AREA
      fluidRow(
        column(10, plotOutput('bayesplot'))
        )
    )))