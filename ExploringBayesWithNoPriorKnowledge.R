###:###:###:###:###:###:###:###:###
# R Code accompanying the article
# Exploring Bayesian Statistics with no Prior Knowledge
# Author: Marvin Schmitt
# MAIN FILE
###:###:###:###:###:###:###:###:###

# Load packages
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)

library(shinydashboard)
library(shiny)
library(rsconnect)

# set wd to current path
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Run or deploy web app ####

# User Interface Code at ./ui.R
# Server Code at ./server.R

#::: UNCOMMENT AND EXECUTE THE DESIRED ACTION :::
# runApp()
deployApp()
# terminateApp('ExploringBayesWithNoPriorKnowledge')

###:###:###:###:###:###:###:###:### END SECTION


# Plot generation global setup####

theme_set(theme_classic())  # set global ggplot theme
set.seed(17)                # set RNG seed


# Figure: Frequentist basic idea, law of large numbers ####

# Set hyperparameters 
N = 1000
p = 0.5

# simulate N coin flips with fair coin (p=0.5)
x = sample(0:1, size=N, replace=TRUE, prob = c(p, 1-p))

# build dataframe with n = 1...N and respective relative frequency after n coin flips
df.largenumbers = data.frame(
  n = 1:N,
  rel_freq = 0
)
for (i in 1:N){
  df.largenumbers[i, "rel_freq"] = mean(x[1:i])
}

# plotting
freq_color = "darkgreen"
prob_color = "darkblue"
dev = max(abs(p - df.largenumbers$rel_freq))

p.largenumbers = ggplot(df.largenumbers,aes(x=n,y=rel_freq))+
  scale_x_continuous(expand=c(0.02,0)) +
  scale_y_continuous(expand=c(0,0), limits=c(p-dev, p+dev))+
  geom_hline(yintercept = p, colour=prob_color, lwd=0.8, linetype="dotted") +
  labs(title="", y="Relative frequency")+
  geom_line(colour="darkgreen")+
  annotate("text", x=quantile(df.largenumbers$n)[4], y=0.85*p, label = "Relative frequency", colour=freq_color)+
  annotate("text", x=quantile(df.largenumbers$n)[4], y=1.15*p, label = "True probability",  colour=prob_color)+
  annotate("point", x=df.largenumbers$n[nrow(df.largenumbers)], y=mean(x), colour=freq_color, size = 3)


p.largenumbers
save_plot("figures/largenumbers.png", p.largenumbers)

###:###:###:###:###:###:###:###:### END SECTION





# Figure: Bayesian Inference Basic Idea ####
# setup
set.seed(42)
dx = .001
x = seq(0,1,by=dx)


df.basicbayes = data.frame(
  x = x,
  prior = dnorm(x, 0.5, 0.15), # gaussian prior with mu=0.5, sd=0.15
  posterior = dnorm(x, 0.55, 0.10) # gaussian posterior that's shifted to the right and contracted
)


# plotting
PRIOR_COLOR = "#e75480"
POSTERIOR_COLOR = "darkgreen"

p.basicbayes = ggplot(df.basicbayes)+
  geom_line(aes(x=x,y=prior), colour=PRIOR_COLOR)+ 
  geom_area(aes(x=x,y=prior), fill=PRIOR_COLOR, alpha=.15) +
  geom_line(aes(x=x,y=posterior), colour=POSTERIOR_COLOR)+ 
  geom_area(aes(x=x,y=posterior), fill=POSTERIOR_COLOR, alpha=.15) +  
  scale_y_continuous(expand = c(0.000, 0), limits = c(0, 1.05*max(c(df.basicbayes$prior, df.basicbayes$posterior))))+
  labs(title="", y="density", x="Probability of 'heads'")+
  geom_segment(aes(x = 0.5 , y = 0, xend = 0.5, yend = max(prior)), colour=PRIOR_COLOR, lwd=0.8, linetype="dotted")+
  geom_segment(aes(x = 0.55 , y = 0, xend = 0.55, yend = max(posterior)), colour=POSTERIOR_COLOR, lwd=0.8, linetype="dotted")+
  annotate("text", 0.25, y=2.5, label = "Prior belief", colour=PRIOR_COLOR)+
  annotate("text", 0.80, y=3.5, label = "Posterior belief", colour=POSTERIOR_COLOR)

p.basicbayes
save_plot("figures/basicbayes.png", p.basicbayes)

###:###:###:###:###:###:###:###:### END SECTION


# Figure: Scatter plot correlation ####
# setup
set.seed(42)
N = 50
x = rnorm(N, 100, 10)
y1 = x + 0.5*rnorm(N, 0, 30) # y1 = x + noise
y2 = x + rnorm(N, 0, 5) # y2 = x + noise
data = data.frame(x=x,y1=y1,y2=y2) # build data frame


# plot
CORR_1_COLOR = "#0077be"
CORR_2_COLOR = "#ba9200"
TITLE=""
YLAB = "Subject's IQ"

p.corr.1 = ggplot(data=data, aes(x=x,y=y1))+
  geom_jitter(size=2.5, color=CORR_1_COLOR)+
  xlim(70,130)+
  ylim(70,130)+
  annotate("text", x=80, y=120, label = paste("r =", cor(data$x, data$y1) %>% round(2)), colour=CORR_1_COLOR) +
  labs(title=TITLE, x="Father's IQ", y = YLAB)

p.corr.2 = ggplot(data=data, aes(x=x,y=y2))+
  geom_jitter(size=2.5, color=CORR_2_COLOR)+
  xlim(70,130)+
  ylim(70,130)+
  annotate("text", x=80, y=120, label = paste("r =", cor(data$x, data$y2) %>% round(2)), colour=CORR_2_COLOR) +
  labs(title=TITLE, x="Mother's IQ", y = YLAB)

p.corr = plot_grid(p.corr.1, p.corr.2,nrow=1, ncol=2, labels="AUTO")

p.corr

save_plot("figures/correlation.png", p.corr, ncol=2, base_asp = 1)

###:###:###:###:###:###:###:###:### END SECTION



# Figure: Sidney the Physician ####

# setup
set.seed(42)
dx = .001 # steps
x = seq(0.50,1,by=dx) # quantile vector
k = 4 # number of correct trials ("R")
n = 5 # number of trials

prior = dbeta((x-0.48), 1.5, 6) # prior with mode slightly above 0.5. shape is beta-distribution
prior = prior / (sum(prior) * dx) # normalize 

likelihood = dbinom(x = k, size = n, prob = x) # get likelihood function depending on underlying success rate
likelihood = likelihood / (sum(likelihood) * dx) # normalize

pxl = prior * likelihood # prior * likelihood (unnormalized posterior)
posterior = pxl/(sum(pxl) * dx) # normalize to get density distribution

df.physician = data.frame(x, prior, likelihood, posterior) # build dataframe

PHYS.TITLE = "" # "Bayesian Inference on Sidney the Physician"

# PLOTTING

PRIOR_COLOR = "#e75480"
LIKELIHOOD_COLOR = "#0077be"
POSTERIOR_COLOR = "darkgreen"
LIKELIHOOD_LABEL = paste("Likelihood\n(of ", k, "R ", n-k, "F)", sep="")

p.physician = ggplot(df.physician)+
  geom_line(aes(x=x,y=prior), colour=PRIOR_COLOR)+ 
  geom_area(aes(x=x,y=prior), fill=PRIOR_COLOR, alpha=.15) +
  
  geom_line(aes(x=x,y=likelihood), colour=LIKELIHOOD_COLOR)+ 
  geom_area(aes(x=x,y=likelihood), fill=LIKELIHOOD_COLOR, alpha=.15) +
  
  geom_line(aes(x=x,y=posterior), colour=POSTERIOR_COLOR)+ 
  geom_area(aes(x=x,y=posterior), fill=POSTERIOR_COLOR, alpha=.15) +  
  
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand = c(0.000, 0), limits = c(0, 1.05*max(c(prior, likelihood, posterior))))+
  labs(title=PHYS.TITLE, y="density", x="Sidney's diagnostic accuracy P")+
  
  geom_segment(aes(x = x[which.max(prior)] , y = 0, xend = x[which.max(prior)], yend = max(prior)), colour=PRIOR_COLOR, lwd=0.6, linetype="dotted")+
  geom_segment(aes(x = x[which.max(likelihood)] , y = 0, xend = x[which.max(likelihood)], yend = max(likelihood)), colour=LIKELIHOOD_COLOR, lwd=0.6, linetype="dotted")+
  geom_segment(aes(x = x[which.max(posterior)] , y = 0, xend = x[which.max(posterior)], yend = max(posterior)), colour=POSTERIOR_COLOR, lwd=0.6, linetype="dotted")+
  
  annotate("text", x=x[which.max(prior)], y=max(prior) + 0.1, label = "Prior", colour=PRIOR_COLOR)+
  annotate("text", x=0.82, y=3.2, label = LIKELIHOOD_LABEL, colour=LIKELIHOOD_COLOR)+
  annotate("text", x=x[which.max(posterior)], y=max(posterior) + 0.1, label = "Posterior", colour=POSTERIOR_COLOR)


p.physician
save_plot("figures/physician.png", p.physician)

###:###:###:###:###:###:###:###:### END SECTION


# Figure: Bayesian Updating -- Today's Prior is Tomorrow's Posterior ####
# setup
set.seed(186) # monte carlo'ing for sequence 4-5-3-4 while maintaining generalizability
dx = .001 # steps
x = seq(0.50,1,by=dx) # quantile vector
N_UPDATING_ITERATIONS = 4
n = 5 # number of trials
Ks = sample(ceiling(n/2):n, N_UPDATING_ITERATIONS, replace=TRUE) # restrict to trial-level accuracy >50%


prior = dbeta((x-0.48), 1.5, 6) # prior with mode slightly above 0.5
prior = prior / (sum(prior) * dx) # normalize

UPDATING.TITLE = "" # "Bayesian Inference on Sidney the Physician"

# PLOTTING

PRIOR_COLOR = "#e75480"
LIKELIHOOD_COLOR = "#0077be"
POSTERIOR_COLOR = "darkgreen"


plots = vector("list", length = N_UPDATING_ITERATIONS) # empty list to store plots in
for (i in 1:N_UPDATING_ITERATIONS){
  k = Ks[i] # get number of correct diagnoses of current experiment
  
  likelihood = dbinom(x = k, size = n, prob = x) # get likelihood function depending on underlying success rate
  likelihood = likelihood / (sum(likelihood) * dx) # normalize
  
  LIKELIHOOD_LABEL = paste("Likelihood\n(of ", k, "R ", n-k, "F)", sep="") # build label with correct k and n
  
  pxl = prior * likelihood # unnormalized posterior
  posterior = pxl / (sum(pxl) * dx) #normalize
  
  df.updating = data.frame(x, prior, likelihood, posterior) # build dataframe
  
  # plot and store in plots list
  plots[[i]] = ggplot(df.updating) +
    geom_line(aes(x=x,y=prior), colour=PRIOR_COLOR)+ 
    geom_area(aes(x=x,y=prior), fill=PRIOR_COLOR, alpha=.15) +
    
    geom_line(aes(x=x,y=likelihood), colour=LIKELIHOOD_COLOR)+ 
    geom_area(aes(x=x,y=likelihood), fill=LIKELIHOOD_COLOR, alpha=.15) +
    
    geom_line(aes(x=x,y=posterior), colour=POSTERIOR_COLOR)+ 
    geom_area(aes(x=x,y=posterior), fill=POSTERIOR_COLOR, alpha=.15) +  
    
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand = c(0.000, 0), limits = c(0, 6.5))+
    labs(title=UPDATING.TITLE, y="density", x="Sidney's diagnostic accuracy P")+
    
    geom_segment(aes(x = x[which.max(prior)] , y = 0, xend = x[which.max(prior)], yend = max(prior)), colour=PRIOR_COLOR, lwd=0.6, linetype="dotted")+
    geom_segment(aes(x = x[which.max(likelihood)] , y = 0, xend = x[which.max(likelihood)], yend = max(likelihood)), colour=LIKELIHOOD_COLOR, lwd=0.6, linetype="dotted")+
    geom_segment(aes(x = x[which.max(posterior)] , y = 0, xend = x[which.max(posterior)], yend = max(posterior)), colour=POSTERIOR_COLOR, lwd=0.6, linetype="dotted")+
    
    annotate("text", x=x[which.max(prior)], y=max(prior) + 0.1, label = "Prior", colour=PRIOR_COLOR)+
    annotate("text", x=0.9, y=3, label = LIKELIHOOD_LABEL, colour=LIKELIHOOD_COLOR)+
    annotate("text", x=x[which.max(posterior)], y=max(posterior) + 0.1, label = "Posterior", colour=POSTERIOR_COLOR)

  # Updating step: current posterior will be the prior of the next iteration.
  prior = posterior
} # end for

GRID_LABELS = paste("Inference #", 1:N_UPDATING_ITERATIONS, ": ", Ks, "/", n, " right diagnoses", sep="") # build labels of the plot grid
p.updating = plot_grid(plotlist = plots, nrow=1, labels=GRID_LABELS) # build grid
save_plot("figures/updating.png", p.updating, ncol = N_UPDATING_ITERATIONS)

###:###:###:###:###:###:###:###:### END SECTION

# Clean up ####
rm(list=ls())
###:###:###:###:###:###:###:###:### END SECTION


###:###:###:###:###:###:###:###:### 
###:###:  END OF SCRIPT    :###:### 
###:###:###:###:###:###:###:###:### 