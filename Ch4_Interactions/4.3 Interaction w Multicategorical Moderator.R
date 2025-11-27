# BRIAN NOTES ----
# simple command currently does not work with explicit dummy codes?
# maintain consistent coloring across numbers of moderator groups?

# INTERACTION INVOLVING A MULTICATEGORICAL MODERATOR

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/reading.csv'

# create data frame from github data
reading <- read.csv(data_url)

# FIT MODEL WITH A MULTICATEGORICAL MODERATOR ----

# interaction with explicit dummy codes and interaction test
model1 <- rblimp(
  data = reading, 
  nominal = 'lrnprob1risk male hispanic',
  center = 'read1',
  model = 'read9 ~ read1 lrnprob1risk.2 lrnprob1risk.3 read1*lrnprob1risk.2@slpdiff2 read1*lrnprob1risk.3@slpdiff3 male hispanic',  
  waldtest = 'slpdiff2:slpdiff3 = 0',
  seed = 90291,                                              
  burn = 10000,                                              
  iter = 10000)                                                

# print output
output(model1)

# interaction with automatic dummy codes and interaction test
model2 <- rblimp(
  data = reading, 
  nominal = 'lrnprob1risk male hispanic',
  center = 'read1',
  model = 'read9 ~ read1 lrnprob1risk read1*lrnprob1risk male hispanic',  
  simple = 'read1 | lrnprob1risk',   
  waldtest = 'read9 ~ read1 lrnprob1risk male hispanic',
  seed = 90291,                                              
  burn = 10000,                                              
  iter = 10000,
  nimps = 20)                                                

# print output and parameter distributions
output(model2)

# plot parameter distributions
posterior_plot(model2,'read9')

# plot conditional effects
simple_plot(read9 ~ read1 | lrnprob1risk.2 + lrnprob1risk.3, model2)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model2)

# plot distributions and residuals
univariate_plot(vars = c('read9','read9.residual'), model2)

# plot standardized residuals vs. predicted values
bivariate_plot(read9.residual ~ read9.predicted, standardize = 'y', model = model2)

# plot standardized residuals vs. numeric predictors
bivariate_plot(y_vars = 'read9.residual', 
               x_vars = 'read1',
               standardize = 'y',
               model = model2)



