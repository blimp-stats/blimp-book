# INTERACTION INVOLVING A CONTINUOUS MODERATOR

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(rblimp)

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/reading.csv'

# create data frame from github data
reading <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT INTERACTIVE MODEL ----
#------------------------------------------------------------------------------#

# interaction with a continuous moderator
model <- rblimp(
  data = reading, 
  nominal = 'male hispanic',
  center = 'read1 lrnprob1',
  model = 'read9 ~ read1 lrnprob1 read1*lrnprob1 male hispanic',  
  simple = 'read1 | lrnprob1',      
  seed = 90291,                                              
  burn = 10000,                                              
  iter = 10000,
  nimps = 20)                                                

# print output
output(model)

# plot parameter distributions
posterior_plot(model,'read9')

# plot conditional effects and johnson-neyman regions of significance
simple_plot(read9 ~ read1 | lrnprob1, model)
jn_plot(read9 ~ read1 | lrnprob1, model)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model)

# plot distributions and residuals
univariate_plot(vars = c('read9','read9.residual'), model)

# plot standardized residuals vs. predicted values
bivariate_plot(read9.residual ~ read9.predicted, standardize = 'y', model = model)

# plot standardized residuals vs. numeric predictors
bivariate_plot(y_vars = 'read9.residual', 
               x_vars = c('read1','lrnprob1'),
               standardize = 'y',
               model = model)

