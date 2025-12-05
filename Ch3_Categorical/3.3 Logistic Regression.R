# BRIAN NOTES ----
# The .residual behavior with logistic models is incorrect
# No predicted values for logistic outcomes

# LOGISTIC REGRESSION FOR BINARY AND MULTICATEGORICAL OUTCOMES ----

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/alcoholuse.csv'

# create data frame from github data
alcoholuse <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT BINARY LOGISTIC MODEL ----
#------------------------------------------------------------------------------#

# binary logistic regression
model1 <- rblimp(
  data = alcoholuse,
  nominal = 'drinker college male',
  # fixed = 'male',
  model = 'drinker ~ alcage college age male', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1,'drinker')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model1)

# # plot distributions and residuals
# univariate_plot(vars = c('intensity','drinker.1.residual'), model2)

# plot individual-level predicted probabilities
bivariate_plot(y_vars = 'drinker.1.probability', 
               x_vars = c('alcage','college','age','male'),
               model = model1)

# plot standardized residuals vs. predicted values
# bivariate_plot(drinker.1.residual ~ turnover.predicted, standardize = 'y', model = model1)

# plot standardized residuals vs. numeric predictors
bivariate_plot(y_vars = 'drinker.1.residual', 
               x_vars = c('alcage','age'),
               standardize = 'y',
               model = model1)

#------------------------------------------------------------------------------#
# FIT MULTINOMIAL LOGISTIC MODEL ----
#------------------------------------------------------------------------------#

# multinomial logistic regression
model2 <- rblimp(
  data = alcoholuse,
  nominal = 'drinkingfreq college male',
  # fixed = 'male',
  model = 'drinkingfreq ~ alcage college age male', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2,'drinkingfreq')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model2)

# # plot distributions and residuals
# univariate_plot(vars = c('intensity','drinker.1.residual'), model2)

# plot individual-level predicted probabilities
bivariate_plot(y_vars = c('drinkingfreq.1.probability','drinkingfreq.2.probability'), 
               x_vars = c('alcage','college','age','male'),
               model = model2)

# plot standardized residuals vs. predicted values
# bivariate_plot(drinker.1.residual ~ turnover.predicted, standardize = 'y', model = model1)

# plot standardized residuals vs. numeric predictors
bivariate_plot(y_vars = c('drinkingfreq.1.residual','drinkingfreq.2.residual'), 
               x_vars = c('alcage','age'),
               standardize = 'y',
               model = model2)


