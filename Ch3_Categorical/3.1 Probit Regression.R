# PROBIT REGRESSION FOR BINARY AND ORDINAL OUTCOMES ----

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/employee.csv'

# create data frame from github data
employee <- read.csv(data_url)

# FIT PROBIT MODEL WITH A BINARY OUTCOME ----

# probit regression with a binary outcome
model1 <- rblimp(
    data = employee,
    ordinal = 'turnover male',
    model = 'turnover ~ lmx empower male', 
    seed = 90291,
    burn = 10000,
    iter = 10000,
    nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1,'turnover')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot individual-level predicted probabilities
bivariate_plot(y_vars = 'turnover.1.probability', 
               x_vars = c('lmx','empower','male'),
               discrete_x = c('male'),
               model = model1)

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model1)
imputed_vs_observed_plot(model1)

# standardized residuals vs. predicted
bivariate_plot(turnover.residual ~ turnover.predicted, standardize = 'y', model = model1)

# standardized residuals vs. predictors
bivariate_plot(y_vars = 'turnover.residual', 
               x_vars = c('lmx','empower'),
               standardize = 'y',
               model = model1)

# FIT PROBIT MODEL WITH AN ORDINAL OUTCOME ----

# probit regression with an ordinal outcome
# large # of iterations to achieve N_eff values for thresholds (psr ok w fewer)
model2 <- rblimp(
  data = employee,
  ordinal = 'jobsat male',
  model = 'jobsat ~ lmx empower male', 
  seed = 90291,
  burn = 50000,
  iter = 50000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2,'jobsat')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model2)
imputed_vs_observed_plot(model2)

# standardized residuals vs. predicted
bivariate_plot(jobsat.residual ~ jobsat.predicted, standardize = 'y', model = model2)

# standardized residuals vs. predictors
bivariate_plot(y_vars = 'jobsat.residual', 
               x_vars = c('lmx','empower'),
               standardize = 'y',
               model = model2)
