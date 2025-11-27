# BRIAN NOTES ----
# remove odds ratio for intercept@ ?
# We need to discuss the .residual behavior with logistic models

# GENERALIZED LINEAR MIXED MODEL WITH DISCRETE OUTCOME

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/employee.csv'

# create data frame from github data
employee <- read.csv(data_url)

# FIT LOGISTIC MODEL WITH COMBINED MODEL SPECIFICATION ----

# logtistic regression with random intercepts
model1 <- rblimp(
  data = employee,
  clusterid = 'team',
  nominal = 'turnover male',
  center = 'groupmean = lmx; grandmean = lmx.mean climate male',
  model = 'turnover ~ lmx male lmx.mean climate',
  seed = 90291,
  burn = 20000,
  iter = 20000,
  nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1,'turnover')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model1)

# plot individual-level predicted probabilities
bivariate_plot(x_vars = c('lmx','male'), 
               y_vars = 'turnover.1.probability', standardize = 'y', model = model1)

# plot distributions and residuals
univariate_plot(vars = c('turnover.1.team','turnover.1.residual','turnover.1.probability'), model1)

# plot standardized level-2 residuals vs. level-2 predictors
bivariate_plot(x_vars = c('lmx.mean.team','climate'), 
               y_vars = 'turnover.1.team', standardize = 'y', model = model1)

# plot standardized level-1 residuals vs. level-1 predictors
bivariate_plot(x_vars = c('lmx','male'), 
               y_vars = 'turnover.1.residual', standardize = 'y', model = model1)

# FIT MODEL WITH LATENT VARIABLE SPECIFICATION ----

# logtistic regression with random intercepts
model2 <- rblimp(
  data = employee,
  clusterid = 'team',
  nominal = 'turnover male',
  latent = 'team = ranicept;',
  center = 'groupmean = lmx; grandmean = lmx.mean climate male',
  model = '
    level2:
    ranicept ~ intercept lmx.mean climate;
    level1:
    turnover ~ intercept@ranicept lmx male;',
  seed = 90291,
  burn = 30000,
  iter = 30000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2,'turnover')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model2)

# plot individual-level predicted probabilities
bivariate_plot(x_vars = c('lmx','male'), 
               y_vars = 'turnover.1.probability', standardize = 'y', model = model2)

# plot distributions and residuals
univariate_plot(vars = c('ranicept.latent','ranicept.residual','turnover.1.probability'), model2)

# plot standardized level-2 residuals vs. level-2 predictors
bivariate_plot(x_vars = c('lmx.mean.team','climate'), 
               y_vars = 'ranicept.residual', standardize = 'y', model = model2)

# plot standardized level-1 residuals vs. level-1 predictors
bivariate_plot(x_vars = c('lmx','male'), 
               y_vars = 'turnover.1.residual', standardize = 'y', model = model2)