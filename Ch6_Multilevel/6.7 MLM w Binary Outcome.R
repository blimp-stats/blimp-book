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

# plot individual-level predicted probabilities
bivariate_plot(turnover.1.probability ~ lmx, model1)
bivariate_plot(turnover.1.probability ~ male, model1)
bivariate_plot(turnover.1.probability ~ lmx.mean[team], model1)
bivariate_plot(turnover.1.probability ~ climate, model1)

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model1)
imputed_vs_observed_plot(model1)
residuals_plot(model1)

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

# plot individual-level predicted probabilities
bivariate_plot(turnover.1.probability ~ lmx, model2)
bivariate_plot(turnover.1.probability ~ male, model2)
bivariate_plot(turnover.1.probability ~ lmx.mean[team], model2)
bivariate_plot(turnover.1.probability ~ climate, model2)

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)