# PROBIT REGRESSION FOR BINARY AND ORDINAL OUTCOMES ----

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/employee.csv'

# plotting function
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

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
    iter = 10000)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1,'turnover')

# GRAPHICAL DIAGNOSTICS ----

# multiple imputations for graphical diagnostics 
model2 <- rblimp(
  data = employee,
  ordinal = 'turnover male',
  model = 'turnover ~ lmx empower male', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# plot individual-level predicted probabilities
bivariate_plot(model2, turnover.1.probability ~ lmx)
bivariate_plot(model2, turnover.1.probability ~ empower)
bivariate_plot(model2, turnover.1.probability ~ male)

# plot distributions, observed vs. imputed scores, and residuals
imputation_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)

# FIT PROBIT MODEL WITH AN ORDINAL OUTCOME ----

# probit regression with an ordinal outcome
model3 <- rblimp(
  data = employee,
  ordinal = 'jobsat male',
  model = 'jobsat ~ lmx empower male', 
  seed = 90291,
  burn = 50000,
  iter = 50000)

# print output
output(model3)

# plot parameter distributions
posterior_plot(model3,'turnover')

# GRAPHICAL DIAGNOSTICS ----

# multiple imputations for graphical diagnostics 
model4 <- rblimp(
  data = employee,
  ordinal = 'jobsat male',
  model = 'jobsat ~ lmx empower male', 
  seed = 90291,
  burn = 50000,
  iter = 50000,
  nimps = 20)

# print output
output(model4)

# plot distributions, observed vs. imputed scores, and residuals
imputation_plot(model4)
imputed_vs_observed_plot(model4)
residuals_plot(model4)
