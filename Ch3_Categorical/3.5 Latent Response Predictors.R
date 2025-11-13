# LOGISTIC REGRESSION WITH A LATENT RESPONSE VARIABLE PREDICTOR ----

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/employee.csv'

# plotting function
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# create data frame from github data
employee <- read.csv(data_url)

# FIT MODEL ----

# logistic regression with a latent response variable predictor
# thresholds require large number of iterations to achieve adequate N_eff 
model1 <- rblimp(
    data = employee,
    ordinal = 'jobsat male',
    nominal = 'turnover',
    model = '
    focal:
    turnover ~ lmx empower jobsat.latent male;
    predictors:
    jobsat ~ lmx empower male', 
    seed = 90291,
    burn = 30000,
    iter = 30000)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1,'turnover')

# GRAPHICAL DIAGNOSTICS ----

# multiple imputations for graphical diagnostics 
model2 <- rblimp(
  data = employee,
  ordinal = 'jobsat male',
  nominal = 'turnover',
  model = '
    focal:
    turnover ~ lmx empower jobsat.latent male;
    predictors:
    jobsat ~ lmx empower male', 
  seed = 90291,
  burn = 30000,
  iter = 30000,
  nimps = 20)

# print output
output(model2)

# plot distributions, observed vs. imputed scores, and residuals
imputation_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)
