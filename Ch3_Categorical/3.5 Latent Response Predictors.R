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

# FIT PROBIT MODEL WITH A LATENT RESPONSE VARIABLE PREDICTOR ----

# probit regression with a binary outcome
model1 <- rblimp(
    data = employee,
    ordinal = 'jobsat male',
    nominal = 'turnover',
    model = 'turnover ~ lmx empower jobsat.latent male', 
    seed = 90291,
    burn = 10000,
    iter = 10000,
    nimps = 100)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1,'turnover')

# GRAPHICAL DIAGNOSTICS ----

# plot distributions, observed vs. imputed scores, and residuals
imputation_plot(model1)
imputed_vs_observed_plot(model1)
residuals_plot(model1)