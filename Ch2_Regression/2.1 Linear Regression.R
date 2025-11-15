# LINEAR REGRESSION

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
library(ggplot2)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflamm <- read.csv(data_url)

# FIT LINEAR REGRESSION MODEL ----

# raw score predictors
model1 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  model = 'dpdd ~ female els age inflam_sum', 
  seed = 90291,
  burn = 10000,
  iter = 10000)

# trace plot of model parameters
trace_plot(model2, 'dpdd') + ggplot2::xlim(0, 250) + ggplot2::theme_minimal()

# print output
output(model1)

# plot parameter distributions
posterior_plot(model2,'dpdd')

# FIT MODEL WITH CENTERED PREDICTORS ----

# mean-centered predictors
model2 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  center = 'female els age inflam_sum',
  model = 'dpdd ~ female els age inflam_sum', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# trace plot of model parameters
trace_plot(model2, 'dpdd') + ggplot2::xlim(0, 250) + ggplot2::theme_minimal()

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2,'dpdd')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
imputation_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)



