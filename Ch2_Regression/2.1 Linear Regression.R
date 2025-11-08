# YEO-JOHNSON NORMALIZING TRANSFORMATION FOR NONNORMAL CONTINUOUS VARIABLES

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

# plotting function
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# FIT LINEAR REGRESSION MODEL ----

# raw score predictors
model1 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  model = 'dpdd ~ female els age inflam_sum', 
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(model1)
trace_plot(model1, 'dpdd')

# mean-centered predictors
model2 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  center = 'female els age inflam_sum',
  model = 'dpdd ~ female els age inflam_sum', 
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model2)

# trace plot of model parameters
trace_plot(model2, 'dpdd') + ggplot2::xlim(0, 250) + ggplot2::theme_minimal()

# plot parameter distributions
posterior_plot(model2,'dpdd')

# GRAPHICAL DIAGNOSTICS ----

# multiple imputations for graphical diagnostics 
model3 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  center = 'female els age inflam_sum',
  model = 'dpdd ~ female els age inflam_sum', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model3)

# plot distributions
imputation_plot(model3)

# plot observed vs. imputed scores
imputed_vs_observed_plot(model3)

# plot residuals
residuals_plot(model3)



