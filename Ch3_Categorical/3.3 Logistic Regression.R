# BRIAN NOTES ----
# We need to discuss the .residual behavior with logistic models
# Loess lines look odd because residuals on latent metric do not appear to have a zero mean

# LOGISTIC REGRESSION FOR BINARY AND MULTICATEGORICAL OUTCOMES ----

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/alcoholuse.csv'

# create data frame from github data
alcoholuse <- read.csv(data_url)

# FIT BINARY LOGISTIC MODEL ----

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

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model1)
imputed_vs_observed_plot(model1)
residuals_plot(model1)

# plot individual-level predicted probabilities
bivariate_plot(model1, drinker.1.probability ~ alcage)
bivariate_plot(model1, drinker.1.probability ~ college)
bivariate_plot(model1, drinker.1.probability ~ age)
bivariate_plot(model1, drinker.1.probability ~ male)

# FIT MULTINOMIAL LOGISTIC MODEL ----

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

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)


