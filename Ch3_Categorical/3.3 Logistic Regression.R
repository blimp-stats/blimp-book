# BRIAN NOTES ----
# We need to discuss the .residual behavior with logistic models
# Loess lines look odd because residuals on latent metric do not appear to have a zero mean

# LOGISTIC REGRESSION FOR BINARY AND MULTICATEGORICAL OUTCOMES ----

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/alcoholuse.csv'

# create data frame from github data
alcoholuse <- read.csv(data_url)

# plotting function
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# FIT MODELS ----

# binary logistic regression
model1 <- rblimp(
  data = alcoholuse,
  nominal = 'drinker college male',
  # fixed = 'male',
  model = 'drinker ~ alcage college age male', 
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1,'drinker')

# multinomial logistic regression
model2 <- rblimp(
  data = alcoholuse,
  nominal = 'drinkingfreq college male',
  # fixed = 'male',
  model = 'drinkingfreq ~ alcage college age male', 
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2,'drinkingfreq')

# GRAPHICAL DIAGNOSTICS ----

# binary logistic with multiple imputations for graphical diagnostics 
model3 <- rblimp(
  data = alcoholuse,
  nominal = 'drinker college male',
  # fixed = 'male',
  model = 'drinker ~ alcage college age male', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model3)

# plot individual-level predicted probabilities
bivariate_plot(model3, drinker.1.probability ~ alcage)
bivariate_plot(model3, drinker.1.probability ~ college)
bivariate_plot(model3, drinker.1.probability ~ age)
bivariate_plot(model3, drinker.1.probability ~ male)

# plot distributions, observed vs. imputed scores, and residuals
imputation_plot(model3)
imputed_vs_observed_plot(model3)
residuals_plot(model3)

# multinomial logistic with multiple imputations for graphical diagnostics 
model4 <- rblimp(
  data = alcoholuse,
  nominal = 'drinkingfreq college male',
  # fixed = 'male',
  model = 'drinkingfreq ~ alcage college age male', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model4)

names(model4@imputations[[1]])

# plot individual-level predicted probabilities
bivariate_plot(model4, drinkingfreq.1.probability ~ alcage)
bivariate_plot(model4, drinkingfreq.1.probability ~ college)
bivariate_plot(model4, drinkingfreq.1.probability ~ age)
bivariate_plot(model4, drinkingfreq.1.probability ~ male)
bivariate_plot(model4, drinkingfreq.2.probability ~ alcage)
bivariate_plot(model4, drinkingfreq.2.probability ~ college)
bivariate_plot(model4, drinkingfreq.2.probability ~ age)
bivariate_plot(model4, drinkingfreq.2.probability ~ male)

# plot distributions, observed vs. imputed scores, and residuals
imputation_plot(model4)
imputed_vs_observed_plot(model4)
residuals_plot(model4)


