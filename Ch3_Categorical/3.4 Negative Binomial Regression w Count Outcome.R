# BRIAN NOTES ----
# We need to discuss the .residual behavior with logistic models
# Loess lines look odd because residuals on latent metric do not appear to have a zero mean

# NEGATIVE BINOMIAL REGRESSION FOR A COUNT OUTCOME ----

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/alcoholuse.csv'

# create data frame from github data
alcoholuse <- read.csv(data_url)

# plotting function
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# FIT MODEL ----

# negative binomial regression
model1 <- rblimp(
    data = alcoholuse,
    nominal = 'college male',
    count = 'alcdays',
    # fixed = 'male',
    model = 'alcdays ~ alcage college age male', 
    seed = 90291,
    burn = 10000,
    iter = 10000)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1,'alcdays')

# GRAPHICAL DIAGNOSTICS ----

# multiple imputations for graphical diagnostics 
model2 <- rblimp(
  data = alcoholuse,
  nominal = 'college male',
  count = 'alcdays',
  # fixed = 'male',
  model = 'alcdays ~ alcage college age male', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# plot distributions, observed vs. imputed scores, and residuals
imputation_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)
