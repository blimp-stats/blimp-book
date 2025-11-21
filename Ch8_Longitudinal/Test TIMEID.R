# TEST TIMEID

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/problemsolving3level.csv'

# create data frame from github data
probsolve <- read.csv(data_url)

# FIT MODEL ----

# latent variable specification
model <- rblimp(
  data = probsolve,
  clusterid = 'student; timeid: month7; dropout: dropout = probsolve;',
  ordinal = 'frlunch condition',
  latent = 'student = ranicept ranslope',
  fixed = 'month7',
  model = ' 
    level2:
    ranicept ~ intercept stanmath frlunch;
    ranslope ~ intercept;
    ranicept ~~ ranslope;
    level1:
    probsolve ~ intercept@ranicept month7@ranslope efficacy;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

output(model)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot predicted values by time
bivariate_plot(model2, probsolve.predicted ~ month7, lines = T)

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)
