# LATENT VARIABLE INTERACTION WITH BINARY MODERATOR

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/pain.csv'

# create data frame from github data
pain <- read.csv(data_url)

# FIT MODEL ----

# latent interaction with a binary moderator
model <- rblimp(
  data = pain,
  ordinal = 'male pain dep1:dep7 dis1:dis6',
  latent = 'dep disab',
  model = '
      structural: 
      disab ~ dep male dep*male pain;
      measurement: 
      dep@1;
      dep -> dep1@lo1_dep dep2:dep7;
      disab@1;
      disab -> dis1@lo1_dis dis2:dis6; 
      predictors: 
      pain male dep ~~ pain male dep;',
  simple = 'dep | male',
  seed = 90291,
  burn = 50000,
  iter = 50000,
  nimps = 20)

# print output
output(model)

# plot parameter distributions
posterior_plot(model)

# print conditional effects (simple slopes)
simple_plot(disab ~ dep | male, model)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model)
imputed_vs_observed_plot(model)
residuals_plot(model)
