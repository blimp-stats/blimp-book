# LATENT VARIABLE INTERACTION WITH A BINARY MODERATOR

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/pain.csv'

# create data frame from github data
pain <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT MODEL ----
#------------------------------------------------------------------------------#

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

# print standardized estimates
model@estimates[grep("standardized|Cor", rownames(model@estimates)), ]

# plot parameter distributions
posterior_plot(model)

# print conditional effects (simple slopes)
simple_plot(disab ~ dep | male, model)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model)

# plot distributions and residuals
residuals_dep <- paste0('dep',1:7,'.residual')
residuals_dis <- paste0('dis',1:6,'.residual')
univariate_plot(vars = c(residuals_dep,residuals_dis), model)

# plot standardized residuals vs. latent variable scores
bivariate_plot(x_vars = 'dep.latent', y_vars = residuals_dep, model = model, standardize = 'both')
bivariate_plot(x_vars = 'disab.latent', y_vars = residuals_dis, model = model, standardize = 'both')

# plot pairs of indicator residuals
bivariate_plot(vars = c(residuals_dep,residuals_dis), model = model, poly_degree = 1, standardize = 'both')
