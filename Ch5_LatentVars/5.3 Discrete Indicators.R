# BRIAN NOTES ----
# how to access residual correlation draws for Wald test
# function to print standardized estimates? e.g., standarized(model)

# MODERATED NONLINEAR FACTOR ANALYSIS (MNLFA)

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/adhd.csv'

# create data frame from github data
adhd <- read.csv(data_url)

# FIT CFA MODEL ----

# cfa with factor mean and variance fixed at 0 and 1
model1 <- rblimp(
  data = adhd,
  ordinal = 'swan1:swan9',
  latent = 'attent',
  model = '
    attent@1;
    attent -> swan1@lo1 swan2:swan9;',
  seed = 90291,
  burn = 25000,
  iter = 25000,
  nimps = 20)

# print output
output(model1)

# print standardized estimates
model1@estimates[grep("standardized|Cor", rownames(model1@estimates)), ]

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# latent variable scores vs. indicator residuals
bivariate_plot(x_vars = 'attent.latent', y_vars = paste0('swan',1:9,'.residual'), model = model1, standardize = 'both')

# plot pairs of indicator residuals
bivariate_plot(vars = paste0('swan',1:9,'.residual'), model = model1)

# plot large residual correlations
bivariate_plot(swan2.residual ~ swan7.residual, model1)
bivariate_plot(swan2.residual ~ swan8.residual, model1)
bivariate_plot(swan7.residual ~ swan9.residual, model1)

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model1)
imputed_vs_observed_plot(model1)
residuals_plot(model1)

row.names(model1@estimates)

# FIT CFA MODEL WITH CORRELATED RESIDUALS ----

# cfa with factor mean and variance fixed at 0 and 1
model2 <- rblimp(
  data = adhd,
  ordinal = 'swan1:swan9',
  latent = 'attent',
  model = '
    attent@1;
    attent -> swan1@lo1 swan2:swan9;
    swan2 ~~ swan8;
    swan7 ~~ swan9;',
  seed = 90291,
  burn = 25000,
  iter = 25000,
  nimps = 20)

# print output
output(model2)

# print standardized estimates
model2@estimates[grep("standardized|Cor", rownames(model2@estimates)), ]


