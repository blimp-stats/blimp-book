# BRIAN NOTES ----
# any way to easily identify columns of iterations matrix? transpose matrix and give same row names as @estimates?
# want to construct wald test of residual correlations among indicators, or all residual correlations

# LATENT VARIABLE WITH NORMAL INDICATORS

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflamm <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT FACTOR MODEL ----
#------------------------------------------------------------------------------#

# factor mean fixed at 0 and first loading fixed to 1
model1 <- rblimp(
  data = inflamm,
  latent = 'inflam',
  model = 'inflam -> inflam_crp inflam_il6 inflam_tnf inflam_ifn',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model1)

# factor mean and variance fixed at 0 and 1
model2 <- rblimp(
  data = inflamm,
  latent = 'inflam',
  model = '
    inflam@1;
    inflam -> inflam_crp@lo1 inflam_il6 inflam_tnf inflam_ifn;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# print standardized estimates
model2@estimates[grep("standardized|Cor", rownames(model2@estimates)), ]

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot distributions and residuals
indicators <- c('inflam_crp','inflam_il6','inflam_tnf','inflam_ifn')
residuals <- paste0(c('inflam_crp','inflam_il6','inflam_tnf','inflam_ifn'),'.residual')
univariate_plot(vars = c(indicators,residuals), model = model2)

# plot standardized residuals vs. predicted values
bivariate_plot(inflam_crp.residual ~ inflam_crp.predicted, standardize = 'y', model = model2)
bivariate_plot(inflam_il6.residual ~ inflam_il6.predicted, standardize = 'y', model = model2)
bivariate_plot(inflam_tnf.residual ~ inflam_tnf.predicted, standardize = 'y', model = model2)
bivariate_plot(inflam_ifn.residual ~ inflam_ifn.predicted, standardize = 'y', model = model2)

# plot standardized residuals vs. latent variable scores
bivariate_plot(x_vars = 'inflam.latent', y_vars = residuals, model = model2, standardize = 'both')

# plot pairs of indicator residuals
bivariate_plot(vars = residuals, model = model2, poly_degree = 1, standardize = 'both')
