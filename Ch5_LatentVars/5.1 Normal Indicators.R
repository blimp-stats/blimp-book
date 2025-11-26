# BRIAN NOTES ----
# any way to easily identify columns of iterations matrix? transpose matrix and give same row names as @estimates?
# want to construct wald test of residual correlations among indicators, or all residual correlations

# LATENT VARIABLE WITH NORMAL INDICATORS

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflamm <- read.csv(data_url)

# FIT STRUCTURAL REGRESSION MODEL ----

# first loading fixed to 1
model1 <- rblimp(
  data = inflamm,
  latent = 'inflam',
  model = '
    structural:
    yjt(dpdd - 6) ~ inflam;
    measurement:
    inflam -> inflam_crp inflam_il6 inflam_tnf inflam_ifn;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1)

# factor variance fixed to 1
model2 <- rblimp(
  data = inflamm,
  latent = 'inflam',
  model = '
    structural:
    yjt(dpdd - 6) ~ inflam;
    measurement:
    inflam@1;
    inflam -> inflam_crp@lo1 inflam_il6 inflam_tnf inflam_ifn;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2)

# FIT MODEL WITH COVARIATES ----

# correlate latent and manifest predictors
model3 <- rblimp(
  data = inflamm,
  ordinal = 'female els',
  latent = 'inflam',
  # fixed = 'female age',
  center = 'age',
  model = '
    structural:
    yjt(dpdd - 6) ~ inflam female els age;
    measurement:
    inflam@1;
    inflam -> inflam_crp@lo1 inflam_il6 inflam_tnf inflam_ifn;
    predictors:
    inflam female els age ~~ inflam female els age;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model3)

# plot parameter distributions
posterior_plot(model3)

# link latent and manifest predictors with regression
model4 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  latent = 'inflam',
  # fixed = 'female age',
  center = 'age',
  model = '
    structural:
    yjt(dpdd - 6) ~ inflam female els age;
    measurement:
    inflam@1;
    inflam -> inflam_crp@lo1 inflam_il6 inflam_tnf inflam_ifn;
    predictors:
    inflam ~ female els age;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model4)

# plot parameter distributions
posterior_plot(model4)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model3)

# plot standardized residuals vs. predicted values
bivariate_plot(inflam_crp.residual ~ inflam_crp.predicted, standardize = 'y', model = model3)
bivariate_plot(inflam_il6.residual ~ inflam_il6.predicted, standardize = 'y', model = model3)
bivariate_plot(inflam_tnf.residual ~ inflam_tnf.predicted, standardize = 'y', model = model3)
bivariate_plot(inflam_ifn.residual ~ inflam_ifn.predicted, standardize = 'y', model = model3)

# plot standardized residuals vs. numeric predictors (latent variable scores)
indicator_residuals <- paste0(c('inflam_crp','inflam_il6','inflam_tnf','inflam_ifn'),'.residual')
bivariate_plot(x_vars = 'inflam.latent', y_vars = indicator_residuals, model = model3, standardize = 'both')

# plot pairs of indicator residuals
bivariate_plot(vars = indicator_residuals, model = model3, poly_degree = 1, standardize = 'both')
