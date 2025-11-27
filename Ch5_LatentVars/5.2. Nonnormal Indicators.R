#  BRIAN NOTES ----
# labels on quadratic factor loadings require (), wald test is omitted without message otherwise

# LATENT VARIABLE WITH NONNONORMAL INDICATORS

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

# FIT MODEL WITH NORMALIZED INDICATORS ----

# normalized indicators
model <- rblimp(
  data = inflamm,
  latent = 'inflam',
  model = '
    inflam@1;
    inflam -> yjt(inflam_crp)@lo1 yjt(inflam_il6) inflam_tnf yjt(inflam_ifn);',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions and residuals
indicators <- c('inflam_crp','inflam_il6','inflam_tnf','inflam_ifn')
normindicators <- paste0(c('inflam_crp','inflam_il6','inflam_ifn'),'.yjt')
residuals <- paste0(c('inflam_crp.yjt','inflam_il6.yjt','inflam_tnf','inflam_ifn.yjt'),'.residual')
univariate_plot(vars = c(indicators,normindicators, residuals), model)

# plot standardized residuals vs. predicted values
bivariate_plot(inflam_crp.yjt.residual ~ inflam_crp.yjt.predicted, standardize = 'y', model = model)
bivariate_plot(inflam_il6.yjt.residual ~ inflam_il6.yjt.predicted, standardize = 'y', model = model)
bivariate_plot(inflam_tnf.residual ~ inflam_tnf.predicted, standardize = 'y', model = model)
bivariate_plot(inflam_ifn.yjt.residual ~ inflam_ifn.yjt.predicted, standardize = 'y', model = model)

# plot standardized residuals vs. latent variable scores
bivariate_plot(x_vars = 'inflam.latent', y_vars = residuals, model = model, standardize = 'both')

# plot pairs of indicator residuals
bivariate_plot(vars = residuals, model = model, poly_degree = 1, standardize = 'both')

