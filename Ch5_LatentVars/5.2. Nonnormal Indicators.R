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

# FIT NONLINEAR FACTOR MODEL ----

# factor mean and variance fixed at 0 and 1
model1 <- rblimp(
  data = inflamm,
  latent = 'inflam',
  model = '
    inflam@1;
    inflam_crp ~ inflam (inflam^2)@quad1;
    inflam_il6 ~ inflam (inflam^2)@quad2;
    inflam_tnf ~ inflam;
    inflam_ifn ~ inflam (inflam^2)@quad3;',
  waldtest = 'quad1:quad3 = 0',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model1)

# print standardized estimates
model1@estimates[grep("standardized|Cor", rownames(model1@estimates)), ]

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot raw residuals
indicators <- c('inflam_crp','inflam_il6','inflam_tnf','inflam_ifn')
residuals <- paste0(c('inflam_crp','inflam_il6','inflam_tnf','inflam_ifn'),'.residual')
univariate_plot(vars = c(indicators,residuals), model1)

# plot standardized residuals vs. predicted values
bivariate_plot(inflam_crp.residual ~ inflam_crp.predicted, standardize = 'y', model = model1)
bivariate_plot(inflam_il6.residual ~ inflam_il6.predicted, standardize = 'y', model = model1)
bivariate_plot(inflam_tnf.residual ~ inflam_tnf.predicted, standardize = 'y', model = model1)
bivariate_plot(inflam_ifn.residual ~ inflam_ifn.predicted, standardize = 'y', model = model1)

# plot standardized residuals vs. latent variable scores
bivariate_plot(x_vars = 'inflam.latent', y_vars = residuals, model = model1, standardize = 'both')

# plot pairs of indicator residuals
bivariate_plot(vars = residuals, model = model1, poly_degree = 1, standardize = 'both')

# FIT MODEL WITH NORMALIZED INDICATORS ----

# normalized indicators
model2 <- rblimp(
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
output(model2)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions and residuals
indicators <- c('inflam_crp','inflam_il6','inflam_tnf','inflam_ifn')
normindicators <- paste0(c('inflam_crp','inflam_il6','inflam_ifn'),'.yjt')
residuals <- paste0(c('inflam_crp.yjt','inflam_il6.yjt','inflam_tnf','inflam_ifn.yjt'),'.residual')
univariate_plot(vars = c(indicators,normindicators, residuals), model2)

# plot standardized residuals vs. predicted values
bivariate_plot(inflam_crp.yjt.residual ~ inflam_crp.yjt.predicted, standardize = 'y', model = model2)
bivariate_plot(inflam_il6.yjt.residual ~ inflam_il6.yjt.predicted, standardize = 'y', model = model2)
bivariate_plot(inflam_tnf.residual ~ inflam_tnf.predicted, standardize = 'y', model = model2)
bivariate_plot(inflam_ifn.yjt.residual ~ inflam_ifn.yjt.predicted, standardize = 'y', model = model2)

# plot standardized residuals vs. latent variable scores
bivariate_plot(x_vars = 'inflam.latent', y_vars = residuals, model = model2, standardize = 'both')

# plot pairs of indicator residuals
bivariate_plot(vars = residuals, model = model2, poly_degree = 1, standardize = 'both')

