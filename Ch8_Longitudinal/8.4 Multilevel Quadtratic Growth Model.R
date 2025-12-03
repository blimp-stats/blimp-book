# BRIAN NOTES ----
# simple command behaves incorrectly with quadratic term?

# MULTILEVEL CURVILINEAR GROWTH MODEL

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/trial_stacked.csv'

# create data frame from github data
trial <- read.csv(data_url)

# FIT CURIVLINEAR GROWTH MODEL (COMBINED SPECIFICATION) ----

# combined-model specification: quadratic fixed effect
model1 <- rblimp(
  data = trial,
  clusterid = 'person',
  model = 'severity ~ intercept week week^2 | week',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1)

# combined-model specification: quadratic fixed effect and variance
model2 <- rblimp(
  data = trial,
  clusterid = 'person',
  model = 'severity ~ intercept week week^2 | week week^2',
  seed = 90291,
  burn = 30000,
  iter = 30000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model1)

# plot distributions and residuals
univariate_plot(vars = c('severity.person','severity_on_week.person','severity.residual'), model1)

# plot standardized residuals vs. predicted values
bivariate_plot(severity.residual ~ severity.predicted, standardize = 'y', model = model1)

# plot standardized residuals by time
bivariate_plot(severity.residual ~ week, model1)

# plot predicted values by time
bivariate_plot(severity.predicted ~ week, model1)

# FIT CURIVLINEAR GROWTH MODEL (LATENT SPECIFICATION) ----

# latent variable specification: quadratic fixed effect
model3 <- rblimp(
  data = trial,
  clusterid = 'person',
  latent = 'person = icept linear',
  model = '
    level2:
    intercept -> icept linear;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear week^2;',
  seed = 90291,
  burn = 20000,
  iter = 20000,
  nimps = 20)

# print output
output(model3)

# plot parameter distributions
posterior_plot(model3)

# latent variable specification: quadratic fixed effect and variance
model4 <- rblimp(
  data = trial,
  clusterid = 'person',
  latent = 'person = icept linear quad',
  model = '
    level2:
    intercept -> icept linear quad;
    icept linear quad ~~ icept linear quad;
    level1:
    severity ~ intercept@icept week@linear week^2@quad;',
  seed = 90291,
  burn = 20000,
  iter = 20000,
  nimps = 20)

# print output
output(model4)

# plot parameter distributions
posterior_plot(model4)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model3)

# plot distributions and residuals
univariate_plot(vars = c('icept.latent','linear.latent','severity.residual'), model3)

# plot standardized residuals vs. predicted values
bivariate_plot(severity.residual ~ severity.predicted, standardize = 'y', model = model3)

# plot standardized residuals by time
bivariate_plot(severity.residual ~ week, model3)

# plot predicted values by time
bivariate_plot(severity.predicted ~ week, model3)

# FIT CURIVLINEAR GROWTH MODEL WITH PREDICTORS (COMBINED SPECIFICATION) ----

# combined-model specification
model5 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'person',
  center = 'grandmean = male',
  model = 'severity ~ intercept@icept_d0 week@slp_d0 week^2@quad_d0 drug@icept_diff drug*week@slp_diff drug*week^2@quad_diff male | week',
  parameters = '
    mu3_drug0 = icept_d0 + slp_d0*3 + quad_d0*9;
    mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3 + (quad_d0 + quad_diff)*9;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 20000,
  iter = 20000,
  nimps = 20)

# print output
output(model5)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model5)

# plot distributions and residuals
univariate_plot(vars = c('severity.person','severity_on_week.person','severity.residual'), model5)

# plot standardized residuals vs. predicted values
bivariate_plot(severity.residual ~ severity.predicted, standardize = 'y', model = model5)

# plot standardized residuals by time
bivariate_plot(severity.residual ~ week, model5)

# plot predicted values by time
bivariate_plot(severity.predicted ~ week, model5)

# FIT CURIVLINEAR GROWTH MODEL WITH PREDICTORS (LATENT SPECIFICATION) ----

# latent variable specification
model6 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'person',
  latent = 'person = icept linear;',
  center = 'grandmean = male',
  model = '
    level2:
    icept ~ intercept@icept_d0 drug@icept_diff male;
    linear ~ intercept@slp_d0 drug@slp_diff;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear week^2@quad_d0 week^2*drug@quad_diff;',
  parameters = '
    mu3_drug0 = icept_d0 + slp_d0*3 + quad_d0*9;
    mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3 + (quad_d0 + quad_diff)*9;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 20000,
  iter = 20000,
  nimps = 20)

# print output
output(model6)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model6)

# plot distributions and residuals
univariate_plot(vars = c('icept.latent','linear.latent','severity.residual'), model6)

# plot standardized residuals vs. predicted values
bivariate_plot(severity.residual ~ severity.predicted, standardize = 'y', model = model6)

# plot standardized residuals by time
bivariate_plot(severity.residual ~ week, model6)

# plot predicted values by time
bivariate_plot(severity.predicted ~ week, model6)
