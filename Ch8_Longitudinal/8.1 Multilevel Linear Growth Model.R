# MULTILEVEL LINEAR GROWTH MODEL

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/trial_stacked.csv'

# create data frame from github data
trial <- read.csv(data_url)

# FIT REPEATED MEASURES MODEL ----

# mixed model specification
model1 <- rblimp(
  data = trial,
  nominal = 'week',
  clusterid = 'person',
  model = 'severity ~ intercept week',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot predicted values by time
bivariate_plot(model1, severity.predicted ~ week)

# FIT LINEAR GROWTH MODEL ----

# combined-model specification
model2 <- rblimp(
  data = trial,
  clusterid = 'person',
  model = 'severity ~ intercept week | week;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2)

# latent variable specification
model3 <- rblimp(
  data = trial,
  clusterid = 'person',
  latent = 'person = icept linear',
  model = '
    level2:
    intercept -> icept linear;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear;',
  seed = 90291,
  burn = 20000,
  iter = 20000,
  nimps = 20)

# print output
output(model3)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot predicted values by time
bivariate_plot(model3, severity.predicted ~ week, lines = T)

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model3)
imputed_vs_observed_plot(model3)
residuals_plot(model3)

# FIT LINEAR GROWTH MODEL WITH PREDICTORS ----

# combined-model specification
model4 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'person',
  center = 'grandmean = male',
  model = 'severity ~ intercept@icept_d0 week@slp_d0 drug@icept_diff drug*week@slp_diff male | week',
  simple = 'week | drug',
  parameters = '
    mu3_drug0 = icept_d0 + slp_d0*3;
    mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model4)

# plot parameter distributions
posterior_plot(model4)

# plot conditional growth curves
simple_plot(severity ~ week | drug, model4)

# latent variable specification
model5 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'person',
  center = 'grandmean = male',
  latent = 'person = icept linear;',
  model = '
    level2:
    icept ~ intercept@icept_d0 drug@icept_diff male;
    linear ~ intercept@slp_d0 drug@slp_diff;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear;',
  parameters = '
    mu3_drug0 = icept_d0 + slp_d0*3;
    mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model5)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot predicted values by time
bivariate_plot(model5, severity.predicted ~ week, lines = T)

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model5)
imputed_vs_observed_plot(model5)
residuals_plot(model5)
