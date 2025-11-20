# BRIAN NOTES ----
# simple command behaves incorrectly with quadratic term

# MULTILEVEL GROWTH MODEL

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

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
bivariate_plot(model1, severity.predicted ~ week, lines = T)

# FIT LINEAR GROWTH MODEL WITH MIXED MODEL SPECIFICATION ----

# linear growth model
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

# linear growth model with predictors
model3 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'person',
  center = 'grandmean = male',
  model = 'severity ~ intercept@b0 week@b1 drug@b01 drug*week@b11 male | week;',
  simple = 'week | drug',
  parameters = '
    mu3_drug0 = b0 + b1*3;
    mu3_drug1 = (b0 + b01) + (b1 + b11)*3;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model3)

# plot parameter distributions
posterior_plot(model3)

# plot conditional growth curves
simple_plot(severity ~ week | drug, model3)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot predicted values by time
bivariate_plot(model3, severity.predicted ~ week, lines = T)

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model3)
imputed_vs_observed_plot(model3)
residuals_plot(model3)

# FIT QUADRATIC GROWTH MODEL WITH MIXED MODEL SPECIFICATION ----

# quadratic fixed effect but no quadratic variance
model4 <- rblimp(
  data = trial,
  clusterid = 'person',
  model = 'severity ~ intercept week week^2 | week;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model4)

# quadratic fixed effect and quadratic variance
model5 <- rblimp(
  data = trial,
  clusterid = 'person',
  model = 'severity ~ intercept week week^2 | week week^2;',
  seed = 90291,
  burn = 30000,
  iter = 30000,
  nimps = 20)

# print output
output(model5)

# quadratic fixed effect with predictor variables
model6 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'person',
  center = 'grandmean = male',
  model = 'severity ~ intercept@b0 week@b1 week^2@b2 drug@b01 drug*week@b11 drug*week^2@b21 male | week',
  parameters = '
    mu3_drug0 = b0 + b1*3 + b2*9;
    mu3_drug1 = (b0 + b01) + (b1 + b11)*3 + (b2 + b21)*9;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 20000,
  iter = 20000
)

# print output
output(model6)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot predicted values by time
bivariate_plot(model4, severity.predicted ~ week, lines = T)

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model4)
imputed_vs_observed_plot(model4)
residuals_plot(model4)

# FIT LINEAR GROWTH MODEL WITH LATENT VARIABLE SPECIFICATION ----

# linear growth model
model7 <- rblimp(
  data = trial,
  clusterid = 'person',
  latent = 'person = icept linear;',
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
output(model7)

# linear growth model with predictors
model8 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'person',
  center = 'grandmean = male',
  latent = 'person = icept linear;',
  model = '
    level2:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear;',
  parameters = '
    mu3_drug0 = b0 + b1*3;
    mu3_drug1 = (b0 + b01) + (b1 + b11)*3;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model8)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot predicted values by time
bivariate_plot(model7, severity.predicted ~ week, lines = T)

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model7)
imputed_vs_observed_plot(model7)
residuals_plot(model7)

# FIT QUADRATIC GROWTH MODEL WITH LATENT VARIABLE SPECIFICATION ----

# quadratic fixed effect but no quadratic variance
model9 <- rblimp(
  data = trial,
  clusterid = 'person',
  latent = 'person = icept linear;',
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
output(model9)

# quadratic fixed effect and quadratic variance
model10 <- rblimp(
  data = trial,
  clusterid = 'person',
  latent = 'person = icept linear quad;',
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
output(model10)

# quadratic fixed effect with predictors
model11 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'person',
  latent = 'person = icept linear;',
  center = 'grandmean = male',
  model = '
    level2:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear week^2@b2 week^2*drug@b21;',
  parameters = '
    mu3_drug0 = b0 + b1*3 + b2*9;
    mu3_drug1 = (b0 + b01) + (b1 + b11)*3 + (b2 + b21)*9;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 20000,
  iter = 20000,
  nimps = 20)

# print output
output(model11)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot predicted values by time
bivariate_plot(model9, severity.predicted ~ week, lines = T)

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model9)
imputed_vs_observed_plot(model9)
residuals_plot(model9)