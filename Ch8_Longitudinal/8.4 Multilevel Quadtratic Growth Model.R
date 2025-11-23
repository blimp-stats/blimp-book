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

# FIT CURIVLINEAR GROWTH MODEL ----

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

# plot predicted values by time
bivariate_plot(model3, severity.predicted ~ week, lines = T)

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model3)
imputed_vs_observed_plot(model3)
residuals_plot(model3)

# FIT CURVILINEAR GROWTH MODEL WITH PREDICTORS ----

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
  iter = 20000
)

# print output
output(model5)

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

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model6)
imputed_vs_observed_plot(model6)
residuals_plot(model6)