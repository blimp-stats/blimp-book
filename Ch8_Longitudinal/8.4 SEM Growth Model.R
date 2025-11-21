# SEM GROWTH MODELS

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/trial_wide.csv'

# create data frame from github data
trial <- read.csv(data_url)

# FIT LINEAR GROWTH MODEL ----

# linear growth model
model1 <- rblimp(
  data = trial,
  latent = 'icept linear',
  model = '
    structural:
    intercept -> icept linear;
    icept ~~ linear;
    measurement:
    icept -> severity0@1 severity1@1 severity2@1 severity3@1; 
    linear -> severity0@0 severity1@1 severity2@2 severity3@3; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    severity0:severity3@res;',
  seed = 90291,
  burn = 30000,
  iter = 30000,
  nimps = 20) 

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model1)
imputed_vs_observed_plot(model1)
residuals_plot(model1)

# FIT LINEAR GROWTH MODEL WITH PREDICTORS ----

# linear growth model with predictors
model2 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  latent = 'icept linear', 
  center = 'male',
  model = '
    structural:
    icept ~ intercept@icept_d0 drug@icept_diff male;
    linear ~ intercept@slp_d0 drug@slp_diff;
    icept ~~ linear;
    measurement:
    icept -> severity0@1 severity1@1 severity2@1 severity3@1; 
    linear -> severity0@0 severity1@1 severity2@2 severity3@3; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    severity0:severity3@res;',
  parameters = '
  mu3_drug0 = icept_d0 + slp_d0*3;
  mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3;
  mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 30000, 
  iter = 30000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)

# FIT CURIVLINEAR GROWTH MODEL ----

# quadratic fixed effect
model3 <- rblimp(
  data = trial,
  latent = 'icept linear quad',
  model = '
    structural:
    intercept -> icept linear quad;
    quad ~~ quad@.001;
    icept ~~ linear;
    measurement:
    icept -> severity0@1 severity1@1 severity2@1 severity3@1; 
    linear -> severity0@0 severity1@1 severity2@2 severity3@3; 
    quad -> severity0@0 severity1@1 severity2@4 severity3@9; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    severity0:severity3@res;',
  seed = 90291, 
  burn = 30000,
  iter = 30000,
  nimps = 20) 

# print output
output(model3)

# plot parameter distributions
posterior_plot(model3)

# quadratic fixed effect and variance
model4 <- rblimp(
  data = trial,
  latent = 'icept linear quad',
  model = '
    structural:
    intercept -> icept linear quad;
    icept linear quad ~~ icept linear quad;
    measurement:
    icept -> severity0@1 severity1@1 severity2@1 severity3@1; 
    linear -> severity0@0 severity1@1 severity2@2 severity3@3; 
    quad -> severity0@0 severity1@1 severity2@4 severity3@9; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    severity0:severity3@res;',
  seed = 90291, 
  burn = 30000, 
  iter = 30000,
  nimps = 20)

# print output
output(model4)

# plot parameter distributions
posterior_plot(model4)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model4)
imputed_vs_observed_plot(model4)
residuals_plot(model4)

# FIT CURVILINEAR GROWTH MODEL WITH PREDICTORS ----

# quadratic fixed effect with predictors
model5 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  latent = 'icept linear quad', 
  center = 'male',
  model = '
    structural:
    icept ~ intercept@icept_d0 drug@icept_diff male;
    linear ~ intercept@slp_d0 drug@slp_diff;
    quad ~ intercept@quad_d0 drug@quad_diff;
    quad ~~ quad@.001;
    icept ~~ linear;
    measurement:
    icept -> severity0@1 severity1@1 severity2@1 severity3@1; 
    linear -> severity0@0 severity1@1 severity2@2 severity3@3; 
    quad -> severity0@0 severity1@1 severity2@4 severity3@9; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    severity0:severity3@res;',
  parameters = '
    mu3_drug0 = icept_d0 + slp_d0*3 + quad_d0*9;
    mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3 + (quad_d0 + quad_diff)*9;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 50000, 
  iter = 50000,
  nimps = 20)

# print output
output(model5)

# plot parameter distributions
posterior_plot(model5)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model5)
imputed_vs_observed_plot(model5)
residuals_plot(model5)


