# MULTILEVEL MODEL WITH RANDOM SLOPES

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/diary.csv'

# create data frame from github data
diary <- read.csv(data_url)

# FIT EMPTY MODEL FOR LEVEL-1 VARIABLES ----

# empty multivariate model for icc's
model1 <- rblimp(
  data = diary,
  clusterid = 'person',
  ordinal = 'pain',
  model = 'intercept -> posaff pain',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model1)

# FIT MODEL WITH COMBINED MODEL SPECIFICATION ----

# random slope model
model2 <- rblimp(
  data = diary,
  ordinal = 'female',
  clusterid = 'person',
  center = 'groupmean = pain; grandmean = pain.mean stress female',
  model = 'posaff ~ pain pain.mean stress female | pain',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2,'posaff')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model2)

# plot distributions and residuals
univariate_plot(vars = c('posaff.person','posaff_on_pain.person','posaff.residual'), model2)

# plot standardized level-2 residuals vs. level-2 predictors
bivariate_plot(x_vars = c('pain.mean.person','stress','female'), 
               y_vars = c('posaff.person','posaff_on_pain.person'), standardize = 'y', model = model2)

# plot standardized level-1 residuals vs. level-1 predictors
bivariate_plot(posaff.residual ~ pain, standardize = 'y', model = model2)

# FIT MODEL WITH LATENT VARIABLE SPECIFICATION ----

# random slope model
model3 <- rblimp(
  data = diary,
  clusterid = 'person',
  nominal = 'female',
  latent = 'person = ranicept ranslope;',
  center = 'groupmean = pain; grandmean = pain.mean stress female',
  model = '
    level2:
    ranicept ~ intercept stress pain.mean female;
    ranslope ~ intercept;
    ranicept ~~ ranslope;
    level1:
    posaff ~ intercept@ranicept pain@ranslope;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model3)

# plot parameter distributions
posterior_plot(model3,'posaff')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model3)

# plot distributions and residuals
univariate_plot(vars = c('ranicept.latent','ranicept.residual','ranslope.latent','ranslope.residual','posaff.residual'), model3)

# plot standardized level-2 residuals vs. level-2 predictors
bivariate_plot(x_vars = c('pain.mean.person','stress','female'), 
               y_vars = c('ranicept.residual','ranslope.residual'), standardize = 'y', model = model3)

# plot standardized level-1 residuals vs. level-1 predictors
bivariate_plot(posaff.residual ~ pain, standardize = 'y', model = model3)
