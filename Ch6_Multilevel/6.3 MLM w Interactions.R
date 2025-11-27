# MULTILEVEL MODEL WITH RANDOM SLOPES AND INTERACTIONS

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/diary.csv'

# create data frame from github data
diary <- read.csv(data_url)

# FIT MODEL WITH COMBINED MODEL SPECIFICATION ----

# cross-level and between-level interactions
model1 <- rblimp(
  data = diary,
  clusterid = 'person',
  center = 'groupmean = pain; grandmean = pain.mean stress female',
  model = 'posaff ~ pain pain.mean stress female pain*stress pain.mean*stress | pain',
  simple = 'pain | stress; pain.mean | stress;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1,'posaff')

# plot conditional effects
simple_plot(posaff ~ pain | stress, model1)
simple_plot(posaff ~ pain.mean[person] | stress, model1)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model1)

# plot distributions and residuals
univariate_plot(vars = c('posaff.person','posaff_on_pain.person','posaff.residual'), model1)

# plot standardized level-2 residuals vs. level-2 predictors
bivariate_plot(x_vars = c('pain.mean.person','stress','female'), 
               y_vars = c('posaff.person','posaff_on_pain.person'), standardize = 'y', model = model1)

# plot standardized level-1 residuals vs. level-1 predictors
bivariate_plot(posaff.residual ~ pain, standardize = 'y', model = model1)

# FIT MODEL WITH LATENT VARIABLE SPECIFICATION ----

# cross-level and between-level interactions
model2 <- rblimp(
  data = diary,
  clusterid = 'person',
  nominal = 'female',
  latent = 'person = ranicept ranslope;',
  center = 'groupmean = pain; grandmean = pain.mean stress female',
  model = '
    level2:
    ranicept ~ intercept stress pain.mean female stress*pain.mean;
    ranslope ~ intercept@mu_ranslp stress@slp_mod;
    ranicept ~~ ranslope;
    level1:
    posaff ~ intercept@ranicept pain@ranslope;
    predictor:
    stress ~ pain.mean female;',
  parameter = '
    pain_at_low_stress = mu_ranslp - slp_mod*sqrt(stress.totalvar);
    pain_at_mean_stress = mu_ranslp;
    pain_at_high_stress = mu_ranslp + slp_mod*sqrt(stress.totalvar);',
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
univariate_plot(vars = c('ranicept.latent','ranicept.residual','ranslope.latent','ranslope.residual','posaff.residual'), model2)

# plot standardized level-2 residuals vs. level-2 predictors
bivariate_plot(x_vars = c('pain.mean.person','stress','female'), 
               y_vars = c('ranicept.residual','ranslope.residual'), standardize = 'y', model = model2)

# plot standardized level-1 residuals vs. level-1 predictors
bivariate_plot(posaff.residual ~ pain, standardize = 'y', model = model2)
