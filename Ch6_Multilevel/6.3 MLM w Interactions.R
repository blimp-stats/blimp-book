# MULTILEVEL MODEL WITH RANDOM SLOPES AND INTERACTIONS

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

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

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model1)
imputed_vs_observed_plot(model1)
residuals_plot(model1)

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

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)
