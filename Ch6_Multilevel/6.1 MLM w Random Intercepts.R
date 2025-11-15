# RANDOM INTERCEPT REGRESSION

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/problemsolving2level.csv'

# create data frame from github data
probsolve <- read.csv(data_url)

# FIT EMPTY MODEL FOR LEVEL-1 VARIABLES ----

# empty multivariate model for icc's
model1 <- rblimp(
  data = probsolve,
  clusterid = 'school',
  ordinal = 'frlunch',
  model = 'intercept -> probsolvpost probsolvpre stanmath frlunch',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model1)

# FIT MODEL WITH COMBINED MODEL SPECIFICATION ----

# mixed model specification
model2 <- rblimp(
    data = probsolve,
    clusterid = 'school',
    nominal = 'condition frlunch',
    center = 'groupmean = probsolvpre stanmath frlunch',
    # fixed = 'probsolve1 condition',
    model = 'probsolvpost ~ intercept@mu0 probsolvpre stanmath frlunch probsolvpre.mean stanmath.mean frlunch.mean condition@diff', 
    parameters = 'd = diff / sqrt(probsolvpost.totalvar)',
    seed = 90291,
    burn = 10000,
    iter = 10000,
    nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2,'probsolvpost')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
imputation_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)

# FIT MODEL WITH LATENT VARIABLE SPECIFICATION ----

# random intercept as level-2 latent variable
model2 <- rblimp(
  data = probsolve,
  clusterid = 'school',
  nominal = 'condition frlunch',
  latent = 'school = ranicept',
  # fixed = 'probsolve1 condition',
  center = 'groupmean = probsolvpre stanmath frlunch',
  model = '
    ranicept ~ probsolvpre.mean stanmath.mean frlunch.mean condition@diff;
    probsolvpost ~ intercept@ranicept probsolvpre stanmath frlunch;',
  parameters = '
    d = diff / sqrt(probsolvpost.totalvar + ranicept.totalvar);
    rsq_total = ranicept.coefvar / (ranicept.totalvar + probsolvpost.totalvar);
    rsq_between = ranicept.coefvar / ranicept.totalvar;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(model2)

# NEED TO REVIST TOTAL VARIANCE AND ADD EFFECT SIZES ----
