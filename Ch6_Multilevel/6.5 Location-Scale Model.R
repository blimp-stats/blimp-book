# MULTILEVEL MODEL WITH HETEROGENEOUS WITHIN-CLUSTER VARIATION

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

# FIT MODEL ----

# location-scale model
model <- rblimp(
  data = diary,
  clusterid = 'person',
  nominal = 'female',
  latent = 'person = ranicept ranslope logvar;',
  center = 'groupmean = pain; grandmean = pain.mean stress female',
  model = '
    level2:
    ranicept ~ intercept pain.mean stress female;
    ranslope ~ intercept stress;
    ranicept ~~ ranslope;
    level1:
    posaff ~ intercept@ranicept pain@ranslope;
    scale:
    logvar ~ intercept stress;
    logvar ~~ ranicept ranslope;
    var(posaff) ~ intercept@logvar pain;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)

# print output
output(model)

# plot parameter distributions
posterior_plot(model,'posaff')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model)
imputed_vs_observed_plot(model)
residuals_plot(model)