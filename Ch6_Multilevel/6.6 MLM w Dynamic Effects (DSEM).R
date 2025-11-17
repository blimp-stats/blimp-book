# MULTILEVEL MODEL WITH LAGGED (DYNAMIC) EFFECTS

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)
set_blimp('/applications/blimp/blimp-nightly')

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/diary.csv'

# create data frame from github data
diary <- read.csv(data_url)

# FIT MODEL ----

# dsem with lagged predictors
model <- rblimp(
  data = diary,
  clusterid = 'person; timeid: day;',
  latent = 'person = pain_ranicept posaff_ranicept;',
  model = '
        pain ~ intercept@pain_ranicept (pain.lag - pain_ranicept) (posaff.lag - posaff_ranicept);
        posaff ~ intercept@posaff_ranicept (posaff.lag - posaff_ranicept) (pain.lag - pain_ranicept);
        pain ~~ posaff;
        intercept -> pain_ranicept posaff_ranicept;
        pain_ranicept ~~ posaff_ranicept;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model)

# plot parameter distributions
posterior_plot(model,'posaff')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model)
imputed_vs_observed_plot(model)
residuals_plot(model)
