# INTERACTION INVOLVING A SUM SCORE

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/pain.csv'

# create data frame from github data
pain <- read.csv(data_url)

# FIT MODEL ----

# interaction with incomplete sum score
model <- rblimp(
  data = pain,
  ordinal = 'dep1:dep7 male pain',
  # fixed = 'male',
  center = 'pain',
  model = '
  depress_sum = dep1:+:dep7;
  disability ~ intercept@b0 depress_sum@b1 male@b2 depress_sum*male@b3 pain;', 
  parameters = '
    icept_female = b0;
    slope_female = b1;
    icept_male = b0 + b2;
    slope_male = b1 + b3;',
  seed = 90291,
  burn = 20000,
  iter = 20000,
  nimps = 20)

# print output
output(model)

# plot parameter distributions
posterior_plot(model,'disability')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
imputation_plot(model)
imputed_vs_observed_plot(model)
residuals_plot(model)
