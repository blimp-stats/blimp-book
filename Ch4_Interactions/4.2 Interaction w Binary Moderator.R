# BRIAN NOTES ----
# Any chance of allowing simple to work for nominal focals with only two categories?

# INTERACTION INVOLVING CATEGORICAL PREDICTORS

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflamm <- read.csv(data_url)

# FIT MODEL ASSUMING NORMALITY ----

# multiple imputations for graphical diagnostics 
model <- rblimp(
  data = inflamm,
  # ordinal = 'els',
  nominal = 'female',
  center = 'age',
  # fixed = 'age female',
  model = 'yjt(inflam_sum) ~ intercept@b0 els@b1 female@b2  female*els@b3 age', 
  simple = 'els | female',
  parameters = '
    mu_fem0els0 = b0;
    mu_fem0els1 = b0 + b1;
    mu_fem1els0 = b0 + b2;
    mu_fem1els1 = b0 + b1 + b2 + b3;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model)

# plot parameter distributions
posterior_plot(model,'inflam_sum')

# plot conditional effects
simple_plot(yjt(inflam_sum) ~ els | female.1, model)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model)
imputed_vs_observed_plot(model)
residuals_plot(model)
