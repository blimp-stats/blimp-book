# REPEATED MEASURES ANALYSIS

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/trial_stacked.csv'

# create data frame from github data
trial <- read.csv(data_url)

# FIT REPEATED MEASURES MODEL ----

# outcome varying by time
model1 <- rblimp(
  data = trial,
  nominal = 'week',
  clusterid = 'person',
  # fixed = 'week drug',
  model = 'severity ~ intercept week',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot predicted values by time
bivariate_plot(severity.predicted ~ week, model1)
bivariate_plot(severity.predicted ~ week, model = model1, discrete_x = 'week')


# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model1)
imputed_vs_observed_plot(model1)
residuals_plot(model1)

# FIT REPEATED MEASURES MODEL WITH GROUP-BY-TIME INTERACTION ----

# outcome varying by time and group
model2 <- rblimp(
  data = trial,
  ordinal = 'drug',
  nominal = 'week',
  clusterid = 'person',
  # fixed = 'week drug',
  model = 'severity ~ week drug week*drug',
  simple = 'drug | week',
  waldtest = 'severity ~ week drug',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2)

# plot conditional effects
simple_plot(severity ~ drug | week.1 + week.2 + week.3, model2)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)

