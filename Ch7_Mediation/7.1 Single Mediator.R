# SINGLE-MEDIATOR MODEL

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/discrimination.csv'

# create data frame from github data
discrimination <- read.csv(data_url)

# FIT SINGLE MEDIATOR MODEL ----

model <- rblimp(
  data = discrimination,
  nominal = 'female',
  model = '
    apath:
    familism ~ discrim@a female age;
    bpath:
    internalize ~ discrim familism@b female age;',
  parameters = '
    indirect = a*b;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model)

# plot parameter distributions
posterior_plot(model)

# plot distribution of indirect effect
posterior_plot(model, 'indirect')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model)
imputed_vs_observed_plot(model)
residuals_plot(model)
