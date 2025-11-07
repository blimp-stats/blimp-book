# YEO-JOHNSON NORMALIZING TRANSFORMATION FOR NONNORMAL CONTINUOUS VARIABLES

# LOAD R PACKAGES ----

library(rblimp)
library(ggplot2)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflamm <- read.csv(data_url)

# plotting function
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# FIT LINEAR REGRESSION MODEL ----

# raw score predictors
model1 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  model = 'dpdd ~ female els age inflam_sum', 
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(model1)

# centered predictors
model2 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  center = 'female els age inflam_sum',
  model = 'dpdd ~ female els age inflam_sum', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 100)

output(model2)

model2 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  center = 'female els age inflam_sum',
  model = '
  inflam_sum ~ female els age;
  dpdd ~ female els age inflam_sum', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 100)

output(model2)

# GRAPHICAL DIAGNOSTICS ----

# inspect contents of imputed data
names(model2@imputations[[1]])

# plot distributions
plot_imputations(model2, var = 'dpdd')
plot_imputations(model2, var = c('dpdd','dpdd.residual'))
plot_imputations(model2)

# plot observed vs. imputed scores
plot_imputed_vs_observed(model2, var = 'dpdd')
plot_imputed_vs_observed(model2, var = c('dpdd','dpdd.residual','age'))
plot_imputed_vs_observed(model2)

# plot residuals
plot_residuals(model2)
# residual_plot(model2, 'dpdd')



