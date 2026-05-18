# BRIAN NOTES ----
# The .residual behavior with logistic models is incorrect
# No predicted values for logistic outcomes

# LOGISTIC REGRESSION FOR BINARY AND MULTICATEGORICAL OUTCOMES ----

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(ggplot2)
library(patchwork)
library(rblimp)

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/alcohol.csv'

# create data frame from github data
alcohol <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT BINARY LOGISTIC MODEL ----
#------------------------------------------------------------------------------#

# binary logistic regression
mod1 <- rblimp(
  data = alcohol,
  ordinal = 'college male',
  nominal = 'drinker',
  model = 'drinker ~ agetryalc college age male; DEBUG: compact_output', 
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(mod1)

# plot parameter distributions
posterior_plot(model1,'drinker')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
# FIT MULTINOMIAL LOGISTIC MODEL ----
#------------------------------------------------------------------------------#

# multinomial logistic regression
model2 <- rblimp(
  data = alcoholuse,
  nominal = 'drinkingfreq college male',
  # fixed = 'male',
  model = 'drinkfreq ~ agetryalc college age male', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2,'drinkingfreq')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# binary logistic regression
mod3 <- rblimp(
  data = alcohol,
  ordinal = 'college male',
  nominal = 'drinker',
  model = 'drinker ~ agetryalc college age male', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(mod3)

summary(alcohol$agetryalc)

# plot distributions
posterior_plot(mod3)
distribution_plot(mod3)
residuals_plot(mod3)