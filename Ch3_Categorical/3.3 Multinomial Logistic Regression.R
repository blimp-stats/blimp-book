# LOGISTIC REGRESSION FOR MULTICATEGORICAL OUTCOMES

# plotting functions
# source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source("/Users/craig/Documents/Claude/Projects/Blimp Book/rblimp_cleaned_functions.R")

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(ggplot2)
library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/alcohol.csv'

# create data frame from github data
alcohol <- read.csv(data_url)

#------------------------------------------------------------------------------#
# MULTINOMIAL LOGISTIC MODEL ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = alcohol,                                # R data frame
  ordinal = 'college male',                      # binary and ordinal variables
  nominal = 'drinkfreq',                         # nominal variables (auto dummy coded)
  center = 'agetryalc age',                      # center predictors
  model = 'drinkfreq ~ agetryalc college age male', # regression model
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod1)                                     # print output
posterior_plot(mod1, 'drinkfreq')                # plot parameter distributions

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = alcohol,                                # R data frame
  ordinal = 'college male',                      # binary and ordinal variables
  nominal = 'drinkfreq',                         # nominal variables (auto dummy coded)
  center = 'agetryalc age',                      # center predictors
  model = 'drinkfreq ~ agetryalc college age male', # regression model
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod2)                                     # print output

distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot binned residuals