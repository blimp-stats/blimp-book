# CURVILINEAR REGRESSION

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/math.csv'

# create data frame from github data
math <- read.csv(data_url)

#------------------------------------------------------------------------------#
# CURVILINEAR MODEL ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
    data = math,                                 # R data frame
    ordinal = 'frlunch male',                    # binary and ordinal variables
    center = 'anxiety mathpre',                  # center predictors
    model = 'mathpost ~ anxiety anxiety^2 mathpre frlunch male',  # quadratic term
    seed = 90291,                                # random number seed
    burn = 10000,                                # warm-up iterations
    iter = 10000)                                # analysis iterations

output(mod1)                                     # print output
posterior_plot(mod1, 'mathpost')                 # plot parameter distributions

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = math,                                   # R data frame
  ordinal = 'frlunch male',                      # binary and ordinal variables
  center = 'anxiety mathpre',                    # center predictors
  model = 'mathpost ~ anxiety anxiety^2 mathpre frlunch male',  # quadratic term
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 data sets

output(mod2)                                     # print output
distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot residuals

