# GENERALIZED LINEAR MIXED MODEL WITH DISCRETE OUTCOME

# plotting functions
# source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source("/Users/craig/Dropbox/Claude/Projects/Blimp Book/rblimp_cleaned_functions.R")

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/worksat.csv'

# create data frame from github data
worksat <- read.csv(data_url)

#------------------------------------------------------------------------------#
# ESTIMATE INTRACLASS CORRELATIONS ----
#------------------------------------------------------------------------------#

mod0 <- rblimp(
  data = worksat,
  clusterid = 'team',
  ordinal = 'turnover male',
  model = '{ turnover lmx male } ~ intercept',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(mod0)

#------------------------------------------------------------------------------#
# FIT LOGISTIC MODEL WITH COMBINED MODEL SPECIFICATION ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = worksat,
  clusterid = 'team',
  ordinal = 'male',
  nominal = 'turnover',
  center = 'groupmean = lmx; grandmean = lmx.mean climate',
  model = 'turnover ~ lmx male lmx.mean climate; DEBUG: compact_output',
  seed = 90291,
  burn = 25000,
  iter = 25000)

# print output
output(mod1)

# plot parameter distributions
posterior_plot(mod1,'turnover')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = worksat,
  clusterid = 'team',
  ordinal = 'male',
  nominal = 'turnover',
  center = 'groupmean = lmx; grandmean = lmx.mean climate',
  model = 'turnover ~ lmx male lmx.mean climate',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)                                    # save 20 imputed data sets

output(mod2)                                     # print output

distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot binned residuals

