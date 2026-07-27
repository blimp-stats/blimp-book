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
# FIT LOGISTIC MODEL WITH COMBINED MODEL SPECIFICATION ----
#------------------------------------------------------------------------------#

# logtistic regression with random intercepts
mod1 <- rblimp(
  data = worksat,
  clusterid = 'team',
  nominal = 'turnover male',
  center = 'groupmean = lmx; grandmean = lmx.mean climate male',
  model = 'turnover ~ lmx male lmx.mean climate | lmx',
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(mod1)

# plot parameter distributions
posterior_plot(model1,'turnover')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

