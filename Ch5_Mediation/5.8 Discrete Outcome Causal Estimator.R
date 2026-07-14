# MEDIATION WITH BINARY OUTCOME USING CONDITIONAL ESTIMATOR

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
# BINARY OUTCOME AND TOTAL NATURAL INDIRECT EFFECT ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = worksat,
  ordinal = 'turnover male suprelqual',
  center  = 'male',
  model = '
    jobsat ~ intercept@a0 suprelqual@a1 male;
    jobsat@resvar_m;
    turnover ~ intercept@b0 jobsat@b1 suprelqual@b2 male;',
  parameters = '
    sd_lat = sqrt(b1^2 * resvar_m + 1);
    p_obs = phi((b0 + b1*(a0 + a1*1) + b2*1) / sd_lat);
    p_cfact = phi((b0 + b1*(a0 + a1*0) + b2*1) / sd_lat);
    tnie    = p_obs - p_cfact;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(mod1)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#