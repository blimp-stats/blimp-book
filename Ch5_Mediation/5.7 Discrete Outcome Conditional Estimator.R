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
# BINARY OUTCOME AND CONDITIONAL INDIRECT EFFECTS ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = worksat,
  ordinal = 'turnover male suprelqual',
  center  = 'male',
  model = '
    jobsat ~ intercept@a0 suprelqual@a1 male;
    turnover ~ intercept@b0 jobsat@b1 suprelqual@b2 male; DEBUG: compact_output',
  parameters = '
    p0 = phi(b0 + b1*(a0 + a1*0) + b2*0);
    p1 = phi(b0 + b1*(a0 + a1*1) + b2*1);
    rate0 = normal_pdf(b0 + b1*(a0 + a1*0) + b2*0);
    rate1 = normal_pdf(b0 + b1*(a0 + a1*1) + b2*1);
    indirect0 = a1 * b1 * rate0;    # conditional indirect effect at x = 0
    indirect1 = a1 * b1 * rate1;',  # conditional indirect effect at x = 1
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(mod1)

#------------------------------------------------------------------------------#
# EXTENSION: CONTINUOUS EXPLANATORY VARIABLE ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = worksat,
  ordinal = 'turnover male',
  center  = 'male',
  model = '
    jobsat ~ intercept@a0 lmx@a1 male;
    turnover ~ intercept@b0 jobsat@b1 lmx@b2 male; DEBUG: compact_output',
  simple = 'a1 * b1 * normal_pdf(b0 + b1*(a0 + a1*lmx) + b2*lmx) | lmx @ quartiles;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(mod2)

#------------------------------------------------------------------------------#
# EXTENSION: LATENT RESPONSE MEDIATOR ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = worksat,
  ordinal = 'turnover male suprelqual jobsat',
  center  = 'male',
  model = '
    jobsat ~ intercept@a0 suprelqual@a1 male;
    turnover ~ intercept@b0 jobsat.latent@b1 suprelqual@b2 male; DEBUG: compact_output',
  parameters = '
    p0 = phi(b0 + b1*(a0 + a1*0) + b2*0);
    p1 = phi(b0 + b1*(a0 + a1*1) + b2*1);
    w0 = normal_pdf(b0 + b1*(a0 + a1*0) + b2*0);
    w1 = normal_pdf(b0 + b1*(a0 + a1*1) + b2*1);
    indirect0 = a1 * b1 * w0;    # conditional indirect effect at x = 0
    indirect1 = a1 * b1 * w1;',  # conditional indirect effect at x = 1
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(mod3)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

