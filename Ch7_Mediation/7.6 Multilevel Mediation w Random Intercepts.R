# MULTILEVEL MEDIATION

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(rblimp)

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/employee.csv'

# create data frame from github data
employee <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT 1-1-1 RANDOM INTERCEPT MODEL WITH COMBINED MODEL SPECIFICATION ----
#------------------------------------------------------------------------------#

# mixed model specification
model1 <- rblimp(
  data = employee,
  clusterid = 'team',
  center = 'groupmean = lmx empower;',
  model = '
    apath:
    empower ~ lmx@a_w lmx.mean@a_b;
    bpath:
    jobsat ~ lmx lmx.mean empower@b_w empower.mean@b_b',
  parameters = '
    ind_w = a_w*b_w;
    ind_b = a_b*b_b;',
  seed = 90291,
  burn = 20000,
  iter = 20000,
  nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1)

# plot distribution of indirect effects
posterior_plot(model1, 'ind_w')
posterior_plot(model1, 'ind_b')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# FIT 1-1-1 RANDOM INTERCEPT MODEL WITH A LATENT VARIABLE SPECIFICATION ----
#------------------------------------------------------------------------------#

# random intercepts as level-2 latent variables
model2 <- rblimp(
  data = employee,
  clusterid = 'team',
  latent = 'team = empower_b jobsat_b;',
  center = 'groupmean = lmx;',
  model = '
    apath:
    empower_b ~ intercept lmx.mean@a_b;
    empower ~ intercept@empower_b lmx@a_w ;
    bpath:
    jobsat_b ~ lmx.mean empower_b@b_b;
    jobsat ~ intercept@jobsat_b lmx (empower - empower_b)@b_w ',
  parameters = '
    ind_w = a_w*b_w;
    ind_b = a_b*b_b;',
  seed = 90291,
  burn = 20000,
  iter = 20000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2)

# plot distribution of indirect effects
posterior_plot(model2, 'ind_w')
posterior_plot(model2, 'ind_b')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#


