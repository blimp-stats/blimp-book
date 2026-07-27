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
# FIT 1-1-1 RANDOM SLOPE MODEL ----
#------------------------------------------------------------------------------#

# latent variable specification
model3 <- rblimp(
  data = employee,
  clusterid = 'team',
  latent = 'team = empower_b jobsat_b a_slope b_slope tau_slope;',
  center = 'groupmean = lmx;',
  model = '
    apath:
    empower_b ~ intercept lmx.mean@a_b;
    empower ~ intercept@empower_b lmx@a_slope;
    bpath:
    jobsat_b ~ lmx.mean empower_b@b_b;
    jobsat ~ intercept@jobsat_b lmx@tau_slope (empower - empower_b)@b_slope;
    ranslopes:
    tau_slope ~ intercept;
    a_slope ~ intercept@a_w;
    b_slope ~ intercept@b_w;
    a_slope ~~ b_slope@cor_ab;
    a_slope b_slope ~~ tau_slope',
  parameters = '
    cov_ab = cor_ab * sqrt(a_slope.totalvar * b_slope.totalvar);
    ind_w = a_w*b_w + cov_ab;
    ind_b = a_b*b_b;',
  seed = 90291,
  burn = 20000,
  iter = 20000,
  nimps = 20)

# print output
output(model3)

# plot parameter distributions
posterior_plot(model3)

# plot distribution of indirect effects
posterior_plot(model3, 'ind_w')
posterior_plot(model3, 'ind_b')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

