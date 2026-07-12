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
    ind_relqual0 = a1 * b1 * normal_pdf(b0 + b1*a0);              # conditional indirect effect at x = 0
    ind_relqual1 = a1 * b1 * normal_pdf(b0 + b1*(a0 + a1) + b2);  # conditional indirect effect at x = 0
    ind_diff = ind_relqual1 - ind_relqual0;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(mod1)









# probit regression for the b-path
mod1 <- rblimp(
  data = inflamm,
  ordinal = 'female els hvydrink',             # ordinal => probit (latent response)
  center  = 'female age',
  model = '
    apath:
    inflam ~ intercept@a0 els@a1 female age;
    bpath:
    hvydrink ~ intercept@b0 inflam@b1 els@b2 female age; DEBUG: compact_output',
  parameters = '
    ind_els0 = a1 * b1 * exp( -0.5*( b0 + b1*(a0 + a1*0) + b2*0 )^2 ) / sqrt(2*_pi);
    ind_els1 = a1 * b1 * exp( -0.5*( b0 + b1*(a0 + a1*1) + b2*1 )^2 ) / sqrt(2*_pi);
    ind_diff = ind_els1 - ind_els0;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(mod1)

# probit regression for the b-path
mod1 <- rblimp(
  data = alcohol,
  ordinal = 'college male drinker', 
  center  = 'college age',
  model = '
    apath:
    agetryalc ~ intercept@a0 male@a1 college age;
    bpath:
    drinker ~ intercept@b0 agetryalc@b1 male@b2 college age',
  parameters = '
    ind_els0 = a1 * b1 * exp( -0.5*( b0 + b1*(a0 + a1*0) + b2*0 )^2 ) / sqrt(2*_pi);
    ind_els1 = a1 * b1 * exp( -0.5*( b0 + b1*(a0 + a1*1) + b2*1 )^2 ) / sqrt(2*_pi);
    ind_diff = ind_els1 - ind_els0;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(mod1)

# plot distribution of indirect effects
posterior_plot(mod1, 'ind_male')
posterior_plot(mod1, 'ind_female')
posterior_plot(mod1, 'ind_diff')

# logistic regression for the b-path
mod2 <- rblimp(
  data = alcoholuse,
  nominal = 'college male drinker', 
  center  = 'college age',
  # fixed = 'male age',
  model = '
    apath:
    agetryalc ~ intercept@m_icept male@a college age;
    bpath:
    drinker ~ intercept@y_icept agetryalc@b male@tau college age',
  parameters = '
    ind_female = a * ( b * exp( y_icept + b*(m_icept + a*0) + tau*0 ) ) /
      ( 1 + exp( y_icept + b*(m_icept + a*0) + tau*0 ) )^2;
    ind_male   = a * ( b * exp( y_icept + b*(m_icept + a*1) + tau*1 ) ) /
      ( 1 + exp( y_icept + b*(m_icept + a*1) + tau*1 ) )^2;
    ind_diff = ind_male - ind_female;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(mod2)

# plot distribution of indirect effects
posterior_plot(mod2, 'ind_male')
posterior_plot(mod2, 'ind_female')
posterior_plot(mod2, 'ind_diff')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

