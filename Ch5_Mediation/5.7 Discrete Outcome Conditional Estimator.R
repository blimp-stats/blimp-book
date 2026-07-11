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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflamm <- read.csv(data_url)

#------------------------------------------------------------------------------#
# BINARY OUTCOME AND CONDITIONAL INDIRECT EFFECTS ----
#-----------#------------------------------------------------------------------------------#--------------------------------------------------------#

# probit regression for the b-path
mod1 <- rblimp(
  data = alcohol,
  nominal = 'college male',        # drinker removed from this line
  ordinal = 'drinker',             # ordinal => probit (latent response)
  center  = 'college age',
  model = '
    apath:
    agetryalc ~ intercept@m_icept male@a college age;
    bpath:
    drinker ~ intercept@y_icept agetryalc@b male@tau college age',
  parameters = '
    ind_female = a * b * exp( -0.5*( y_icept + b*(m_icept + a*0) + tau*0 )^2 ) / sqrt(2*_pi);
    ind_male   = a * b * exp( -0.5*( y_icept + b*(m_icept + a*1) + tau*1 )^2 ) / sqrt(2*_pi);
    ind_diff   = ind_male - ind_female;',
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

