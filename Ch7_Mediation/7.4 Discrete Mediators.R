# MEDIATION WITH DISCRETE MEDIATORS ----

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/alcoholuse.csv'

# create data frame from github data
alcoholuse <- read.csv(data_url)

# FIT MODEL WITH LATENT RESPONSE MEDIATOR ----

# binary probit (latent response) model for mediator
model1 <- rblimp(
  data = alcoholuse,
  nominal = 'college male',
  ordinal = 'alcearly',
  center  = 'college age',
  # fixed = 'male age',
  model = '
    apath:
    alcearly ~ intercept@m_icept male@a college age;
    bpath:
    alcdays ~ intercept@y_icept alcearly.latent@b male@tau college age',
  parameters = 'indirect = a*b',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1)

# plot distribution of indirect effect
posterior_plot(model1, 'indirect')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model1)
imputed_vs_observed_plot(model1)
residuals_plot(model1)

# FIT MODEL WITH CONDITIONAL INDIRECT EFFECTS ----

# binary logistic model for mediator
model2 <- rblimp(
  data = alcoholuse,
  nominal = 'college male alcearly', 
  center  = 'college age',
  # fixed = 'male age',
  model = '
    apath:
    alcearly ~ intercept@m_icept male@a college age;
    bpath:
    alcdays ~ intercept@y_icept alcearly@b male@tau college age',
  parameters = '
    ind_female = a * b * exp( m_icept + a*0 ) /
       ( 1 + exp( m_icept + a*0 ) )^2;
    ind_male = a * b * exp( m_icept + a*1 ) /
     ( 1 + exp( m_icept + a*1 ) )^2;
    ind_diff   = ind_male - ind_female;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2)

# plot distribution of indirect effects
posterior_plot(model2, 'ind_male')
posterior_plot(model2, 'ind_female')
posterior_plot(model2, 'ind_diff')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)

