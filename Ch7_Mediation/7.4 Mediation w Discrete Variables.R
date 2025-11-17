# MEDIATION WITH DISCRETE INDICATORS ----

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/alcoholuse.csv'

# create data frame from github data
alcoholuse <- read.csv(data_url)

# FIT MODEL WITH BINARY MEDIATOR ----

# conditional indirect effects with a binary logistic mediator
model1 <- rblimp(
  data = alcoholuse,
  nominal = 'college male alcearly', 
  center  = 'college age',
  # fixed = 'male',
  model = '
    apath:
    alcearly ~ intercept@m_icept male@a college age;
    bpath:
    alcdays ~ intercept@y_icept alcage@b male@tau college age',
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
output(model1)

# plot parameter distributions
posterior_plot(model1)

# plot distribution of indirect effects
posterior_plot(model1, 'ind_male')
posterior_plot(model1, 'ind_female')
posterior_plot(model1, 'ind_diff')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model1)
imputed_vs_observed_plot(model1)
residuals_plot(model1)

# FIT MODEL WITH A BINARY OUTCOME ----

# conditional indirect effects with a binary logistic outcome
model2 <- rblimp(
  data = alcoholuse,
  nominal = 'college male drinker', 
  center  = 'college age',
  # fixed = 'male',
  model = '
    apath:
    alcage ~ intercept@m_icept male@a college age;
    bpath:
    drinker ~ intercept@y_icept alcage@b male@tau college age',
  parameters = '
    ind_female = a * ( b * exp( y_icept + b*(m_icept + a*0) + tau*0 ) ) /
      ( 1 + exp( y_icept + b*(m_icept + a*0) + tau*0 ) )^2;
    ind_male   = a * ( b * exp( y_icept + b*(m_icept + a*1) + tau*1 ) ) /
      ( 1 + exp( y_icept + b*(m_icept + a*1) + tau*1 ) )^2;
    ind_diff = ind_male - ind_female;',
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

# causally-defined indirect effects with a binary probit outcome
model3 <- rblimp(
  data = alcoholuse,
  nominal = 'college male', 
  ordinal = 'drinker',
  center  = 'college age',
  # fixed = 'male',
  model = '
    apath:
    alcage ~ intercept@m_icept male@a college age;
    alcage ~~ alcage@m_resvar;
    bpath:
    drinker ~ intercept@y_icept alcage@b male@tau college age',
  parameters = '
    denom = sqrt(b^2*m_resvar + 1);
    p_x0 = phi( ( y_icept + b*(m_icept + a*0) + tau*0 ) / denom );
    p_x1 = phi( ( y_icept + b*(m_icept + a*1) + tau*1 ) / denom );
    nie = phi( ( y_icept + b*(m_icept + a*1) + tau*1 ) / denom )
      - phi( ( y_icept + b*(m_icept + a*0) + tau*1 ) / denom );
    nde = phi( ( y_icept + b*(m_icept + a*0) + tau*1 ) / denom )
      - phi( ( y_icept + b*(m_icept + a*0) + tau*0 ) / denom );
    te  = p_x1 - p_x0;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model3)

# plot parameter distributions
posterior_plot(model3)

# plot distribution of indirect effects
posterior_plot(model3, 'nie')
posterior_plot(model3, 'nde')
posterior_plot(model3, 'te')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model3)
imputed_vs_observed_plot(model3)
residuals_plot(model3)

# FIT MODEL WITH A COUNT OUTCOME ----

# conditional indirect effects with a count outcome
model4 <- rblimp(
  data = alcoholuse,
  nominal = 'college male',
  count = 'alcdays',
  center = 'college age',
  # fixed = 'male',
  model = '
      apath:
      alcage ~ intercept@m_icept male@a college age;
      bpath:
      alcdays ~ intercept@y_icept alcage@b male@tau college age',
  parameters = '
      ind_female = a * (b*exp(y_icept + b*m_icept + tau*0)); 
      ind_male = a * (b*exp(y_icept + b*(m_icept + a*1) + tau*1));
      ind_diff = ind_female - ind_male;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model4)

# plot parameter distributions
posterior_plot(model4)

# plot distribution of indirect effects
posterior_plot(model4, 'ind_male')
posterior_plot(model4, 'ind_female')
posterior_plot(model4, 'ind_diff')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model4)
imputed_vs_observed_plot(model4)
residuals_plot(model4)






