# MEDIATION WITH DISCRETE INDICATORS ----

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/alcoholuse.csv'

# create data frame from github data
alcoholuse <- read.csv(data_url)

# FIT MEDIATION MODEL WITH DISCRETE MEDIATOR ----

# mediation with a binary logistic mediator
model_lm <- rblimp(
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
  iter = 10000)

output(model_lm)

# plot distribution of indirect effects
posterior_plot(model_lm, 'ind_male')
posterior_plot(model_lm, 'ind_female')
posterior_plot(model_lm, 'ind_diff')

# FIT MEDIATION MODEL WITH DISCRETE OUTCOMES ----

# mediation with a binary logistic outcome
model_lo <- rblimp(
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
  iter = 10000)

output(model_lo)

# plot distribution of indirect effects
posterior_plot(model_lo, 'ind_male')
posterior_plot(model_lo, 'ind_female')
posterior_plot(model_lo, 'ind_diff')

# mediation with a count outcome
model_co <- rblimp(
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
    iter = 10000)

output(model_co)

# plot distribution of indirect effects
posterior_plot(model_co, 'ind_male')
posterior_plot(model_co, 'ind_female')
posterior_plot(model_co, 'ind_diff')

# FIT MEDIATION MODEL WITH CAUSAL INDIRECT EFFECT DEFINITIONS ----

# mediation with a binary probit outcome
model_oo <- rblimp(
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
  iter = 10000)

output(model_oo)

# plot distribution of indirect effects
posterior_plot(model_co, 'nie')
posterior_plot(model_co, 'nde')
posterior_plot(model_co, 'te')
