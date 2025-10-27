# EXAMPLE 5.7 - Bayes Curvilinear Regression

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/math.rda"
load(gzcon(url(data_url, open = "rb")))

# multivariate specification for incomplete predictors
analysis1 <- rblimp(
    data = math,
    ordinal = 'frlunch male',
    fixed = 'male mathpre',
    center = 'anxiety',
    model = 'mathpost ~ anxiety (anxiety^2) frlunch mathpre male', # automatic multivariate distribution for incomplete predictors and latent response scores
    seed = 12345,
    burn = 10000,
    iter = 10000)

output(analysis1)

# sequential specification for incomplete predictors
analysis2 <- rblimp(
  data = math,
  ordinal = 'frlunch male',
  fixed = 'male mathpre',
  center = 'anxiety',
  model = '
    focal.model: 
    mathpost ~ anxiety (anxiety^2) frlunch mathpre male;
    predictor.models: 
    anxiety frlunch ~ mathpre male', # automatic sequential specification for variables to the left of the tilde 
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(analysis2)