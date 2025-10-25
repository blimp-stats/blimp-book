# EXAMPLE 5.3 - Bayes Regression

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/employee.rda"
load(gzcon(url(data_url, open = "rb")))

# multivariate specification for incomplete predictors
analysis1 <- rblimp(
    data = employee,
    ordinal = 'male',
    fixed = 'male',
    model = 'empower ~ lmx@beta1 climate@beta2 male@beta3', # automatic multivariate distribution for incomplete predictors and latent response scores 
    waldtest = 'beta1:beta3 = 0',
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis1)

# sequential specification for incomplete predictors
analysis2 <- rblimp(
  data = employee,
  ordinal = 'male',
  fixed = 'male',
  model = '
    focal.model: 
    empower ~ lmx climate male;
    predictor.model: 
    lmx climate ~ male', # automatic sequential specification for variables to the left of the tilde 
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(analysis2)
