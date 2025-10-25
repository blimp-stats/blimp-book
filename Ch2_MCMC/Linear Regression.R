# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/employee.csv'

# create data frame from github data
employee <- read.csv(data_url)

# FIT MODEL ----

# linear regression
model <- rblimp(
    data = employee,
    ordinal = 'male',
    fixed = 'male',
    model = 'empower ~ lmx climate male', 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(model)

# CUSTOM WALD TEST ----

model2 <- rblimp(
  data = employee,
  ordinal = 'male',
  fixed = 'male',
  model = 'empower ~ lmx@beta1 climate@beta2 male@beta3', # automatic multivariate distribution for incomplete predictors and latent response scores 
  waldtest = 'beta1:beta3 = 0',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(model2)
