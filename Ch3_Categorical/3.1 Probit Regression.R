# PROBIT REGRESSION FOR BINARY AND ORDINAL OUTCOMES ----

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/employee.csv'

# create data frame from github data
employee <- read.csv(data_url)

# FIT MODEL ----

# probit regression with a binary outcome
model1 <- rblimp(
    data = employee,
    ordinal = 'turnover male',
    model = 'turnover ~ lmx empower male', # automatic multivariate distribution for incomplete predictors and latent response scores 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(model1)

# ADD PLOTS OF LATENT SCORES ----

# probit regression with an ordinal outcome
model2 <- rblimp(
  data = employee,
  ordinal = 'jobsat male',
  model = 'jobsat ~ lmx empower male', # automatic multivariate distribution for incomplete predictors and latent response scores 
  seed = 90291,
  burn = 20000,
  iter = 20000)

output(model2)

# ADD PLOTS OF LATENT SCORES ----
