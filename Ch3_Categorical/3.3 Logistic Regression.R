# LOGISTIC REGRESSION FOR A BINARY OUTCOME ----

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/employee.csv'

# create data frame from github data
employee <- read.csv(data_url)

# FIT MODEL ----

# logistic regression defining outcome as ordinal
model1 <- rblimp(
    data = employee,
    ordinal = 'turnover male',
    model = 'logit(turnover) ~ lmx empower male', # automatic multivariate distribution for incomplete predictors and latent response scores 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(model1)

# logistic regression defining outcome as nominal
model2 <- rblimp(
  data = employee,
  nominal = 'turnover male',
  model = 'turnover ~ lmx empower male', # automatic multivariate distribution for incomplete predictors and latent response scores 
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(model2)


