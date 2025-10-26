# LOGISTIC REGRESSION FOR BINARY AND MULTICATEGORICAL OUTCOMES ----

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/alcoholuse.csv'

# create data frame from github data
alcoholuse <- read.csv(data_url)

# FIT MODELS ----

# binary logistic regression
model1 <- rblimp(
  data = alcoholuse,
  nominal = 'drinker college male',
  # fixed = 'male',
  model = 'drinker ~ alcage college age male', 
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(model1)

# multinomial logistic regression
model2 <- rblimp(
  data = alcoholuse,
  nominal = 'drinkingfreq college male',
  # fixed = 'male',
  model = 'drinkingfreq ~ alcage college age male', 
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(model2)

