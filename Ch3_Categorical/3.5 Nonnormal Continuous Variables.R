# NEGATIVE BINOMIAL REGRESSION FOR A COUNT OUTCOME ----

# LOAD R PACKAGES ----

library(rblimp)
library(psych)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflamm <- read.csv(data_url)

# plotting function
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# SUMMARIZE DATA ----

describe(inflamm)
hist(inflamm$dpdd, breaks = 50)
hist(inflamm$inflam_sum, breaks = 50)

# FIT MODEL ----

# nonnormal outcome
model1 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  center = 'age',
  # fixed = 'male',
  model = 'dpdd ~ female els age inflam_sum', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

output(model1)

plot_imputations_overlay(model1, inflamm, var = "dpdd")

# yeo-johnson transformation for nonnormal outcome
model2 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  center = 'age',
  # fixed = 'male',
  model = 'yjt(dpdd - 6) ~ female els age inflam_sum', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

plot_imputations_overlay(model2, inflamm, var = "dpdd")
plot_imputations(model2, var = "yjt.dpdd.6.")
