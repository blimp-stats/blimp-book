# NEGATIVE BINOMIAL REGRESSION FOR A COUNT OUTCOME ----

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflamm <- read.csv(data_url)

# plotting function
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# SUMMARIZE DATA ----

dfSummary(inflamm)
describe(inflamm)
hist(inflamm$dpdd, breaks = 50)
hist(inflamm$inflam_sum, breaks = 50)

# FIT MODEL ASSUMING NORMALITY ----

# assuming normal outcome and predictor
model1 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  center = 'age',
  # fixed = 'age female',
  model = 'dpdd ~ female els age inflam_sum', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 100)

output(model1)

# imputed vs. observed distributions
plot_imputations_overlay(model1, inflamm, var = "dpdd")
plot_imputations_overlay(model1, inflamm, var = "inflam_sum")

# FIT MODEL WITH NORMALIZED OUTCOME ----

# normalized outcome with yeo-johnson transformation
model2 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  center = 'age',
  # fixed = 'age female',
  model = 'yjt(dpdd - 6) ~ female els age inflam_sum', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 100)

output(model2)

# imputed vs. observed distributions
plot_imputations_overlay(model2, inflamm, var = "dpdd")

# normalized distribution
plot_imputations(model2, var = "yjt.dpdd.6.")

# FIT MODEL WITH NORMALIZED PREDICTOR ----

# normalized outcome and predictor with yeo-johnson transformation
model3 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  center = 'age',
  # fixed = 'age female',
  model = '
  focal:
  yjt(dpdd - 6) ~ female els age inflam_sum;
  predictor:
  yjt(inflam_sum) ~ female els age;', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 100)

output(model3)

# imputed vs. observed distributions
plot_imputations_overlay(model3, inflamm, var = "dpdd")
plot_imputations_overlay(model3, inflamm, var = "inflam_sum")
