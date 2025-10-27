# INTERACTION INVOLVING A SUM SCORE

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/pain.csv'

# create data frame from github data
pain <- read.csv(data_url)

# FIT MODEL ----

# interaction with incomplete sum score
model <- rblimp(
  data = pain,
  ordinal = 'dep1:dep7 male pain',
  # fixed = 'male',
  model = '
  depress_sum = dep1:+:dep7;
  disability ~ intercept@b0 depress_sum@b1 male@b2 (depress_sum*male)@b3 pain;', 
  parameters = '
    icept_female = b0;
    slope_female = b1;
    icept_male = b0 + b2;
    slope_male = b1 + b3;',
  seed = 90291,
  burn = 20000,
  iter = 10000)

output(model)
