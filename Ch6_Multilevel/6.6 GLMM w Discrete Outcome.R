# GENERALIZED LINEAR MIXED MODEL WITH DISCRETE OUTCOME

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/employee.csv'

# create data frame from github data
employee <- read.csv(data_url)

# FIT LOGISTIC MODEL ----

# mixed model specification
model1 <- rblimp(
  data = employee,
  clusterid = 'team',
  nominal = 'turnover male',
  center = 'groupmean = lmx; grandmean = lmx.mean climate male',
  model = 'turnover ~ lmx male lmx.mean climate',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model1)

model2 <- rblimp(
  data = employee,
  clusterid = 'team',
  nominal = 'turnover male',
  latent = 'team = ranicept;',
  center = 'groupmean = lmx; grandmean = lmx.mean climate male',
  model = '
    level2:
    ranicept ~ intercept lmx.mean climate;
    level1:
    turnover ~ intercept@ranicept lmx male;',
  seed = 90291,
  burn = 20000,
  iter = 30000
)
output(model2)

# BRIAN, REMOVE ODDS RATIO FOR INTERCEPT@ ? ----