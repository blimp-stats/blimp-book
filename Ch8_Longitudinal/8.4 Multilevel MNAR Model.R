# MULTILEVEL MODEL WITH RANDOM SLOPES AND INTERACTIONS

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)
set_blimp('/applications/blimp/blimp-nightly')

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/trial_stacked.csv'

# create data frame from github data
trial <- read.csv(data_url)

# FIT WU-CARROL SHARED PARAMETER LINEAR GROWTH MODEL ----

# linear growth model with predictors
model1 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout',
  clusterid = 'person',
  center = 'grandmean = male',
  latent = 'person = icept linear;',
  model = '
    level2:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear;
    missingness:
    dropout ~ intercept@-3 (week==1) (week==2) (week==3) 
      (week==1)*drug (week==2)*drug (week==3)*drug
      (week > 0)*icept (week > 0)*linear | 1@0;',
  parameters = '
    mu3_drug0 = b0 + b1*3;
    mu3_drug1 = (b0 + b01) + (b1 + b11)*3;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 40000,
  iter = 40000
)

output(model1)

# FIT WU-CARROL SHARED PARAMETER QUADRATIC GROWTH MODEL ----

# quadratic growth model with predictors
model2 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout',
  clusterid = 'person',
  center = 'grandmean = male',
  latent = 'person = icept linear;',
  model = '
    level2:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear week^2@b2 week^2*drug@b21;
    missingness:
    dropout ~ intercept@-3 (week==1) (week==2) (week==3) 
      (week==1)*drug (week==2)*drug (week==3)*drug
      (week > 0)*icept (week > 0)*linear | 1@0;',
  parameters = '
    mu3_drug0 = b0 + b1*3 + b2*9;
    mu3_drug1 = (b0 + b01) + (b1 + b11)*3 + (b2 + b21)*9;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 30000,
  iter = 30000
)

output(model2)

# FIT DIGGLE-KENWARD LINEAR GROWTH MODEL ----

# linear growth model with predictors
model3 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout',
  clusterid = 'person; timeid: week;',
  center = 'grandmean = male',
  latent = 'person = icept linear;',
  model = '
    level2:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear;
    missingness:
    dropout ~ intercept@-3 (week==1) (week==2) (week==3) 
      (week==1)*drug (week==2)*drug (week==3)*drug
      (week > 0)*severity (week > 0)*severity.lag | 1@0;',
  parameters = '
    mu3_drug0 = b0 + b1*3;
    mu3_drug1 = (b0 + b01) + (b1 + b11)*3;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 50000,
  iter = 50000
)

output(model3)

# FIT DIGGLE-KENWARD QUADRATIC GROWTH MODEL ----

# quadratic growth model with predictors
model4 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout',
  clusterid = 'person; timeid: week;',
  center = 'grandmean = male',
  latent = 'person = icept linear;',
  model = '
    level2:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear week^2@b2 week^2*drug@b21;
    missingness:
    dropout ~ intercept@-3 (week==1) (week==2) (week==3) 
      (week==1)*drug (week==2)*drug (week==3)*drug
      (week > 0)*severity (week > 0)*severity.lag | 1@0;',
  parameters = '
    mu3_drug0 = b0 + b1*3 + b2*9;
    mu3_drug1 = (b0 + b01) + (b1 + b11)*3 + (b2 + b21)*9;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 30000,
  iter = 30000
)

output(model4)

