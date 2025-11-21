# BRIAN NOTES ----
# when specifying nonexistent timeid, error says that time is not at lowest level (rather than nonexistent)
# when omitting timeid, error references time variable (not timeid variable)
# timeid variable automatically becomes fixed (as it should, but any downstream consequences?)
# timeid does not appear in missing data rate table

# MULTILEVEL GROWTH MODEL WITH MNAR ASSUMPTION

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/trial_stacked.csv'

# create data frame from github data
trial <- read.csv(data_url)

# MAR MISSINGNESS MODEL ----

psych::describeBy(dropout ~ week:drug, data = trial)

# mar missingness model with dropout varying by time and group
model1 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'person; timeid: week; dropout: dropout = severity',
  # timeid = 'week',
  # dropout = 'dropoutb = severity',
  center = 'grandmean = male',
  latent = 'person = icept linear;',
  model = '
    level2:
    icept ~ intercept@icept_d0 drug@icept_diff male;
    linear ~ intercept@slp_d0 drug@slp_diff;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear;
    missingness:
    dropout ~ intercept@-3 (week==1) (week==2) (week==3)
      (week==1)*drug (week==2)*drug (week==3)*drug | intercept@0;',
  parameters = '
    mu3_drug0 = icept_d0 + slp_d0*3;
    mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot predicted values by time
bivariate_plot(model5, severity.predicted ~ week, lines = T)

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model5)
imputed_vs_observed_plot(model5)
residuals_plot(model5)



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

