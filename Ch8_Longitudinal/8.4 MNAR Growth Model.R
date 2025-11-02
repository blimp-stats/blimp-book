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

# FIT SHARED PARAMETER MODEL ----

# latent variable specification
model1 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout',
  clusterid = 'person',
  latent = 'person = icept linear quad;',
  center = 'grandmean = male',
  model = '
    level2:
    icept ~ intercept drug male;
    linear ~ intercept drug;
    quad ~ intercept drug;
    icept linear quad ~~ icept linear quad;
    level1:
    severity ~ intercept@icept week@linear week^2@quad;
    missingness:
    dropout ~ intercept@-3 (week==1)@0 (week==2) (week==3) (week==4) (week==5) (week==6)
      (week==2)*drug (week==3)*drug (week==4)*drug (week==5)*drug (week==6)*drug
      (week > 1)*icept (week > 1)*linear (week > 1)*quad | 1@0',
  seed = 90291,
  burn = 30000,
  iter = 30000
)
output(model1)

# FIT DIGGLE-KENWARD MODEL ----

# latent variable specification
model2 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout',
  clusterid = 'person; timeid: week;',
  latent = 'person = icept linear quad;',
  center = 'grandmean = male',
  model = '
    level2:
    icept ~ intercept drug male;
    linear ~ intercept drug;
    quad ~ intercept drug;
    icept linear quad ~~ icept linear quad;
    level1:
    severity ~ intercept@icept week@linear week^2@quad;
    missingness:
    dropout ~ intercept@-3 (week==1)@0 (week==2) (week==3) (week==4) (week==5) (week==6)
      (week==2)*drug (week==3)*drug (week==4)*drug (week==5)*drug (week==6)*drug
      (week > 1)*severity (week > 1)*severity.lag | 1@0',
  seed = 90291,
  burn = 40000,
  iter = 40000
)
output(model2)

# FIT DETRENDED DIGGLE-KENWARD MODEL ----

# latent variable specification
model3 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout',
  clusterid = 'person; timeid: week;',
  latent = 'person = icept linear quad;',
  center = 'grandmean = male',
  model = '
    level2:
    icept ~ intercept drug male;
    linear ~ intercept drug;
    quad ~ intercept drug;
    icept linear quad ~~ icept linear quad;
    level1:
    severity ~ intercept@icept week@linear week^2@quad;
    missingness:
    sev_hat = icept + linear*week + quad*week;
    sevlag_hat = icept + linear*(week - 1) + quad*(week - 1);
    dropout ~ intercept@-3 (week==1)@0 (week==2) (week==3) (week==4) (week==5) (week==6)
      (week==2)*drug (week==3)*drug (week==4)*drug (week==5)*drug (week==6)*drug
      (week > 1)*(severity - sev_hat) (week > 1)*(severity.lag - sevlag_hat) | 1@0',
  seed = 90291,
  burn = 30000,
  iter = 30000
)
output(model3)