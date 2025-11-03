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

# linear growth model
model1 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout',
  clusterid = 'person',
  latent = 'person = icept linear;',
  center = 'grandmean = male',
  fixed = 'male drug',
  model = '
    level2:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear;
    missingness:
    dropout ~ intercept@-3 (week==1)@0 (week==2) (week==3) (week==4) (week==5) (week==6)
      (week==2)*drug (week==3)*drug (week==4)*drug (week==5)*drug (week==6)*drug
      (week > 1)*icept (week > 1)*linear | 1@0',
  parameters = '
    mu6_drug0 = b0 + 6*b1;
    mu6_drug1 = (b0 + b01) + 6*(b1 + b11);
    mu6_diff = mu6_drug1 - mu6_drug0;',
  seed = 90291,
  burn = 20000,
  iter = 20000
)

output(model1)

# quadratic growth model
model2 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout',
  clusterid = 'person',
  latent = 'person = icept linear quad;',
  center = 'grandmean = male',
  fixed = 'male drug',
  model = '
    level2:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    quad ~ intercept@b2 drug@b21;
    quad ~~ quad@.001;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear week^2@quad;
    missingness:
    dropout ~ intercept@-3 (week==1)@0 (week==2) (week==3) (week==4) (week==5) (week==6)
      (week==2)*drug (week==3)*drug (week==4)*drug (week==5)*drug (week==6)*drug
      (week > 1)*icept (week > 1)*linear | 1@0',
  parameters = '
    mu6_drug0 = b0 + 6*b1 + 36*b2;
    mu6_drug1 = (b0 + b01) + 6*(b1 + b11) + 36*(b2 + b21);
    mu6_diff = mu6_drug1 - mu6_drug0;',
  seed = 90291,
  burn = 20000,
  iter = 20000
)

output(model2)

# FIT DIGGLE-KENWARD MODEL ----

# linear growth model
model3 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout',
  clusterid = 'person; timeid: week;',
  latent = 'person = icept linear;',
  center = 'grandmean = male',
  fixed = 'male drug',
  model = '
    level2:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear;
    missingness:
    dropout ~ intercept@-3 (week==1)@0 (week==2) (week==3) (week==4) (week==5) (week==6)
      (week==2)*drug (week==3)*drug (week==4)*drug (week==5)*drug (week==6)*drug
      (week > 1)*severity (week > 1)*severity.lag | 1@0',
  parameters = '
    mu6_drug0 = b0 + 6*b1;
    mu6_drug1 = (b0 + b01) + 6*(b1 + b11);
    mu6_diff = mu6_drug1 - mu6_drug0;',
  seed = 90291,
  burn = 100000,
  iter = 100000
)

output(model3)

# quadratic growth model
model4 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout',
  clusterid = 'person; timeid: week;',
  latent = 'person = icept linear quad;',
  center = 'grandmean = male',
  fixed = 'male drug',
  model = '
    level2:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    quad ~ intercept@b2 drug@b21;
    quad ~~ quad@.001;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear week^2@quad;
    missingness:
    dropout ~ intercept@-3 (week==1)@0 (week==2) (week==3) (week==4) (week==5) (week==6)
      (week==2)*drug (week==3)*drug (week==4)*drug (week==5)*drug (week==6)*drug
      (week > 1)*severity (week > 1)*severity.lag | 1@0',
  parameters = '
    mu6_drug0 = b0 + 6*b1 + 36*b2;
    mu6_drug1 = (b0 + b01) + 6*(b1 + b11) + 36*(b2 + b21);
    mu6_diff = mu6_drug1 - mu6_drug0;',
  seed = 90291,
  burn = 100000,
  iter = 100000
)

output(model4)

# FIT DETRENDED DIGGLE-KENWARD MODEL ----

# linear growth model
model5 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout',
  clusterid = 'person; timeid: week;',
  latent = 'person = icept linear;',
  center = 'grandmean = male',
  fixed = 'male drug',
  model = '
    level2:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear;
    missingness:
    sev_hat = icept + linear*week;
    sevlag_hat = icept + linear*(week - 1);
    dropout ~ intercept@-3 (week==1)@0 (week==2) (week==3) (week==4) (week==5) (week==6)
      (week==2)*drug (week==3)*drug (week==4)*drug (week==5)*drug (week==6)*drug
      (week > 1)*(severity - sev_hat) (week > 1)*(severity.lag - sevlag_hat) | 1@0',
  parameters = '
    mu6_drug0 = b0 + 6*b1;
    mu6_drug1 = (b0 + b01) + 6*(b1 + b11);
    mu6_diff = mu6_drug1 - mu6_drug0;',
  seed = 90291,
  burn = 400000,
  iter = 400000
)

output(model5)

# quadratic growth model
model6 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout',
  clusterid = 'person; timeid: week;',
  latent = 'person = icept linear quad;',
  center = 'grandmean = male',
  model = '
    level2:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    quad ~ intercept@b2 drug@b21;
    quad ~~ quad@.001;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear week^2@quad;
    missingness:
    sev_hat = icept + linear*week + quad*week;
    sevlag_hat = icept + linear*(week - 1) + quad*(week - 1);
    dropout ~ intercept@-3 (week==1)@0 (week==2) (week==3) (week==4) (week==5) (week==6)
      (week==2)*drug (week==3)*drug (week==4)*drug (week==5)*drug (week==6)*drug
      (week > 1)*(severity - sev_hat) (week > 1)*(severity.lag - sevlag_hat) | 1@0',
  parameters = '
    mu6_drug0 = b0 + 6*b1 + 36*b2;
    mu6_drug1 = (b0 + b01) + 6*(b1 + b11) + 36*(b2 + b21);
    mu6_diff = mu6_drug1 - mu6_drug0;',
  seed = 90291,
  burn = 300000,
  iter = 300000
)

output(model6)
