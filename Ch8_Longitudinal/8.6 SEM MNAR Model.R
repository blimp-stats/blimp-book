# REPEATED MEASURES ANALYSIS WITH UNSTRUCTURED CHANGE

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/trial_wide.csv'

# create data frame from github data
trial <- read.csv(data_url)

# FIT WU-CARROL SHARED PARAMETER LINEAR GROWTH MODEL ----

# linear growth model with predictors
# equality constraints produce low N_eff
model1 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout1:dropout3',
  latent = 'icept linear', 
  center = 'male',
  model = '
    structural:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    icept ~~ linear;
    measurement:
    icept -> severity0@1 severity1@1 severity2@1 severity3@1; 
    linear -> severity0@0 severity1@1 severity2@2 severity3@3; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    residual:
    severity0 ~~ severity0@res;
    severity1 ~~ severity1@res;
    severity2 ~~ severity2@res;
    severity3 ~~ severity3@res;
    missingness:
    dropout1 ~ icept@bi linear@bl drug;
    dropout2 ~ icept@bi linear@bl drug;
    dropout3 ~ icept@bi linear@bl drug;',
  parameters = '
    mu3_drug0 = b0 + b1*3;
    mu3_drug1 = (b0 + b01) + (b1 + b11)*3;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291, # integer random number seed
  burn = 100000, # warm up iterations
  iter = 100000) # iterations for analysis

output(model1)

# FIT WU-CARROL SHARED PARAMETER QUADRATIC GROWTH MODEL ----

# quadratic fixed effect with predictors
model2 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout1:dropout3',
  latent = 'icept linear quad', 
  center = 'male',
  model = '
    structural:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    quad ~ intercept@b2 drug@b21;
    quad ~~ quad@.01;
    icept ~~ linear;
    measurement:
    icept -> severity0@1 severity1@1 severity2@1 severity3@1; 
    linear -> severity0@0 severity1@1 severity2@2 severity3@3; 
    quad -> severity0@0 severity1@1 severity2@4 severity3@9; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    residual:
    severity0 ~~ severity0@res;
    severity1 ~~ severity1@res;
    severity2 ~~ severity2@res;
    severity3 ~~ severity3@res;
    missingness:
    dropout1 ~ icept@bi linear@bl drug;
    dropout2 ~ icept@bi linear@bl drug;
    dropout3 ~ icept@bi linear@bl drug;',
  parameters = '
    mu3_drug0 = b0 + b1*3 + b2*9;
    mu3_drug1 = (b0 + b01) + (b1 + b11)*3 + (b2 + b21)*9;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291, # integer random number seed
  burn = 300000, # warm up iterations
  iter = 300000) # iterations for analysis

output(model2)

# FIT DIGGLE-KENWARD LINEAR GROWTH MODEL ----

# linear growth model with predictors
model3 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout1:dropout3',
  latent = 'icept linear', 
  center = 'male',
  model = '
    structural:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    icept ~~ linear;
    measurement:
    icept -> severity0@1 severity1@1 severity2@1 severity3@1; 
    linear -> severity0@0 severity1@1 severity2@2 severity3@3; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    residual:
    severity0 ~~ severity0@res;
    severity1 ~~ severity1@res;
    severity2 ~~ severity2@res;
    severity3 ~~ severity3@res;
    missingness:
    dropout1 ~ severity0@mar severity1@mnar drug;
    dropout2 ~ severity1@mar severity2@mnar drug;
    dropout3 ~ severity2@mar severity3@mnar drug;',
  parameters = '
    mu3_drug0 = b0 + b1*3;
    mu3_drug1 = (b0 + b01) + (b1 + b11)*3;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291, # integer random number seed
  burn = 200000, # warm up iterations
  iter = 200000) # iterations for analysis

output(model3)

# FIT DIGGLE-KENWARD QUADRATIC GROWTH MODEL ----

# quadratic fixed effect with predictors
model4 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout1:dropout3',
  latent = 'icept linear quad', 
  center = 'male',
  model = '
    structural:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    quad ~ intercept@b2 drug@b21;
    quad ~~ quad@.01;
    icept ~~ linear;
    measurement:
    icept -> severity0@1 severity1@1 severity2@1 severity3@1; 
    linear -> severity0@0 severity1@1 severity2@2 severity3@3; 
    quad -> severity0@0 severity1@1 severity2@4 severity3@9; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    residual:
    severity0 ~~ severity0@res;
    severity1 ~~ severity1@res;
    severity2 ~~ severity2@res;
    severity3 ~~ severity3@res;
    missingness:
    dropout1 ~ severity0@mar severity1@mnar drug;
    dropout2 ~ severity1@mar severity2@mnar drug;
    dropout3 ~ severity2@mar severity3@mnar drug;',
  parameters = '
    mu3_drug0 = b0 + b1*3 + b2*9;
    mu3_drug1 = (b0 + b01) + (b1 + b11)*3 + (b2 + b21)*9;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291, # integer random number seed
  burn = 120000, # warm up iterations
  iter = 120000) # iterations for analysis

output(model4)


