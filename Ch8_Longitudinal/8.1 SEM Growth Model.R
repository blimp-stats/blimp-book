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

# FIT REPEATED MEASURES MODEL ----

model1 <- rblimp(
  data = trial,
  latent = 'subjects',
  model = '
    subjects ~ intercept@0;
    { severity0:severity6 } ~ intercept subjects@1;',
  seed = 90291, # integer random number seed
  burn = 10000, # warm up iterations
  iter = 10000) # iterations for analysis

output(model1)

# FIT LINEAR GROWTH MODEL ----

# linear growth model
model2 <- rblimp(
  data = trial,
  latent = 'icept linear',
  model = '
    structural:
    intercept -> icept linear;
    icept ~~ linear;
    measurement:
    icept -> severity0@1 severity1@1 severity2@1 severity3@1; 
    linear -> severity0@0 severity1@1 severity2@2 severity3@3; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    residual:
    severity0 ~~ severity0@res;
    severity1 ~~ severity1@res;
    severity2 ~~ severity2@res;
    severity3 ~~ severity3@res;',
  seed = 90291, # integer random number seed
  burn = 30000, # warm up iterations
  iter = 30000) # iterations for analysis

output(model2)

# linear growth model with predictors
model3 <- rblimp(
  data = trial,
  ordinal = 'male drug',
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
    severity3 ~~ severity3@res;',
  parameters = '
    mu3_drug0 = b0 + b1*3;
    mu3_drug1 = (b0 + b01) + (b1 + b11)*3;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291, # integer random number seed
  burn = 20000, # warm up iterations
  iter = 20000) # iterations for analysis

output(model3)

# FIT QUADRATIC GROWTH MODEL ----

# quadratic fixed effect but no quadratic variance
model4 <- rblimp(
  data = trial,
  latent = 'icept linear quad',
  model = '
    structural:
    intercept -> icept linear quad;
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
    severity3 ~~ severity3@res;',
  seed = 90291, # integer random number seed
  burn = 30000, # warm up iterations
  iter = 30000) # iterations for analysis

output(model4)

# quadratic fixed effect and quadratic variance
# N_eff values decrease given same iterations as model 4
model5 <- rblimp(
  data = trial,
  latent = 'icept linear quad',
  model = '
    structural:
    intercept -> icept linear quad;
    icept linear quad ~~ icept linear quad;
    measurement:
    icept -> severity0@1 severity1@1 severity2@1 severity3@1; 
    linear -> severity0@0 severity1@1 severity2@2 severity3@3; 
    quad -> severity0@0 severity1@1 severity2@4 severity3@9; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    residual:
    severity0 ~~ severity0@res;
    severity1 ~~ severity1@res;
    severity2 ~~ severity2@res;
    severity3 ~~ severity3@res;',
  seed = 90291, # integer random number seed
  burn = 30000, # warm up iterations
  iter = 30000) # iterations for analysis

output(model5)

# quadratic fixed effect with predictors
model6 <- rblimp(
  data = trial,
  ordinal = 'male drug',
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
    severity3 ~~ severity3@res;',
  parameters = '
    mu3_drug0 = b0 + b1*3 + b2*9;
    mu3_drug1 = (b0 + b01) + (b1 + b11)*3 + (b2 + b21)*9;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291, # integer random number seed
  burn = 40000, # warm up iterations
  iter = 40000) # iterations for analysis

output(model6)


