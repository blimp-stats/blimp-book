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
    icept ~ intercept;
    linear ~ intercept;
    icept ~ linear;
    measurement:
    severity0 ~ intercept@0 icept@1 linear@0; 
    severity1 ~ intercept@0 icept@1 linear@1; 
    severity2 ~ intercept@0 icept@1 linear@2; 
    severity3 ~ intercept@0 icept@1 linear@3; 
    severity4 ~ intercept@0 icept@1 linear@4; 
    severity5 ~ intercept@0 icept@1 linear@5; 
    severity6 ~ intercept@0 icept@1 linear@6;
    residual:
    severity0 ~~ severity0@res;
    severity1 ~~ severity1@res;
    severity2 ~~ severity2@res;
    severity3 ~~ severity3@res;
    severity4 ~~ severity4@res;
    severity5 ~~ severity5@res;
    severity6 ~~ severity6@res;',
  seed = 90291, # integer random number seed
  burn = 10000, # warm up iterations
  iter = 10000) # iterations for analysis

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
    severity0 ~ intercept@0 icept@1 linear@0; 
    severity1 ~ intercept@0 icept@1 linear@1; 
    severity2 ~ intercept@0 icept@1 linear@2; 
    severity3 ~ intercept@0 icept@1 linear@3; 
    severity4 ~ intercept@0 icept@1 linear@4; 
    severity5 ~ intercept@0 icept@1 linear@5; 
    severity6 ~ intercept@0 icept@1 linear@6;
    residual:
    severity0 ~~ severity0@res;
    severity1 ~~ severity1@res;
    severity2 ~~ severity2@res;
    severity3 ~~ severity3@res;
    severity4 ~~ severity4@res;
    severity5 ~~ severity5@res;
    severity6 ~~ severity6@res;',
  parameters = '
    mu6_drug0 = b0 + 6*b1;
    mu6_drug1 = (b0 + b01) + 6*(b1 + b11);
    mu6_diff = mu6_drug1 - mu6_drug0;',
  seed = 90291, # integer random number seed
  burn = 10000, # warm up iterations
  iter = 10000) # iterations for analysis

output(model3)

# FIT QUADRATIC GROWTH MODEL ----

# quadratic fixed effect but no quadratic variance
model4 <- rblimp(
  data = trial,
  latent = 'icept linear quad',
  model = '
    structural:
    intercept -> icept linear quad;
    quad ~~ quad@.001;
    icept ~~ linear;
    measurement:
    severity0 ~ intercept@0 icept@1 linear@0 quad@0; 
    severity1 ~ intercept@0 icept@1 linear@1 quad@1; 
    severity2 ~ intercept@0 icept@1 linear@2 quad@4; 
    severity3 ~ intercept@0 icept@1 linear@3 quad@9; 
    severity4 ~ intercept@0 icept@1 linear@4 quad@16; 
    severity5 ~ intercept@0 icept@1 linear@5 quad@25; 
    severity6 ~ intercept@0 icept@1 linear@6 quad@36;
    residual:
    severity0 ~~ severity0@res;
    severity1 ~~ severity1@res;
    severity2 ~~ severity2@res;
    severity3 ~~ severity3@res;
    severity4 ~~ severity4@res;
    severity5 ~~ severity5@res;
    severity6 ~~ severity6@res;',
  seed = 90291, # integer random number seed
  burn = 20000, # warm up iterations
  iter = 20000) # iterations for analysis

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
    severity0 ~ intercept@0 icept@1 linear@0 quad@0; 
    severity1 ~ intercept@0 icept@1 linear@1 quad@1; 
    severity2 ~ intercept@0 icept@1 linear@2 quad@4; 
    severity3 ~ intercept@0 icept@1 linear@3 quad@9; 
    severity4 ~ intercept@0 icept@1 linear@4 quad@16; 
    severity5 ~ intercept@0 icept@1 linear@5 quad@25; 
    severity6 ~ intercept@0 icept@1 linear@6 quad@36;
    residual:
    severity0 ~~ severity0@res;
    severity1 ~~ severity1@res;
    severity2 ~~ severity2@res;
    severity3 ~~ severity3@res;
    severity4 ~~ severity4@res;
    severity5 ~~ severity5@res;
    severity6 ~~ severity6@res;',
  seed = 90291, # integer random number seed
  burn = 20000, # warm up iterations
  iter = 20000) # iterations for analysis

output(model5)

# quadratic fixed effect with predictors
model6 <- rblimp(
  data = trial,
  latent = 'icept linear quad',
  model = '
    structural:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    quad ~ intercept@b2 drug@b21;
    quad ~~ quad@.001;
    icept ~~ linear;
    measurement:
    severity0 ~ intercept@0 icept@1 linear@0 quad@0; 
    severity1 ~ intercept@0 icept@1 linear@1 quad@1; 
    severity2 ~ intercept@0 icept@1 linear@2 quad@4; 
    severity3 ~ intercept@0 icept@1 linear@3 quad@9; 
    severity4 ~ intercept@0 icept@1 linear@4 quad@16; 
    severity5 ~ intercept@0 icept@1 linear@5 quad@25; 
    severity6 ~ intercept@0 icept@1 linear@6 quad@36;
    residual:
    severity0 ~~ severity0@res;
    severity1 ~~ severity1@res;
    severity2 ~~ severity2@res;
    severity3 ~~ severity3@res;
    severity4 ~~ severity4@res;
    severity5 ~~ severity5@res;
    severity6 ~~ severity6@res;',
  parameters = '
    mu6_drug0 = b0 + 6*b1 + 36*b2;
    mu6_drug1 = (b0 + b01) + 6*(b1 + b11) + 36*(b2 + b21);
    mu6_diff = mu6_drug1 - mu6_drug0;',
  seed = 90291, # integer random number seed
  burn = 30000, # warm up iterations
  iter = 30000) # iterations for analysis

output(model6)


