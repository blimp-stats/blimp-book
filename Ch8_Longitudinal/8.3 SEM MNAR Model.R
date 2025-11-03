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

describeBy(trial[,c('dropout2','dropout3','dropout4','dropout5','dropout6')], trial$drug)

names(trial)

# FIT SHARED PARAMETER MODEL ----

# linear growth
model1 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout2:dropout6',
  latent = 'icept linear',
  center = 'male',
  fixed = 'male drug',
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
    severity6 ~~ severity6@res;
    missingness:
    dropout2 ~ icept@bi linear@bl drug;
    dropout3 ~ icept@bi linear@bl drug;
    dropout4 ~ icept@bi linear@bl drug;
    dropout5 ~ icept@bi linear@bl drug;
    dropout6 ~ icept@bi linear@bl drug;',
  parameters = '
    mu6_drug0 = b0 + 6*b1;
    mu6_drug1 = (b0 + b01) + 6*(b1 + b11);
    mu6_diff = mu6_drug1 - mu6_drug0;',
  seed = 90291, # integer random number seed
  burn = 300000, # warm up iterations
  iter = 300000) # iterations for analysis

output(model1)

# quadratic growth
model2 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout2:dropout6',
  latent = 'icept linear quad', 
  center = 'male',
  fixed = 'male drug',
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
    severity6 ~~ severity6@res;
    missingness: 
    dropout2 ~ icept@bi linear@bl drug;
    dropout3 ~ icept@bi linear@bl drug;
    dropout4 ~ icept@bi linear@bl drug;
    dropout5 ~ icept@bi linear@bl drug;
    dropout6 ~ icept@bi linear@bl drug;',
  parameters = '
    mu6_drug0 = b0 + 6*b1 + 36*b2;
    mu6_drug1 = (b0 + b01) + 6*(b1 + b11) + 36*(b2 + b21);
    mu6_diff = mu6_drug1 - mu6_drug0;',
  seed = 90291, # integer random number seed
  burn = 300000, # warm up iterations
  iter = 300000) # iterations for analysis

output(model2)

# FIT DIGGLE-KENWARD MODEL ----

# linear growth
model3 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout2:dropout6',
  latent = 'icept linear',
  center = 'male',
  fixed = 'male drug',
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
    severity6 ~~ severity6@res;
    missingness:
    dropout2 ~ severity1@mar severity2@mnar drug;
    dropout3 ~ severity2@mar severity3@mnar drug;
    dropout4 ~ severity3@mar severity4@mnar drug;
    dropout5 ~ severity4@mar severity5@mnar drug;
    dropout6 ~ severity5@mar severity6@mnar drug;',
  parameters = '
    mu6_drug0 = b0 + 6*b1;
    mu6_drug1 = (b0 + b01) + 6*(b1 + b11);
    mu6_diff = mu6_drug1 - mu6_drug0;',
  seed = 90291, # Integer random number seed
  burn = 300000, # Warm up iterations
  iter = 300000) # Iterations for analysis

output(model3)

# quadratic growth
model4 <- rblimp(
  data = trial,
  ordinal = 'male drug dropout2:dropout6',
  latent = 'icept linear quad',
  center = 'male',
  fixed = 'male drug',
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
    severity6 ~~ severity6@res;
    missingness:
    dropout2 ~ severity1@mar severity2@mnar drug;
    dropout3 ~ severity2@mar severity3@mnar drug;
    dropout4 ~ severity3@mar severity4@mnar drug;
    dropout5 ~ severity4@mar severity5@mnar drug;
    dropout6 ~ severity5@mar severity6@mnar drug;',
  parameters = '
    mu6_drug0 = b0 + 6*b1 + 36*b2;
    mu6_drug1 = (b0 + b01) + 6*(b1 + b11) + 36*(b2 + b21);
    mu6_diff = mu6_drug1 - mu6_drug0;',
  seed = 90291, # Integer random number seed
  burn = 300000, # Warm up iterations
  iter = 300000) # Iterations for analysis

output(model4)


