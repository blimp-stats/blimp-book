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

# FIT LINEAR GROWTH MODEL ----

model1 <- rblimp(
  data = trial,
  latent = 'icept linear', # Define latent variable for subjects factor
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
  seed = 90291, # Integer random number seed
  burn = 10000, # Warm up iterations
  iter = 10000) # Iterations for analysis

output(model1)

# FIT QUADRATIC GROWTH MODEL ----

model2 <- rblimp(
  data = trial,
  latent = 'icept linear quad', # Define latent variable for subjects factor
  model = '
    structural:
    icept ~ intercept;
    linear ~ intercept;
    quad ~ intercept;
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
  seed = 90291, # Integer random number seed
  burn = 30000, # Warm up iterations
  iter = 30000) # Iterations for analysis

output(model2)

# FIT QUADRATIC GROWTH MODEL WITH PREDICTOR ----

model3 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  latent = 'icept linear quad', # Define latent variable for subjects factor
  center = 'male',
  model = '
    structural:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11 male;
    quad ~ intercept@b2 drug@b21 male;
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
  parameters = '
    mu6_placebo = b0 + 6*b1 + 36*b1;
    mu6_drug = (b0 + b01) + 6*(b1 + b11) + 36*(b2 + b21);
    mu6_diff = mu6_drug - mu6_placebo;',
  seed = 90291, # Integer random number seed
  burn = 30000, # Warm up iterations
  iter = 30000) # Iterations for analysis

output(model3)
