# MODERATED NONLINEAR FACTOR ANALYSIS (MNLFA)

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/adhd.csv'

# create data frame from github data
adhd <- read.csv(data_url)

# FIT MODEL ----

# BRIAN NOTES ----
# how to access residual correlation draws for Wald test
# function to print standardized estimates? e.g., standarized(model)

# LATENT VARIABLE WITH DISCRETE INDICATORS

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/adhd.csv'

# create data frame from github data
adhd <- read.csv(data_url)

# FIT MODEL WITH MODERATED FACTOR MEAN AND VARIANCE ----

# factor mean and conditional variance fixed at 0 and 1
model1 <- rblimp(
  data = adhd,
  ordinal = 'male adhd study swan1:swan9',
  latent = 'attent',
  # fixed = 'study',
  center = 'alphaEEG age', 
  model = '
    structural:
    attent <- alphaEEG alphaEEG^2 age age^2 male adhd study;
    var(attent) <- 1@0 alphaEEG alphaEEG^2 age age^2 male adhd study;
    measurement:
    attent -> swan1@lo1 swan2:swan9;
    swan2 ~~ swan8;
    swan7 ~~ swan9;
    predictors:
    yjt(age-10) ~ intercept;
    yjt(alphaEEG) ~ intercept;
    alphaEEG age male adhd study ~~ alphaEEG age male adhd study;
    ',
  seed = 90291,
  burn = 50000,
  iter = 50000,
  nimps = 20)

# print output
output(model1)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model1)

# plot standardized residuals vs. predicted values
bivariate_plot(attent.residual ~ attent.predicted, standardize = 'y', model = model1)

# plot standardized residuals vs. predictors
bivariate_plot(x_vars = c('alphaEEG','age','male','adhd','study'), y_vars = 'attent.residual', model = model1, standardize = 'y')

# FIT MODEL WITH MODERATED MEASUREMENT INTERCEPTS AND LOADINGS ----

# factor mean and conditional variance fixed at 0 and 1
model2 <- rblimp(
  data = adhd,
  ordinal = 'swan1:swan9',
  nominal = 'male adhd study',
  latent = 'attent',
  # fixed = 'study',
  center = 'alphaEEG age', 
  model = '
    structural:
    attent <- alphaEEG alphaEEG^2 age age^2 male adhd study;
    var(attent) <- 1@0 alphaEEG alphaEEG^2 age age^2 male adhd study;
    invariant:
    attent -> swan4@lo4 swan5 swan6 swan9;
    noninvariant:
    attent alphaEEG alphaEEG^2 age age^2 male adhd study 
    attent*alphaEEG attent*age attent*male attent*adhd attent*study -> swan1@lo1 swan2 swan3 swan7 swan8;
    correlations:
    swan2 ~~ swan8;
    swan7 ~~ swan9;
    predictors:
    yjt(age-10) ~ intercept;
    yjt(alphaEEG) ~ intercept;
    alphaEEG age male adhd study ~~ alphaEEG age male adhd study;
    ',
  seed = 90291,
  burn = 50000,
  iter = 50000,
  nimps = 20)

# print output
output(model2)


