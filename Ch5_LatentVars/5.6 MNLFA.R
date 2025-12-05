# MODERATED NONLINEAR FACTOR ANALYSIS (MNLFA)

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/adhd.csv'

# create data frame from github data
adhd <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT MODEL WITH MODERATED FACTOR MEAN AND VARIANCE ----
#------------------------------------------------------------------------------#

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

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot distributions and residuals
residuals <- paste0(c('attent','alphaEEG.yjt','age.yjt','male','adhd','study'),'.residual')
univariate_plot(vars = c('attent.latent', residuals), model = model1)

# plot imputed vs. observed values
imputation_plot(model1)

# plot standardized residuals vs. predicted values
bivariate_plot(attent.residual ~ attent.predicted, standardize = 'y', model = model1)

# plot standardized residuals vs. predictors
bivariate_plot(x_vars = c('alphaEEG','age','male','adhd','study'), y_vars = 'attent.residual', model = model1, standardize = 'y')

#------------------------------------------------------------------------------#
# FIT MODEL WITH MODERATED MEASUREMENT INTERCEPTS AND LOADINGS ----
#------------------------------------------------------------------------------#

# factor mean and conditional variance fixed at 0 and 1
model2 <- rblimp(
  data = adhd,
  ordinal = 'male adhd study swan1:swan9',
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
  burn = 70000,
  iter = 70000,
  nimps = 20)

# print output
output(model2)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model2)

# plot distributions and residuals
residuals <- paste0('swan',1:9,'.residual')
univariate_plot(vars = residuals, model = model2)

# plot standardized residuals vs. predicted values
bivariate_plot(swan1.residual ~ swan1.predicted, standardize = 'y', model = model2)
bivariate_plot(swan2.residual ~ swan2.predicted, standardize = 'y', model = model2)
bivariate_plot(swan3.residual ~ swan3.predicted, standardize = 'y', model = model2)
bivariate_plot(swan4.residual ~ swan4.predicted, standardize = 'y', model = model2)
bivariate_plot(swan5.residual ~ swan5.predicted, standardize = 'y', model = model2)
bivariate_plot(swan6.residual ~ swan6.predicted, standardize = 'y', model = model2)
bivariate_plot(swan7.residual ~ swan7.predicted, standardize = 'y', model = model2)
bivariate_plot(swan8.residual ~ swan8.predicted, standardize = 'y', model = model2)
bivariate_plot(swan9.residual ~ swan9.predicted, standardize = 'y', model = model2)

# plot standardized residuals vs. predictors
bivariate_plot(x_vars = c('attent.latent','alphaEEG','age','male','adhd','study'),
               y_vars = residuals, model = model2, standardize = 'y')

# plot pairs of indicator residuals
bivariate_plot(vars = residuals, model = model1, poly_degree = 1, standardize = 'both')



