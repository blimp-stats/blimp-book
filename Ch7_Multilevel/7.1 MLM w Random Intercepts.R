# RANDOM INTERCEPT REGRESSION

# plotting functions
# source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source("/Users/craig/Dropbox/Claude/Projects/Blimp Book/rblimp_cleaned_functions.R")

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(ggplot2)
library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/mathprobsolve.csv'

# create data frame from github data
mathprobsolve <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT EMPTY MODEL FOR LEVEL-1 VARIABLES ----
#------------------------------------------------------------------------------#

# empty multivariate model for icc's
model1 <- rblimp(
  data = mathprobsolve,
  clusterid = 'school',
  ordinal = 'hispanic',
  model = 'intercept -> probsolve stanmath hispanic',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model1)

#------------------------------------------------------------------------------#
# FIT MODEL WITH COMBINED MODEL SPECIFICATION ----
#------------------------------------------------------------------------------#

# mixed model specification
mod <- rblimp(
    data = mathprobsolve,
    clusterid = 'school',
    ordinal = 'condition frlunch hispanic male',
    # fixed = 'probsolve1 condition',
    center = 'groupmean = stanmath hispanic',
    model = 'probsolve ~ intercept stanmath hispanic stanmath.mean hispanic.mean condition', 
    seed = 90291,
    burn = 10000,
    iter = 10000)

# print output
output(mod)

# mixed model specification
model2 <- rblimp(
  data = mathprobsolve,
  clusterid = 'school',
  nominal = 'condition frlunch',
  # fixed = 'probsolve1 condition',
  center = 'groupmean = psolvepre stanmath frlunch',
  model = 'psolvepst ~ intercept@mu0 psolvepre stanmath frlunch psolvepre.mean stanmath.mean frlunch.mean condition@diff', 
  parameters = 'd = diff / sqrt(psolvepst.totalvar);',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2,'psolvepst')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model2)

# plot distributions and residuals
univariate_plot(vars = c('psolvepst.school','psolvepst.residual'), model2)

# plot standardized level-2 residuals vs. level-2 predictors
bivariate_plot(x_vars = c('psolvepre.mean.school','stanmath.mean.school','frlunch.1.mean.school','condition'), 
               y_vars = 'psolvepst.school', standardize = 'y', model = model2)

# plot standardized level-1 residuals vs. level-1 predictors
bivariate_plot(x_vars = c('psolvepre','stanmath','frlunch'), 
               y_vars = 'psolvepst.residual', standardize = 'y', model = model2)

#------------------------------------------------------------------------------#
# FIT MODEL WITH LATENT VARIABLE SPECIFICATION ----
#------------------------------------------------------------------------------#

# random intercept as level-2 latent variable
model3 <- rblimp(
  data = mathprobsolve,
  clusterid = 'school',
  nominal = 'condition frlunch',
  latent = 'school = ranicept',
  # fixed = 'probsolve1 condition',
  center = 'groupmean = psolvepre stanmath frlunch',
  model = '
    ranicept ~ intercept psolvepre.mean stanmath.mean frlunch.mean condition@diff;
    psolvepst ~ intercept@ranicept psolvepre stanmath frlunch;',
  parameters = 'd = diff / sqrt(psolvepst.totalvar + ranicept.totalvar);',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model3)

# plot parameter distributions
posterior_plot(model3,'psolvepst')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model3)

# plot distributions and residuals
univariate_plot(vars = c('ranicept.residual','psolvepst.residual'), model3)

# plot standardized level-2 residuals vs. level-2 predictors
bivariate_plot(x_vars = c('psolvepre.mean.school','stanmath.mean.school','frlunch.1.mean.school','condition'), 
               y_vars = 'ranicept.residual', standardize = 'y', model = model3)

# plot standardized level-1 residuals vs. level-1 predictors
bivariate_plot(x_vars = c('psolvepre','stanmath','frlunch'), 
               y_vars = 'psolvepst.residual', standardize = 'y', model = model2)
