# BRIAN NOTES ----
# any way to get names in imputations more easily without [[]]?

# RANDOM INTERCEPT REGRESSION

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(rblimp)

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/problemsolving2level.csv'

# create data frame from github data
probsolve <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT EMPTY MODEL FOR LEVEL-1 VARIABLES ----
#------------------------------------------------------------------------------#

# empty multivariate model for icc's
model1 <- rblimp(
  data = probsolve,
  clusterid = 'school',
  ordinal = 'frlunch',
  model = 'intercept -> probsolvpost probsolvpre stanmath frlunch',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model1)

#------------------------------------------------------------------------------#
# FIT MODEL WITH COMBINED MODEL SPECIFICATION ----
#------------------------------------------------------------------------------#

# mixed model specification
model2 <- rblimp(
    data = probsolve,
    clusterid = 'school',
    nominal = 'condition frlunch',
    # fixed = 'probsolve1 condition',
    center = 'groupmean = probsolvpre stanmath frlunch',
    model = 'probsolvpost ~ intercept@mu0 probsolvpre stanmath frlunch probsolvpre.mean stanmath.mean frlunch.mean condition@diff', 
    parameters = 'd = diff / sqrt(probsolvpost.totalvar);',
    seed = 90291,
    burn = 10000,
    iter = 10000,
    nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2,'probsolvpost')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model2)

# plot distributions and residuals
univariate_plot(vars = c('probsolvpost.school','probsolvpost.residual'), model2)

# plot standardized level-2 residuals vs. level-2 predictors
bivariate_plot(x_vars = c('probsolvpre.mean.school','stanmath.mean.school','frlunch.1.mean.school','condition'), 
               y_vars = 'probsolvpost.school', standardize = 'y', model = model2)

# plot standardized level-1 residuals vs. level-1 predictors
bivariate_plot(x_vars = c('probsolvpre','stanmath','frlunch'), 
               y_vars = 'probsolvpost.residual', standardize = 'y', model = model2)

#------------------------------------------------------------------------------#
# FIT MODEL WITH LATENT VARIABLE SPECIFICATION ----
#------------------------------------------------------------------------------#

# random intercept as level-2 latent variable
model3 <- rblimp(
  data = probsolve,
  clusterid = 'school',
  nominal = 'condition frlunch',
  latent = 'school = ranicept',
  # fixed = 'probsolve1 condition',
  center = 'groupmean = probsolvpre stanmath frlunch',
  model = '
    ranicept ~ intercept probsolvpre.mean stanmath.mean frlunch.mean condition@diff;
    probsolvpost ~ intercept@ranicept probsolvpre stanmath frlunch;',
  parameters = 'd = diff / sqrt(probsolvpost.totalvar + ranicept.totalvar);',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model3)

# plot parameter distributions
posterior_plot(model3,'probsolvpost')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model3)

# plot distributions and residuals
univariate_plot(vars = c('ranicept.residual','probsolvpost.residual'), model3)

# plot standardized level-2 residuals vs. level-2 predictors
bivariate_plot(x_vars = c('probsolvpre.mean.school','stanmath.mean.school','frlunch.1.mean.school','condition'), 
               y_vars = 'ranicept.residual', standardize = 'y', model = model3)

# plot standardized level-1 residuals vs. level-1 predictors
bivariate_plot(x_vars = c('probsolvpre','stanmath','frlunch'), 
               y_vars = 'probsolvpost.residual', standardize = 'y', model = model2)
