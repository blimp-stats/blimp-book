# MULTILEVEL MODEL WITH HETEROGENEOUS WITHIN-CLUSTER VARIATION

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/diary.csv'

# create data frame from github data
diary <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT MODEL ----
#------------------------------------------------------------------------------#

# location-scale model
model <- rblimp(
  data = diary,
  clusterid = 'person',
  nominal = 'female',
  latent = 'person = ranicept ranslope logvar;',
  center = 'groupmean = pain; grandmean = pain.mean stress female',
  model = '
    level2:
    ranicept ~ intercept pain.mean stress female;
    ranslope ~ intercept stress;
    ranicept ~~ ranslope;
    level1:
    posaff ~ intercept@ranicept pain@ranslope;
    scale:
    logvar ~ intercept stress;
    logvar ~~ ranicept ranslope;
    var(posaff) ~ intercept@logvar pain;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model)

# plot parameter distributions
posterior_plot(model,'posaff')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model)

# plot distributions and residuals
univariate_plot(vars = c('ranicept.latent','ranicept.residual','ranslope.latent','ranslope.residual','logvar.latent', 'logvar.residual','posaff.residual'), model)

# plot standardized level-2 residuals vs. level-2 predictors
bivariate_plot(x_vars = c('pain.mean.person','stress','female'), 
               y_vars = c('ranicept.residual','ranslope.residual','logvar.residual'), standardize = 'y', model = model)

# plot standardized level-1 residuals vs. level-1 predictors
bivariate_plot(posaff.residual ~ pain, standardize = 'y', model = model)
