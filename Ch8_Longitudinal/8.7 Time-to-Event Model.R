# TIME-TO-EVENT (DISCRETE TIME) SURVIVAL MODEL

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# stack imputations function
stack_imputations <- function(model)
  do.call(rbind, lapply(seq_along(model@imputations), \(i) transform(model@imputations[[i]], .imp = i)))

#-------------------------------------------------------------------#
# LOAD R PACKAGES ----
#-------------------------------------------------------------------#

library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

#-------------------------------------------------------------------#
# READ DATA ----
#-------------------------------------------------------------------#

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/trial_stacked.csv'

# create data frame from github data
trial <- read.csv(data_url)

#-------------------------------------------------------------------#
# FIT UNRESTRICTED HAZARD MODEL ----
#-------------------------------------------------------------------#

# observed missing data rates by time and group
aggregate(dropout_man ~ week, data = trial, mean)

# dummy coded time
model1 <- rblimp(
  data = trial,
  clusterid = 'person; timeid: week; dropout: dropout = severity',
  # timeid = 'week',
  # dropout = 'dropoutb = severity',
  model = 'dropout ~ intercept@-3 (week==1) (week==2) (week==3) | intercept@0',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1)

#-------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#-------------------------------------------------------------------#

# compare marginal predicted probabilities by time
aggregate(dropout.1.probability ~ week, data = stack_imputations(model1), mean)

# plot imputed vs. observed values
imputation_plot(model1)

# plot distributions and residuals
univariate_plot(vars = c('dropout.latent','dropout.residual'), model1)

# plot dropout probabilities by time
bivariate_plot(dropout.1.probability ~ week, model1, discrete_x = 'week')

# plot standardized residuals vs. predicted values
bivariate_plot(dropout.residual ~ dropout.predicted, standardize = 'y', model = model1)

# plot standardized residuals vs. time
bivariate_plot(dropout.residual ~ week, model = model1, discrete_x = 'week')

#-------------------------------------------------------------------#
# FIT LINEAR HAZARD MODEL ----
#-------------------------------------------------------------------#

# linear time
model2 <- rblimp(
  data = trial,
  clusterid = 'person; timeid: week; dropout: dropout = severity',
  # timeid = 'week',
  # dropout = 'dropoutb = severity',
  model = 'dropout ~ intercept@-3 week | intercept@0',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2)

#-------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#-------------------------------------------------------------------#

# compare marginal predicted probabilities by time
aggregate(dropout.1.probability ~ week, data = stack_imputations(model2), mean)

# plot imputed vs. observed values
imputation_plot(model2)

# plot distributions and residuals
univariate_plot(vars = c('dropout.latent','dropout.residual'), model2)

# plot dropout probabilities by week
bivariate_plot(dropout.1.probability ~ week, model2, discrete_x = 'week')

# plot standardized residuals vs. predicted values
bivariate_plot(dropout.residual ~ dropout.predicted, standardize = 'y', model = model2)

# plot standardized residuals vs. time
bivariate_plot(dropout.residual ~ week, model = model2, discrete_x = 'week')

#-------------------------------------------------------------------#
# FIT QUADRATIC HAZARD MODEL ----
#-------------------------------------------------------------------#

# linear time
model3 <- rblimp(
  data = trial,
  clusterid = 'person; timeid: week; dropout: dropout = severity',
  # timeid = 'week',
  # dropout = 'dropoutb = severity',
  model = 'dropout ~ intercept@-3 week week^2 | intercept@0',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model3)

# plot parameter distributions
posterior_plot(model3)

#-------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#-------------------------------------------------------------------#

# compare marginal predicted probabilities by time
aggregate(dropout.1.probability ~ week, data = stack_imputations(model3), mean)

# plot imputed vs. observed values
imputation_plot(model3)

# plot distributions and residuals
univariate_plot(vars = c('dropout.latent','dropout.residual'), model3)

# plot dropout probabilities by week
bivariate_plot(dropout.1.probability ~ week, model3, discrete_x = 'week')

# plot standardized residuals vs. predicted values
bivariate_plot(dropout.residual ~ dropout.predicted, standardize = 'y', model = model3)

# plot standardized residuals vs. week
bivariate_plot(dropout.residual ~ week, model = model3, discrete_x = 'week')

#-------------------------------------------------------------------#
# FIT HAZARD MODEL WITH PREDICTOR ----
#-------------------------------------------------------------------#

# observed missing data rates by time and group
aggregate(dropout_man ~ drug + week, data = trial, mean)

# main effect of group
model4 <- rblimp(
  data = trial,
  clusterid = 'person; timeid: week; dropout: dropout = severity',
  # timeid = 'week',
  # dropout = 'dropoutb = severity',
  model = 'dropout ~ intercept@-3 (week==1) (week==2) (week==3) drug*(week>0) | intercept@0',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model4)

# plot parameter distributions
posterior_plot(model4)

#-------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#-------------------------------------------------------------------#

# compare marginal predicted probabilities by time and group
aggregate(dropout.1.probability ~ week + drug, data = stack_imputations(model4), mean)

# plot imputed vs. observed values
imputation_plot(model4)

# plot distributions and residuals
univariate_plot(vars = c('dropout.latent','dropout.residual'), model4)

# plot standardized residuals vs. predicted values
bivariate_plot(dropout.residual ~ dropout.predicted, standardize = 'y', model = model4)

# plot standardized residuals vs. week
bivariate_plot(dropout.residual ~ week, model = model4, discrete_x = 'week')

#-------------------------------------------------------------------#
# FIT HAZARD MODEL WITH GROUP-BY-TIME INTERACTION ----
#-------------------------------------------------------------------#

# observed missing data rates by time and group
aggregate(dropout_man ~ drug + week, data = trial, mean)

# group-by-time interaction
model5 <- rblimp(
  data = trial,
  clusterid = 'person; timeid: week; dropout: dropout = severity',
  # timeid = 'week',
  # dropout = 'dropoutb = severity',
  model = 'dropout ~ intercept@-3 (week==1) (week==2) (week==3)
    (week==1)*drug@d1 (week==2)*drug@d2 (week==3)*drug@d3 | intercept@0',
  waldtest = 'd1 = d2; d2 = d3',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model5)

# plot parameter distributions
posterior_plot(model5)

#-------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#-------------------------------------------------------------------#

# compare marginal predicted probabilities by time and group
aggregate(dropout.1.probability ~ week + drug, data = stack_imputations(model5), mean)

# plot imputed vs. observed values
imputation_plot(model5)

# plot distributions and residuals
univariate_plot(vars = c('dropout.latent','dropout.residual'), model5)

# plot standardized residuals vs. predicted values
bivariate_plot(dropout.residual ~ dropout.predicted, standardize = 'y', model = model5)

# plot standardized residuals vs. week
bivariate_plot(dropout.residual ~ week, model = model5, discrete_x = 'week')
