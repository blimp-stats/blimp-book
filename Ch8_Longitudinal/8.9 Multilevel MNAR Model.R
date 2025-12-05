# BRIAN NOTES ----
# when specifying nonexistent timeid, error says that time is not at lowest level (rather than nonexistent)
# when omitting timeid, error references time variable (not timeid variable)
# timeid variable automatically becomes fixed (as it should, but any downstream consequences?)
# timeid does not appear in missing data rate table
# possible to have add a function that stacks imputations?

# MULTILEVEL GROWTH MODEL WITH MNAR ASSUMPTION

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
# FIT WU-CARROL SHARED PARAMETER LINEAR GROWTH MODEL ----
#-------------------------------------------------------------------#

# random intercepts and slopes predicting dropout
model1 <- rblimp(
  data = trial,
  nominal = 'male drug',
  clusterid = 'person; timeid: week; dropout: dropout = severity',
  # timeid = 'week',
  # dropout = 'dropoutb = severity',
  # fixed = 'male drug',
  center = 'grandmean = male',
  latent = 'person = icept linear;',
  model = '
    level2:
    icept ~ intercept@icept_d0 drug@icept_diff male;
    linear ~ intercept@slp_d0 drug@slp_diff;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear;
    missingness:
    dropout ~ intercept@-3 (week==1) (week==2) (week==3) 
      drug*(week>0)
      # (week==1)*drug (week==2)*drug (week==3)*drug
      (week > 0)*icept (week > 0)*linear | 1@0;',
  parameters = '
    mu3_drug0 = icept_d0 + slp_d0*3;
    mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 40000,
  iter = 40000,
  nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1)

# plot predicted values by time
bivariate_plot(severity.predicted ~ week, model = model1)

#-------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#-------------------------------------------------------------------#

# compare marginal predicted probabilities by time and group
aggregate(dropout.1.probability ~ drug + week, data = stack_imputations(model1), mean)

# plot imputed vs. observed values
imputation_plot(model1)

# plot distributions and residuals
univariate_plot(vars = c('icept.latent','linear.latent','severity.residual'), model1)

# plot standardized residuals vs. predicted values
bivariate_plot(severity.residual ~ severity.predicted, standardize = 'y', model = model1)
bivariate_plot(dropout.residual ~ dropout.predicted, standardize = 'y', model = model1)

# plot standardized residuals vs. predictor values
bivariate_plot(y_vars = c('icept.residual','linear.residual'), x_vars = c('male','drug'), discrete_x = c('male','drug'), standardize = 'y', model = model1)
bivariate_plot(y_vars = 'severity.residual', x_vars = c('week'), discrete_x = c('week'), standardize = 'y', model = model1)
bivariate_plot(y_vars = 'dropout.residual', x_vars = c('week','drug','icept','linear'), discrete_x = c('week','drug'), standardize = 'y', model = model1)

#-------------------------------------------------------------------#
# FIT DIGGLE-KENWARD LINEAR GROWTH MODEL ----
#-------------------------------------------------------------------#

# current (missing) and previous (observed) outcome predicting dropout
model2 <- rblimp(
  data = trial,
  nominal = 'male drug',
  clusterid = 'person; timeid: week; dropout: dropout = severity',
  # timeid = 'week',
  # dropout = 'dropoutb = severity',
  # fixed = 'male drug',
  center = 'grandmean = male',
  latent = 'person = icept linear;',
  model = '
    level2:
    icept ~ intercept@icept_d0 drug@icept_diff male;
    linear ~ intercept@slp_d0 drug@slp_diff;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear;
    missingness:
    dropout ~ intercept@-3 (week==1) (week==2) (week==3) 
      drug*(week>0)
      # (week==1)*drug (week==2)*drug (week==3)*drug
      (week > 0)*severity (week > 0)*severity.lag | 1@0;',
  parameters = '
    mu3_drug0 = icept_d0 + slp_d0*3;
    mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 40000,
  iter = 40000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2)

# plot predicted values by time
bivariate_plot(severity.predicted ~ week, model = model2)

#-------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#-------------------------------------------------------------------#

# compare marginal predicted probabilities by time and group
aggregate(dropout.1.probability ~ drug + week, data = stack_imputations(model2), mean)

# plot imputed vs. observed values
imputation_plot(model2)

# plot distributions and residuals
univariate_plot(vars = c('icept.latent','linear.latent','severity.residual'), model2)

# plot standardized residuals vs. predicted values
bivariate_plot(severity.residual ~ severity.predicted, standardize = 'y', model = model2)
bivariate_plot(dropout.residual ~ dropout.predicted, standardize = 'y', model = model2)

# plot standardized residuals vs. predictor values
bivariate_plot(y_vars = c('icept.residual','linear.residual'), x_vars = c('male','drug'), discrete_x = c('male','drug'), standardize = 'y', model = model2)
bivariate_plot(y_vars = 'severity.residual', x_vars = c('week'), discrete_x = c('week'), standardize = 'y', model = model2)
bivariate_plot(y_vars = 'dropout.residual', x_vars = c('week','severity','severity.lag1'), discrete_x = c('week','drug'), standardize = 'y', model = model2)

#-------------------------------------------------------------------#
# FIT WU-CARROL SHARED PARAMETER CURVILINEAR GROWTH MODEL ----
#-------------------------------------------------------------------#

# random intercepts and slopes predicting dropout
model3 <- rblimp(
  data = trial,
  nominal = 'male drug',
  clusterid = 'person; timeid: week; dropout: dropout = severity',
  # timeid = 'week',
  # dropout = 'dropoutb = severity',
  # fixed = 'male drug',
  center = 'grandmean = male',
  latent = 'person = icept linear;',
  model = '
    level2:
    icept ~ intercept@icept_d0 drug@icept_diff male;
    linear ~ intercept@slp_d0 drug@slp_diff;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear week^2@quad_d0 week^2*drug@quad_diff;
    missingness:
    dropout ~ intercept@-3 (week==1) (week==2) (week==3) 
      drug*(week>0)
      # (week==1)*drug (week==2)*drug (week==3)*drug
      (week > 0)*icept (week > 0)*linear | 1@0;',
  parameters = '
    mu3_drug0 = icept_d0 + slp_d0*3 + quad_d0*9;
    mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3 + (quad_d0 + quad_diff)*9;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 30000,
  iter = 30000,
  nimps = 20)

# print output
output(model3)

# plot parameter distributions
posterior_plot(model3)

#-------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#-------------------------------------------------------------------#

# compare marginal predicted probabilities by time and group
aggregate(dropout.1.probability ~ drug + week, data = stack_imputations(model3), mean)

# plot imputed vs. observed values
imputation_plot(model3)

# plot distributions and residuals
univariate_plot(vars = c('icept.latent','linear.latent','severity.residual'), model3)

# plot standardized residuals vs. predicted values
bivariate_plot(severity.residual ~ severity.predicted, standardize = 'y', model = model3)
bivariate_plot(dropout.residual ~ dropout.predicted, standardize = 'y', model = model3)

# plot standardized residuals vs. predictor values
bivariate_plot(y_vars = c('icept.residual','linear.residual'), x_vars = c('male','drug'), discrete_x = c('male','drug'), standardize = 'y', model = model3)
bivariate_plot(y_vars = 'severity.residual', x_vars = c('week'), discrete_x = c('week'), standardize = 'y', model = model3)
bivariate_plot(y_vars = 'dropout.residual', x_vars = c('week','drug','icept','linear'), discrete_x = c('week','drug'), standardize = 'y', model = model3)

#-------------------------------------------------------------------#
# FIT DIGGLE-KENWARD CURVILINEAR GROWTH MODEL ----
#-------------------------------------------------------------------#

# current (missing) and previous (observed) outcome predicting dropout
model4 <- rblimp(
  data = trial,
  nominal = 'male drug',
  clusterid = 'person; timeid: week; dropout: dropout = severity',
  # timeid = 'week',
  # dropout = 'dropoutb = severity',
  # fixed = 'male drug',
  center = 'grandmean = male',
  latent = 'person = icept linear;',
  model = '
    level2:
    icept ~ intercept@icept_d0 drug@icept_diff male;
    linear ~ intercept@slp_d0 drug@slp_diff;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear week^2@quad_d0 week^2*drug@quad_diff;
    missingness:
    dropout ~ intercept@-3 (week==1) (week==2) (week==3) 
      drug*(week>0)
      # (week==1)*drug (week==2)*drug (week==3)*drug
      (week > 0)*severity (week > 0)*severity.lag | 1@0;',
  parameters = '
    mu3_drug0 = icept_d0 + slp_d0*3 + quad_d0*9;
    mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3 + (quad_d0 + quad_diff)*9;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 30000,
  iter = 30000,
  nimps = 20)

# print output
output(model4)

# plot parameter distributions
posterior_plot(model4)

#-------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#-------------------------------------------------------------------#

# compare marginal predicted probabilities by time and group
aggregate(dropout.1.probability ~ drug + week, data = stack_imputations(model4), mean)

# plot imputed vs. observed values
imputation_plot(model4)

# plot distributions and residuals
univariate_plot(vars = c('icept.latent','linear.latent','severity.residual'), model4)

# plot standardized residuals vs. predicted values
bivariate_plot(severity.residual ~ severity.predicted, standardize = 'y', model = model4)
bivariate_plot(dropout.residual ~ dropout.predicted, standardize = 'y', model = model4)

# plot standardized residuals vs. predictor values
bivariate_plot(y_vars = c('icept.residual','linear.residual'), x_vars = c('male','drug'), discrete_x = c('male','drug'), standardize = 'y', model = model4)
bivariate_plot(y_vars = 'severity.residual', x_vars = c('week'), discrete_x = c('week'), standardize = 'y', model = model4)
bivariate_plot(y_vars = 'dropout.residual', x_vars = c('week','severity','severity.lag1'), discrete_x = c('week','drug'), standardize = 'y', model = model4)
