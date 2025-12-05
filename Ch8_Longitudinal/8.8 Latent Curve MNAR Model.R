# BRIAN NOTES ----
# how to handle wide-format dropout indicators in the code?
# possible to have add a function that stacks imputations?

#-------------------------------------------------------------------#
# SEM GROWTH MODELS WITH MNAR ASSUMPTION
#-------------------------------------------------------------------#

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

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/trial_wide.csv'

# create data frame from github data
trial <- read.csv(data_url)

#-------------------------------------------------------------------#
# FIT WU-CARROL SHARED PARAMETER LINEAR GROWTH MODEL ----
#-------------------------------------------------------------------#

# random intercepts and slopes predicting dropout
model1 <- rblimp(
  data = trial,
  ordinal = 'dropout1:dropout3',
  nominal = 'male drug',
  latent = 'icept linear', 
  # fixed = 'male drug',
  center = 'male',
  model = '
    structural:
    icept ~ intercept@icept_d0 drug@icept_diff male;
    linear ~ intercept@slp_d0 drug@slp_diff;
    icept ~~ linear;
    measurement:
    icept -> severity0@1 severity1@1 severity2@1 severity3@1; 
    linear -> severity0@0 severity1@1 severity2@2 severity3@3; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    severity0:severity3@res;
    missingness:
    dropout1 ~ (icept - icept_d0)@slp_icpt (linear - slp_d0)@slp_lin drug@slp_d;
    dropout2 ~ (icept - icept_d0)@slp_icpt (linear - slp_d0)@slp_lin drug@slp_d;
    dropout3 ~ (icept - icept_d0)@slp_icpt (linear - slp_d0)@slp_lin drug@slp_d;',
  parameters = '
    mu3_drug0 = icept_d0 + slp_d0*3;
    mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 300000, 
  iter = 300000,
  nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1)

#-------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#-------------------------------------------------------------------#

# compare marginal predicted probabilities by time and group
aggregate(
  cbind(dropout1.1.probability,dropout2.1.probability,dropout3.1.probability) ~ drug,
  data = stack_imputations(model1),
  FUN = mean
)

# plot imputed vs. observed values
imputation_plot(model1)

# plot distributions and residuals
latent_res <- c('icept.residual','linear.residual')
dropout_res <- paste0(c('dropout1','dropout2','dropout3'),'.residual')
indicator_res <- paste0(c('dropout1','dropout2','dropout3'),'.residual')
univariate_plot(vars = c(latent_res,dropout_res,indicator_res), model = model1)

# plot standardized residuals vs. predicted values
bivariate_plot(severity0.residual ~ severity0.predicted, standardize = 'y', model = model1)
bivariate_plot(severity1.residual ~ severity1.predicted, standardize = 'y', model = model1)
bivariate_plot(severity2.residual ~ severity2.predicted, standardize = 'y', model = model1)
bivariate_plot(severity3.residual ~ severity3.predicted, standardize = 'y', model = model1)
bivariate_plot(dropout1.residual ~ dropout1.predicted, standardize = 'y', model = model1)
bivariate_plot(dropout2.residual ~ dropout2.predicted, standardize = 'y', model = model1)
bivariate_plot(dropout3.residual ~ dropout3.predicted, standardize = 'y', model = model1)

# plot standardized residuals vs. predictor values
bivariate_plot(y_vars = latent_res, x_vars = c('drug','male'), discrete_x = c('drug','male'), standardize = 'y', model = model2)
bivariate_plot(y_vars = dropout_res, x_vars = c('icept.latent','linear.latent','drug'), discrete_x = 'drug', standardize = 'y', model = model2)
bivariate_plot(y_vars = indicator_res, x_vars = c('icept.latent','linear.latent'), standardize = 'y', model = model2)

#-------------------------------------------------------------------#
# FIT DIGGLE-KENWARD LINEAR GROWTH MODEL ----
#-------------------------------------------------------------------#

# current (missing) and previous (observed) outcome predicting dropout
model2 <- rblimp(
  data = trial,
  ordinal = 'dropout1:dropout3',
  nominal = 'male drug',
  latent = 'icept linear', 
  # fixed = 'male drug',
  center = 'male',
  model = '
    structural:
    icept ~ intercept@icept_d0 drug@icept_diff male;
    linear ~ intercept@slp_d0 drug@slp_diff;
    icept ~~ linear;
    measurement:
    icept -> severity0@1 severity1@1 severity2@1 severity3@1; 
    linear -> severity0@0 severity1@1 severity2@2 severity3@3; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    severity0:severity3@res;
    missingness:
    dropout1 ~ (severity0 - icept_d0)@slp_mar (severity1 - icept_d0)@slp_mnar drug@slp_d;
    dropout2 ~ (severity1 - icept_d0)@slp_mar (severity2 - icept_d0)@slp_mnar drug@slp_d;
    dropout3 ~ (severity2 - icept_d0)@slp_mar (severity3 - icept_d0)@slp_mnar drug@slp_d;',
  parameters = '
    mu3_drug0 = icept_d0 + slp_d0*3;
    mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 100000, 
  iter = 100000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2)

#-------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#-------------------------------------------------------------------#

# compare marginal predicted probabilities by time and group
aggregate(
  cbind(dropout1.1.probability,dropout2.1.probability,dropout3.1.probability) ~ drug,
  data = stack_imputations(model2),
  FUN = mean
)

# plot imputed vs. observed values
imputation_plot(model2)

# plot distributions and residuals
latent_res <- c('icept.residual','linear.residual')
dropout_res <- paste0(c('dropout1','dropout2','dropout3'),'.residual')
indicator_res <- paste0(c('dropout1','dropout2','dropout3'),'.residual')
univariate_plot(vars = c(latent_res,dropout_res,indicator_res), model = model2)

# plot standardized residuals vs. predicted values
bivariate_plot(severity0.residual ~ severity0.predicted, standardize = 'y', model = model2)
bivariate_plot(severity1.residual ~ severity1.predicted, standardize = 'y', model = model2)
bivariate_plot(severity2.residual ~ severity2.predicted, standardize = 'y', model = model2)
bivariate_plot(severity3.residual ~ severity3.predicted, standardize = 'y', model = model2)
bivariate_plot(dropout1.residual ~ dropout1.predicted, standardize = 'y', model = model2)
bivariate_plot(dropout2.residual ~ dropout2.predicted, standardize = 'y', model = model2)
bivariate_plot(dropout3.residual ~ dropout3.predicted, standardize = 'y', model = model2)

# plot standardized residuals vs. predictor values
bivariate_plot(y_vars = latent_res, x_vars = 'drug', discrete_x = 'drug', standardize = 'y', model = model2)
bivariate_plot(y_vars = 'dropout1.residual', x_vars = c('severity0','severity1','drug'), discrete_x = 'drug', standardize = 'y', model = model2)
bivariate_plot(y_vars = 'dropout2.residual', x_vars = c('severity1','severity2','drug'), discrete_x = 'drug', standardize = 'y', model = model2)
bivariate_plot(y_vars = 'dropout3.residual', x_vars = c('severity2','severity3','drug'), discrete_x = 'drug', standardize = 'y', model = model2)
bivariate_plot(y_vars = indicator_res, x_vars = c('icept.latent','linear.latent'), standardize = 'y', model = model2)

#-------------------------------------------------------------------#
# FIT WU-CARROL SHARED PARAMETER CURVILINEAR GROWTH MODEL ----
#-------------------------------------------------------------------#

# random intercepts and slopes predicting dropout
model3 <- rblimp(
  data = trial,
  ordinal = 'dropout1:dropout3',
  nominal = 'male drug',
  latent = 'icept linear quad', 
  # fixed = 'male drug',
  center = 'male',
  model = '
    structural:
    icept ~ intercept@icept_d0 drug@icept_diff male;
    linear ~ intercept@slp_d0 drug@slp_diff;
    quad ~ intercept@quad_d0 drug@quad_diff;
    quad ~~ quad@.001;
    icept ~~ linear;
    measurement:
    icept -> severity0@1 severity1@1 severity2@1 severity3@1; 
    linear -> severity0@0 severity1@1 severity2@2 severity3@3; 
    quad -> severity0@0 severity1@1 severity2@4 severity3@9; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    severity0:severity3@res;
    missingness:
    dropout1 ~ (icept - icept_d0)@slp_icpt (linear - slp_d0)@slp_lin drug@slp_d;
    dropout2 ~ (icept - icept_d0)@slp_icpt (linear - slp_d0)@slp_lin drug@slp_d;
    dropout3 ~ (icept - icept_d0)@slp_icpt (linear - slp_d0)@slp_lin drug@slp_d;',
  parameters = '
    mu3_drug0 = icept_d0 + slp_d0*3 + quad_d0*9;
    mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3 + (quad_d0 + quad_diff)*9;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291, 
  burn = 300000, 
  iter = 300000,
  nimps = 20) 

# print output
output(model3)

# plot parameter distributions
posterior_plot(model3)

#-------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#-------------------------------------------------------------------#

# compare marginal predicted probabilities by time and group
aggregate(
  cbind(dropout1.1.probability,dropout2.1.probability,dropout3.1.probability) ~ drug,
  data = stack_imputations(model3),
  FUN = mean
)

# plot imputed vs. observed values
imputation_plot(model3)

# plot distributions and residuals
latent_res <- c('icept.residual','linear.residual')
dropout_res <- paste0(c('dropout1','dropout2','dropout3'),'.residual')
indicator_res <- paste0(c('dropout1','dropout2','dropout3'),'.residual')
univariate_plot(vars = c(latent_res,dropout_res,indicator_res), model = model3)

# plot standardized residuals vs. predicted values
bivariate_plot(severity0.residual ~ severity0.predicted, standardize = 'y', model = model3)
bivariate_plot(severity1.residual ~ severity1.predicted, standardize = 'y', model = model3)
bivariate_plot(severity2.residual ~ severity2.predicted, standardize = 'y', model = model3)
bivariate_plot(severity3.residual ~ severity3.predicted, standardize = 'y', model = model3)
bivariate_plot(dropout1.residual ~ dropout1.predicted, standardize = 'y', model = model3)
bivariate_plot(dropout2.residual ~ dropout2.predicted, standardize = 'y', model = model3)
bivariate_plot(dropout3.residual ~ dropout3.predicted, standardize = 'y', model = model3)

# plot standardized residuals vs. predictor values
bivariate_plot(y_vars = latent_res, x_vars = c('drug','male'), discrete_x = c('drug','male'), standardize = 'y', model = model3)
bivariate_plot(y_vars = dropout_res, x_vars = c('icept.latent','linear.latent','drug'), discrete_x = 'drug', standardize = 'y', model = model3)
bivariate_plot(y_vars = indicator_res, x_vars = c('icept.latent','linear.latent'), standardize = 'y', model = model3)

#-------------------------------------------------------------------#
# FIT DIGGLE-KENWARD CURVILINEAR GROWTH MODEL ----
#-------------------------------------------------------------------#

# current (missing) and previous (observed) outcome predicting dropout
model4 <- rblimp(
  data = trial,
  ordinal = 'dropout1:dropout3',
  nominal = 'male drug',
  latent = 'icept linear quad', 
  # fixed = 'male drug',
  center = 'male',
  model = '
    structural:
    icept ~ intercept@icept_d0 drug@icept_diff male;
    linear ~ intercept@slp_d0 drug@slp_diff;
    quad ~ intercept@quad_d0 drug@quad_diff;
    quad ~~ quad@.001;
    icept ~~ linear;
    measurement:
    icept -> severity0@1 severity1@1 severity2@1 severity3@1; 
    linear -> severity0@0 severity1@1 severity2@2 severity3@3; 
    quad -> severity0@0 severity1@1 severity2@4 severity3@9; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    severity0:severity3@res;
    missingness:
    dropout1 ~ (severity0 - icept_d0)@slp_mar (severity1 - icept_d0)@slp_mnar drug@slp_d;
    dropout2 ~ (severity1 - icept_d0)@slp_mar (severity2 - icept_d0)@slp_mnar drug@slp_d;
    dropout3 ~ (severity2 - icept_d0)@slp_mar (severity3 - icept_d0)@slp_mnar drug@slp_d;',
  parameters = '
    mu3_drug0 = icept_d0 + slp_d0*3 + quad_d0*9;
    mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3 + (quad_d0 + quad_diff)*9;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291, 
  burn = 100000,
  iter = 100000,
  nimps = 20)

# print output
output(model4)

# plot parameter distributions
posterior_plot(model4)

#-------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#-------------------------------------------------------------------#

# compare marginal predicted probabilities by time and group
aggregate(
  cbind(dropout1.1.probability,dropout2.1.probability,dropout3.1.probability) ~ drug,
  data = stack_imputations(model4),
  FUN = mean
)

# plot imputed vs. observed values
imputation_plot(model4)

# plot distributions and residuals
latent_res <- c('icept.residual','linear.residual')
dropout_res <- paste0(c('dropout1','dropout2','dropout3'),'.residual')
indicator_res <- paste0(c('dropout1','dropout2','dropout3'),'.residual')
univariate_plot(vars = c(latent_res,dropout_res,indicator_res), model = model4)

# plot standardized residuals vs. predicted values
bivariate_plot(severity0.residual ~ severity0.predicted, standardize = 'y', model = model4)
bivariate_plot(severity1.residual ~ severity1.predicted, standardize = 'y', model = model4)
bivariate_plot(severity2.residual ~ severity2.predicted, standardize = 'y', model = model4)
bivariate_plot(severity3.residual ~ severity3.predicted, standardize = 'y', model = model4)
bivariate_plot(dropout1.residual ~ dropout1.predicted, standardize = 'y', model = model4)
bivariate_plot(dropout2.residual ~ dropout2.predicted, standardize = 'y', model = model4)
bivariate_plot(dropout3.residual ~ dropout3.predicted, standardize = 'y', model = model4)

# plot standardized residuals vs. predictor values
bivariate_plot(y_vars = latent_res, x_vars = 'drug', discrete_x = 'drug', standardize = 'y', model = model4)
bivariate_plot(y_vars = 'dropout1.residual', x_vars = c('severity0','severity1','drug'), discrete_x = 'drug', standardize = 'y', model = model4)
bivariate_plot(y_vars = 'dropout2.residual', x_vars = c('severity1','severity2','drug'), discrete_x = 'drug', standardize = 'y', model = model4)
bivariate_plot(y_vars = 'dropout3.residual', x_vars = c('severity2','severity3','drug'), discrete_x = 'drug', standardize = 'y', model = model4)
bivariate_plot(y_vars = indicator_res, x_vars = c('icept.latent','linear.latent'), standardize = 'y', model = model4)
