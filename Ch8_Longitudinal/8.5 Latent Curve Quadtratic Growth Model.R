# LATENT CURVE QUADRATIC GROWTH MODELS

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#-------------------------------------------------------------------#
# LOAD R PACKAGES ----
#-------------------------------------------------------------------#

library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

#-------------------------------------------------------------------#
# READ DATA ----
#-------------------------------------------------------------------#

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/trial_wide.csv'

# create data frame from github data
trial <- read.csv(data_url)

#-------------------------------------------------------------------#
# FIT CURIVLINEAR GROWTH MODEL ----
#-------------------------------------------------------------------#

# quadratic fixed effect
model1 <- rblimp(
  data = trial,
  latent = 'icept linear quad',
  model = '
    structural:
    intercept -> icept linear quad;
    quad ~~ quad@.001;
    icept ~~ linear;
    measurement:
    icept -> severity0@1 severity1@1 severity2@1 severity3@1; 
    linear -> severity0@0 severity1@1 severity2@2 severity3@3; 
    quad -> severity0@0 severity1@1 severity2@4 severity3@9; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    severity0:severity3@res;',
  seed = 90291, 
  burn = 30000,
  iter = 30000,
  nimps = 20) 

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1)

# quadratic fixed effect and variance
model2 <- rblimp(
  data = trial,
  latent = 'icept linear quad',
  model = '
    structural:
    intercept -> icept linear quad;
    icept linear quad ~~ icept linear quad;
    measurement:
    icept -> severity0@1 severity1@1 severity2@1 severity3@1; 
    linear -> severity0@0 severity1@1 severity2@2 severity3@3; 
    quad -> severity0@0 severity1@1 severity2@4 severity3@9; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    severity0:severity3@res;',
  seed = 90291, 
  burn = 30000, 
  iter = 30000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2)

#-------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#-------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model1)

# plot distributions and residuals
latents <- c('icept.latent','linear.latent','quad.latent')
residuals <- paste0(c('severity0','severity1','severity2','severity3'),'.residual')
univariate_plot(vars = c(latents,residuals), model = model1)
univariate_plot(vars = c(latents,residuals), model = model2)

# plot standardized residuals vs. predicted values
bivariate_plot(severity0.residual ~ severity0.predicted, standardize = 'y', model = model1)
bivariate_plot(severity1.residual ~ severity1.predicted, standardize = 'y', model = model1)
bivariate_plot(severity2.residual ~ severity2.predicted, standardize = 'y', model = model1)
bivariate_plot(severity3.residual ~ severity3.predicted, standardize = 'y', model = model1)

# plot standardized residuals vs. latent variable scores
bivariate_plot(x_vars = c('icept.latent','linear.latent'), y_vars = residuals, model = model1, standardize = 'y')

# plot pairs of indicator residuals
bivariate_plot(vars = residuals, model = model1, poly_degree = 1, standardize = 'both')

#-------------------------------------------------------------------#
# FIT CURVILINEAR GROWTH MODEL WITH PREDICTORS ----
#-------------------------------------------------------------------#

# quadratic fixed effect with predictors
model3 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  latent = 'icept linear quad', 
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
    severity0:severity3@res;',
  parameters = '
    mu3_drug0 = icept_d0 + slp_d0*3 + quad_d0*9;
    mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3 + (quad_d0 + quad_diff)*9;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 50000, 
  iter = 50000,
  nimps = 20)

# print output
output(model3)

# plot parameter distributions
posterior_plot(model3)

#-------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#-------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model3)

# plot distributions and residuals
latents <- c('icept.latent','linear.latent','quad.latent')
residuals <- paste0(c('icept','linear','quad','severity0','severity1','severity2','severity3'),'.residual')
univariate_plot(vars = c(latents,residuals), model = model3)

# plot standardized residuals vs. predicted values
bivariate_plot(severity0.residual ~ severity0.predicted, standardize = 'y', model = model3)
bivariate_plot(severity1.residual ~ severity1.predicted, standardize = 'y', model = model3)
bivariate_plot(severity2.residual ~ severity2.predicted, standardize = 'y', model = model3)
bivariate_plot(severity3.residual ~ severity3.predicted, standardize = 'y', model = model3)

# plot standardized residuals vs. latent variable scores
bivariate_plot(x_vars = c('icept.latent','linear.latent'), y_vars = residuals, model = model3, standardize = 'y')

# plot pairs of indicator residuals
bivariate_plot(vars = residuals, model = model3, poly_degree = 1, standardize = 'both')


