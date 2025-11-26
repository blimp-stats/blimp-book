# BRIAN NOTES ----
# Example of the naming conventions for yjt variables for clean-up
# messes up simple_plot and my plots

# YEO-JOHNSON NORMALIZING TRANSFORMATION FOR NONNORMAL CONTINUOUS VARIABLES

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflamm <- read.csv(data_url)

# FIT MODEL WITH NORMALIZED OUTCOME ----

# normalized outcome with yeo-johnson transformation
model1 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  center = 'age',
  # fixed = 'age female',
  model = 'yjt(dpdd - 6) ~ female els age inflam_sum', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1,'dpdd')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model1)

# plot standardized residuals vs. predicted values
bivariate_plot(dpdd.yjt.residual ~ dpdd.yjt.predicted, standardize = 'y', model = model1)

# plot standardized residuals vs. numeric predictors
bivariate_plot(y_vars = 'dpdd.yjt.residual', 
               x_vars = c('age','inflam_sum'),
               standardize = 'y',
               model = model1)

# FIT MODEL WITH NORMALIZED PREDICTOR ----

# predictor is normalized in its missing data model but skewed in the focal model
model2 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  center = 'age',
  # fixed = 'age female',
  model = '
  focal:
  yjt(dpdd - 6) ~ female els age inflam_sum;
  predictor:
  yjt(inflam_sum) ~ female els age;', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2,'dpdd')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model2)

# FIT MODEL WITH FULLY NORMALIZED PREDICTOR ----

# predictor is normalized in its missing data model and normalized in the focal model
model3 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  center = 'age',
  # fixed = 'age female',
  model = '
  focal:
  yjt(dpdd - 6) ~ female els age yjt(inflam_sum, shape_prior);
  predictor:
  yjt(inflam_sum, shape_prior) ~ female els age;', 
  parameters = 'shape_prior ~ uniform(0, 2);',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model3)

# plot parameter distributions
posterior_plot(model3,'dpdd')