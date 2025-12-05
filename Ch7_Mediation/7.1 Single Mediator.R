# SINGLE-MEDIATOR MODEL

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/discrimination.csv'

# create data frame from github data
discrimination <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT SINGLE MEDIATOR MODEL ----
#------------------------------------------------------------------------------#

model <- rblimp(
  data = discrimination,
  nominal = 'female',
  model = '
    apath:
    familism ~ discrim@a female age;
    bpath:
    internalize ~ discrim familism@b female age;',
  parameters = '
    indirect = a*b;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model)

# plot parameter distributions
posterior_plot(model)

# plot distribution of indirect effect
posterior_plot(model, 'indirect')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model)

# plot distributions and residuals
univariate_plot(vars = c('familism.residual','internalize.residual'), model)

# plot standardized residuals vs. predicted values
bivariate_plot(familism.residual ~ familism.predicted, standardize = 'y', model = model)
bivariate_plot(internalize.residual ~ internalize.predicted, standardize = 'y', model = model)

# plot standardized residuals vs. numeric predictors
bivariate_plot(y_vars = 'familism.residual', 
               x_vars = c('discrim','female','age'),
               discrete_x = c('female','age'),
               standardize = 'y',
               model = model)

bivariate_plot(y_vars = 'internalize.residual', 
               x_vars = c('discrim','familism','female','age'),
               discrete_x = c('female','age'),
               standardize = 'y',
               model = model)
