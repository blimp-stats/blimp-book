# THREE-LEVEL MULTILEVEL GROWTH MODEL

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/problemsolving3level.csv'

# create data frame from github data
probsolve <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT LINEAR GROWTH MODEL (COMBINED SPECIFICATION) ----
#------------------------------------------------------------------------------#

# mixed model specification
model1 <- rblimp(
    data = probsolve,
    clusterid = 'school student',
    nominal = 'frlunch condition',
    # fixed = 'month7 condition',
    center = 'grandmean = stanmath frlunch',
    model = 'probsolve ~ month7 stanmath frlunch condition month7*condition | month7', 
    simple = ' month7 | condition', 
    seed = 90291,
    burn = 30000,
    iter = 30000,
    nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1)

# plot conditional growth curves
simple_plot(probsolve ~ month7 | condition.1, model1)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model1)

# plot distributions and residuals
univariate_plot(vars = c('probsolve.student','probsolve_on_month7.student','probsolve.school','probsolve_on_month7.school','probsolve.residual'), model1)

# plot standardized residuals vs. predicted values
bivariate_plot(probsolve.residual ~ probsolve.predicted, standardize = 'y', model = model1)

# plot level-1 standardized residuals by time
bivariate_plot(probsolve.residual ~ month7, model = model1)

# plot predicted values by time
bivariate_plot(probsolve.predicted ~ month7, model = model1)

#------------------------------------------------------------------------------#
# FIT LINEAR GROWTH MODEL (LATENT SPECIFICATION) ----
#------------------------------------------------------------------------------#

# latent variable specification
model2 <- rblimp(
  data = probsolve,
  clusterid = 'school student',
  ordinal = 'frlunch condition',
  latent = '
    student = iceptl2 linearl2; 
    school = iceptl3 linearl3',
  model = ' 
    level3:
    iceptl3 ~ intercept condition;
    linearl3 ~ intercept condition;
    iceptl3 ~~ linearl3;
    level2:
    iceptl2 ~ intercept@iceptl3 stanmath frlunch;
    linearl2 ~ intercept@linearl3;
    iceptl2 ~~ linearl2;
    level1:
    probsolve ~ intercept@iceptl2 month7@linearl2',
  seed = 90291,
  burn = 50000,
  iter = 50000,
  nimps = 20)

output(model2)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model2)

# plot distributions and residuals
residuals <- paste0(c('iceptl2','linearl2','iceptl3','linearl3','probsolve'), '.residual')
univariate_plot(vars = residuals, model2)

# plot standardized residuals vs. predicted values
bivariate_plot(probsolve.residual ~ probsolve.predicted, standardize = 'y', model = model2)

# plot standardized residuals by predictors
bivariate_plot(probsolve.residual ~ month7, model = model2)
bivariate_plot(y_vars = c('iceptl3.residual','linearl3.residual'), x_vars = 'condition', model = model2)

# plot predicted values by time
bivariate_plot(probsolve.predicted ~ month7, model = model2)
