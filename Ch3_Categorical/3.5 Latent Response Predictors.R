# LOGISTIC REGRESSION WITH A LATENT RESPONSE VARIABLE PREDICTOR ----

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/employee.csv'

# create data frame from github data
employee <- read.csv(data_url)

# FIT MODEL ----

# logistic regression with a latent response variable predictor
# thresholds require large number of iterations to achieve adequate N_eff 
model <- rblimp(
    data = employee,
    ordinal = 'jobsat male',
    nominal = 'turnover',
    model = '
    focal:
    turnover ~ lmx empower jobsat.latent male;
    predictors:
    jobsat ~ lmx empower male', 
    seed = 90291,
    burn = 30000,
    iter = 30000,
    nimps = 20)

# print output
output(model)

# plot parameter distributions
posterior_plot(model,'turnover')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model)

# plot distributions and residuals
# univariate_plot(vars = c('intensity','drinker.1.residual'), model2)

# plot individual-level predicted probabilities
bivariate_plot(y_vars = 'turnover.1.probability', 
               x_vars = c('lmx','empower','jobsat.latent','male'),
               model = model)

# plot standardized residuals vs. predicted values
# bivariate_plot(turnover.1.residual ~ turnover.1.predicted, standardize = 'y', model = model)

# plot standardized residuals vs. numeric predictors
bivariate_plot(y_vars = 'turnover.1.residual', 
               x_vars = c('lmx','empower','jobsat.latent'),
               standardize = 'y',
               model = model)
