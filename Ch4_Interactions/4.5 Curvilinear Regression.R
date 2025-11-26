# CURVILINEAR REGRESSION

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/math.csv'

# create data frame from github data
math <- read.csv(data_url)

# FIT MODEL ----

# curvilinear regression
model <- rblimp(
    data = math,
    ordinal = 'frlunch male',
    fixed = 'male mathpre',
    center = 'anxiety',
    model = 'mathpost ~ anxiety anxiety^2 frlunch mathpre male',
    seed = 12345,
    burn = 10000,
    iter = 10000,
    nimps = 20)

# print output
output(model)

# plot parameter distributions
posterior_plot(model,'mathpost')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model)

# plot standardized residuals vs. predicted values
bivariate_plot(mathpost.residual ~ mathpost.predicted, standardize = 'y', model = model)

# plot standardized residuals vs. numeric predictors
bivariate_plot(y_vars = 'mathpost.residual', 
               x_vars = c('anxiety','mathpre'),
               standardize = 'y',
               model = model)

