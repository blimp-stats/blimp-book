# BRIAN NOTES ----
# The .residual behavior with count models is incorrect

# NEGATIVE BINOMIAL REGRESSION FOR A COUNT OUTCOME ----

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/alcoholuse.csv'

# create data frame from github data
alcoholuse <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT MODEL ----
#------------------------------------------------------------------------------#

# negative binomial regression
model1 <- rblimp(
    data = alcoholuse,
    nominal = 'college male',
    count = 'alcdays',
    # fixed = 'male',
    model = 'alcdays ~ alcage college age male', 
    seed = 90291,
    burn = 10000,
    iter = 10000,
    nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1,'alcdays')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model1)

# plot standardized residuals vs. predicted values
bivariate_plot(alcdays.residual ~ alcdays.predicted, standardize = 'y', model = model1)
# bivariate_plot(alcdays.residual ~ alcdays.predicted, model = model1)

# plot standardized residuals vs. numeric predictors
bivariate_plot(y_vars = 'alcdays.residual', 
               x_vars = c('alcage','age'),
               standardize = 'y',
               model = model1)

bivariate_plot(y_vars = 'alcdays.residual', 
               x_vars = c('alcage','age'),
               model = model1)
