# CURVILINEAR REGRESSION

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

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

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model)
imputed_vs_observed_plot(model)
residuals_plot(model)

# plot predicted values against curvilinear predictor
bivariate_plot(mathpost.predicted ~ anxiety, model)

