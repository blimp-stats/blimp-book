# LINEAR REGRESSION WITH CATEGORICAL PREDICTORS ----

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/smoking.csv'

# plotting function
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# create data frame from github data
smoking <- read.csv(data_url)

# FIT MODEL ----

# linear regression with binary and multicategorical predictor 
model1 <- rblimp(
    data = smoking,
    ordinal = 'parsmoke',
    nominal = 'educ',
    center = 'age',
    model = 'intensity ~ parsmoke educ age', 
    seed = 90291,
    burn = 10000,
    iter = 10000)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1,'intensity')

# GRAPHICAL DIAGNOSTICS ----

# multiple imputations for graphical diagnostics 
model2 <- rblimp(
  data = smoking,
  ordinal = 'parsmoke',
  nominal = 'educ',
  center = 'age',
  model = 'intensity ~ parsmoke educ age', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# plot distributions, observed vs. imputed scores, and residuals
imputation_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)
