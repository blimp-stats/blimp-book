# LINEAR REGRESSION WITH CATEGORICAL PREDICTORS ----

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/smoking.csv'

# create data frame from github data
smoking <- read.csv(data_url)

# FIT MODEL ----

# linear regression with automatic dummy coding
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

# linear regression with automatic dummy coding
model2 <- rblimp(
  data = smoking,
  ordinal = 'parsmoke',
  nominal = 'educ',
  center = 'parsmoke age',
  model = 'intensity ~ intercept@b0 parsmoke educ.2@b2 educ.3@b3 age',
  waldtest = 'b2:b3 = 0',
  parameters = '
    mu_educ.1 = b0;
    mu_educ.2 = b0 + b2;
    mu_educ.3 = b0 + b3;
    mu2_vs_mu3 = mu_educ.2 - mu_educ.3;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model2)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)
