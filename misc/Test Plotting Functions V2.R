# MULTILEVEL LINEAR GROWTH MODEL

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/trial_stacked.csv'

# create data frame from github data
trial <- read.csv(data_url)

# FIT REPEATED MEASURES MODEL ----

# mixed model specification
# model1 <- rblimp(
#   data = trial,
#   ordinal = 'dropout',
#   nominal = 'week',
#   clusterid = 'person',
#   model = '
#     severity ~ intercept week;
#     dropout ~ intercept@-3 week | intercept@0;',
#   seed = 90291,
#   burn = 10000,
#   iter = 10000,
#   nimps = 10)

model1 <- rblimp(
  data = trial,
  ordinal = 'dropout',
  clusterid = 'person',
  model = '
    severity ~ intercept week drug drug*week | week;
    dropout ~ intercept@-3 week | intercept@0;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 10)

bivariate_plot(model1, severity.predicted ~ week)
bivariate_plot(model1, severity.predicted ~ week, x_type = "numeric")
bivariate_plot(model1, severity.predicted ~ week, x_type = "discrete")


# plot
bivariate_plot(model1, severity.predicted ~ week)
bivariate_plot(model1, severity.predicted ~ week, lines = T)
bivariate_plot(model1, dropout.1.probability ~ week)
distribution_plot(model1)
imputed_vs_observed_plot(model1)
residuals_plot(model1)
 
