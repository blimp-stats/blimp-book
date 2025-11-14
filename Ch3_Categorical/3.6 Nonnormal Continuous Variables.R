# BRIAN NOTES ----
# Example of the naming conventions for yjt variables for clean-up

# YEO-JOHNSON NORMALIZING TRANSFORMATION FOR NONNORMAL CONTINUOUS VARIABLES

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflamm <- read.csv(data_url)

# plotting function
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# FIT MODEL ASSUMING NORMALITY ----

# multiple imputations for graphical diagnostics 
model1 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  center = 'age',
  # fixed = 'age female',
  model = 'dpdd ~ female els age inflam_sum', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1,'dpdd')

# GRAPHICAL DIAGNOSTICS ----

# plot distributions, observed vs. imputed scores, and residuals
imputation_plot(model1)
imputed_vs_observed_plot(model1)
residuals_plot(model1)

# FIT MODEL WITH NORMALIZED OUTCOME ----

# normalized outcome with yeo-johnson transformation
# multiple imputations for graphical diagnostics 
model2 <- rblimp(
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
output(model2)

# plot parameter distributions
posterior_plot(model2,'dpdd')

# GRAPHICAL DIAGNOSTICS ----

# plot distributions, observed vs. imputed scores, and residuals
imputation_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)

# FIT MODEL WITH NORMALIZED PREDICTOR ----

# predictor is normalized in its missing data model but skewed in the focal model
# multiple imputations for graphical diagnostics 
model3 <- rblimp(
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
output(model3)

# plot parameter distributions
posterior_plot(model3,'dpdd')

# GRAPHICAL DIAGNOSTICS ----

# plot distributions, observed vs. imputed scores, and residuals
imputation_plot(model3)
imputed_vs_observed_plot(model3)
residuals_plot(model3)

# FIT MODEL WITH FULLY NORMALIZED PREDICTOR ----

# predictor is normalized in its missing data model and normalized in the focal model
# multiple imputations for graphical diagnostics 
model4 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  center = 'age',
  # fixed = 'age female',
  model = '
  focal:
  yjt(dpdd - 6) ~ female els age yjt(inflam_sum, lam_prior);
  predictor:
  yjt(inflam_sum, lam_prior) ~ female els age;', 
  parameters = 'lam_prior ~ uniform(0, 2);',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model4)

# plot parameter distributions
posterior_plot(model4,'dpdd')

# GRAPHICAL DIAGNOSTICS ----

# plot distributions, observed vs. imputed scores, and residuals
imputation_plot(model4)
imputed_vs_observed_plot(model4)
residuals_plot(model4)

# PREDICTED VALUES ARE NA
names(model4@imputations[[1]])
head(model4@imputations[[1]])