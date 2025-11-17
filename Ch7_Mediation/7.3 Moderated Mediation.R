# MODERATED MEDIATION MODEL

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/discrimination.csv'

# create data frame from github data
discrimination <- read.csv(data_url)

# FIT MODEL WITH BINARY MODERATOR OF A PATH ----

# sex as a moderator of the a path
model1 <- rblimp(
  data = discrimination,
  nominal = 'female',
  model = '
    apath:
    familism ~ discrim@a_male female age discrim*female@a_dif;
    bpath:
    internalize ~ discrim familism@b female age;',
  parameters = '
    ind_male = a_male*b;
    ind_female = (a_male + a_dif)*b;
    ind_diff = ind_female - ind_male;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1)

# plot distribution of indirect effects
posterior_plot(model1, 'ind_male')
posterior_plot(model1, 'ind_female')
posterior_plot(model1, 'ind_diff')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model1)
imputed_vs_observed_plot(model1)
residuals_plot(model1)

# FIT MODEL WITH BINARY MODERATOR OF B PATH ----

# sex as a moderator of the b path
model2 <- rblimp(
  data = discrimination,
  nominal = 'female',
  model = '
    apath:
    familism ~ discrim@a female age;
    bpath:
    internalize ~ discrim familism@b_male female age familism*female@b_dif;',
  parameters = '
    ind_male = a*b_male;
    ind_female = a*(b_male + b_dif);
    ind_diff = ind_female - ind_male;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2)

# plot distribution of indirect effects
posterior_plot(model2, 'ind_male')
posterior_plot(model2, 'ind_female')
posterior_plot(model2, 'ind_diff')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)

# FIT MODEL WITH BINARY MODERATOR OF A AND B PATHS ----

# sex as a moderator of the a and b path
model3 <- rblimp(
  data = discrimination,
  nominal = 'female',
  model = '
    apath:
    familism ~ discrim@a_male female age discrim*female@a_dif;
    bpath:
    internalize ~ discrim familism@b_male female age familism*female@b_dif;',
  parameters = '
    ind_male = a_male*b_male;
    ind_female = (a_male + a_dif)*(b_male + b_dif);
    ind_diff = ind_female - ind_male;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model3)

# plot parameter distributions
posterior_plot(model3)

# plot distribution of indirect effects
posterior_plot(model3, 'ind_male')
posterior_plot(model3, 'ind_female')
posterior_plot(model3, 'ind_diff')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model3)
imputed_vs_observed_plot(model3)
residuals_plot(model3)

