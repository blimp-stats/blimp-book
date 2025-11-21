# THREE-LEVEL MULTILEVEL GROWTH MODEL

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/problemsolving3level.csv'

# create data frame from github data
probsolve <- read.csv(data_url)

# FIT MODEL ----

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

# latent variable specification
model2 <- rblimp(
  data = probsolve,
  clusterid = 'school student',
  ordinal = 'frlunch condition',
  latent = '
    student = raniceptl2 ranslopel2; 
    school = raniceptl3 ranslopel3',
  model = ' 
    level3:
    raniceptl3 ~ intercept condition;
    ranslopel3 ~ intercept condition;
    raniceptl3 ~~ ranslopel3;
    level2:
    raniceptl2 ~ intercept@raniceptl3 stanmath frlunch;
    ranslopel2 ~ intercept@ranslopel3;
    raniceptl2 ~~ ranslopel2;
    level1:
    probsolve ~ intercept@raniceptl2 month7@ranslopel2',
  seed = 90291,
  burn = 50000,
  iter = 50000,
  nimps = 20)

output(model2)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot predicted values by time
bivariate_plot(model2, probsolve.predicted ~ month7, lines = T)

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)
