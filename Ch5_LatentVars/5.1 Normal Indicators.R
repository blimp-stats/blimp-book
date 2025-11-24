# BRIAN NOTES ----
# any way to easily identify columns of iterations matrix? transpose matrix and give same row names as @estimates?
# want to construct wald test of residual correlations among indicators, or all residual correlations

# LATENT VARIABLE WITH NORMAL INDICATORS

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflamm <- read.csv(data_url)

# FIT STRUCTURAL REGRESSION MODEL ----

# first loading fixed to 1
model1 <- rblimp(
  data = inflamm,
  latent = 'inflam',
  model = '
    structural:
    yjt(dpdd - 6) ~ inflam;
    measurement:
    inflam -> inflam_crp inflam_il6 inflam_tnf inflam_ifn;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model1)

# factor variance fixed to 1
model2 <- rblimp(
  data = inflamm,
  latent = 'inflam',
  model = '
    structural:
    yjt(dpdd - 6) ~ inflam female els age;
    measurement:
    inflam@1;
    inflam -> inflam_crp@lo1 inflam_il6 inflam_tnf inflam_ifn;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# FIT MODEL WITH COVARIATES ----

# correlate latent and manifest predictors
model3 <- rblimp(
  data = inflamm,
  ordinal = 'female els',
  latent = 'inflam',
  center = 'age',
  model = '
    structural:
    yjt(dpdd - 6) ~ inflam female els age;
    measurement:
    inflam@1;
    inflam -> inflam_crp@lo1 inflam_il6 inflam_tnf inflam_ifn;
    predictors:
    inflam female els age ~~ inflam female els age;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model3)

# link latent and manifest predictors with regression
model4 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  latent = 'inflam',
  center = 'age',
  model = '
    structural:
    yjt(dpdd - 6) ~ inflam female els age;
    measurement:
    inflam@1;
    inflam -> inflam_crp@lo1 inflam_il6 inflam_tnf inflam_ifn;
    predictors:
    inflam ~ female els age;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model4)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

bivariate_plot(inflam_crp ~ inflam.latent, model3)
bivariate_plot(inflam_il6 ~ inflam.latent, model3)
bivariate_plot(inflam_tnf ~ inflam.latent, model3)
bivariate_plot(inflam_ifn ~ inflam.latent, model3)
bivariate_plot(yjt(dpdd-6) ~ inflam_crp.residual, model3)

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model3)
imputed_vs_observed_plot(model3)
residuals_plot(model3)
