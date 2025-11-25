# LATENT VARIABLE WITH SKEWED INDICATORS

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

# FIT MODEL ----

# normalized indicators
model <- rblimp(
  data = inflamm,
  ordinal = 'female els',
  latent = 'inflam',
  # fixed = 'female age',
  center = 'age',
  model = '
    structural:
    yjt(dpdd - 6) ~ inflam female els age;
    measurement:
    inflam@1;
    inflam -> yjt(inflam_crp)@lo1 yjt(inflam_il6) inflam_tnf yjt(inflam_ifn);;
    predictors:
    inflam female els age ~~ inflam female els age;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

bivariate_plot(yjt(inflam_crp) ~ inflam.latent, model)
bivariate_plot(inflam_il6 ~ inflam.latent, model)
bivariate_plot(inflam_tnf ~ inflam.latent, model)
bivariate_plot(inflam_ifn ~ inflam.latent, model)

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model)
imputed_vs_observed_plot(model)
residuals_plot(model)

