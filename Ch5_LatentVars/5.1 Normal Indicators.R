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

# normal indicators with first loading fixed to 1
model1 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  latent = 'inflam',
  model = '
    inflam -> inflam_crp inflam_il6 inflam_tnf inflam_ifn', 
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model1)

# normal indicators with factor variance fixed to 1
model2 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  latent = 'inflam',
  model = '
    inflam@1;
    inflam -> inflam_crp@lo1 inflam_il6 inflam_tnf inflam_ifn', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)

# normal indicators with factor variance fixed to 1
model3 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  latent = 'inflam',
  model = '
    inflam@1;
    inflam -> yjt(inflam_crp)@lo1 yjt(inflam_il6) inflam_tnf yjt(inflam_ifn);', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model3)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model3)
imputed_vs_observed_plot(model3)
residuals_plot(model3)

