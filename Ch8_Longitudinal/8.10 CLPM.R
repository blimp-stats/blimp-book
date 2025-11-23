# CROSS-LAGGED PANEL MODEL

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url fores raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/loneliness.csv'

# create data frame from github data
lonely <- read.csv(data_url)

# FIT CROSS-LAGGED PANEL MODEL ----

# unrestricted model
model1 <- rblimp(
  data = lonely,
  model = ' 
    anxdep:
    anxdep1 ~ intercept;
    anxdep2 ~ anxdep1@anx_anx1 lonely1@anx_lon1;
    anxdep3 ~ anxdep2@anx_anx2 lonely2@anx_lon2;
    anxdep4 ~ anxdep3@anx_anx3 lonely3@anx_lon3;
    lonely:
    lonely1 ~ intercept;
    lonely2 ~ lonely1@lon_lon1 anxdep1@lon_anx1;
    lonely3 ~ lonely2@lon_lon2 anxdep2@lon_anx2;
    lonely4 ~ lonely3@lon_lon3 anxdep3@lon_anx3;
    covariances:
    anxdep1 ~~ lonely1;
    anxdep2 ~~ lonely2;
    anxdep3 ~~ lonely3;
    anxdep4 ~~ lonely4;',
  waldtest = c('anx_anx1 = anx_anx2; anx_anx2 = anx_anx3; lon_lon1 = lon_lon2; lon_lon2 = lon_lon3', 
               'anx_lon1 = anx_lon2; anx_lon2 = anx_lon3; lon_anx1 = lon_anx2; lon_anx2 = lon_anx3'),
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(model2)

# constrained autoregressive and cross-lagged effects
model2 <- rblimp(
  data = lonely,
  model = ' 
    anxdep:
    anxdep1 ~ intercept;
    anxdep2 ~ anxdep1@anx_anx lonely1@anx_lon;
    anxdep3 ~ anxdep2@anx_anx lonely2@anx_lon;
    anxdep4 ~ anxdep3@anx_anx lonely3@anx_lon;
    lonely:
    lonely1 ~ intercept;
    lonely2 ~ lonely1@lon_lon anxdep1@lon_anx;
    lonely3 ~ lonely2@lon_lon anxdep2@lon_anx;
    lonely4 ~ lonely3@lon_lon anxdep3@lon_anx;
    covariances:
    anxdep1 ~~ lonely1;
    anxdep2 ~~ lonely2;
    anxdep3 ~~ lonely3;
    anxdep4 ~~ lonely4;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(model2)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model2)
imputed_vs_observed_plot(model2)
residuals_plot(model2)