# RANDOM INTERCEPT CROSS-LAGGED PANEL MODEL

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url fores raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/loneliness.csv'

# create data frame from github data
lonely <- read.csv(data_url)

# FIT RICLPM MODEL ----

model1 <- rblimp(
  data = lonely,
  latent = 'icept_ad icept_lo',
  model = ' 
    anxdep1res = anxdep1 - (ad_mu1 + icept_ad);  # definition variables
    anxdep2res = anxdep2 - (ad_mu2 + icept_ad);
    anxdep3res = anxdep3 - (ad_mu3 + icept_ad);
    lonely1res = lonely1 - (lo_mu1 + icept_lo);
    lonely2res = lonely2 - (lo_mu2 + icept_lo);
    lonely3res = lonely3 - (lo_mu3 + icept_lo);
    random_intercepts:
    icept_ad ~~ icept_lo;
    anxdep:
    anxdep1 ~ intercept@ad_mu1 icept_ad@1;
    anxdep2 ~ intercept@ad_mu2 icept_ad@1 anxdep1res lonely1res;
    anxdep3 ~ intercept@ad_mu3 icept_ad@1 anxdep2res lonely2res;
    anxdep4 ~ intercept@ad_mu4 icept_ad@1 anxdep3res lonely3res;
    lonely:
    lonely1 ~ intercept@lo_mu1 icept_lo@1;
    lonely2 ~ intercept@lo_mu2 icept_lo@1 lonely1res anxdep1res;
    lonely3 ~ intercept@lo_mu3 icept_lo@1 lonely2res anxdep2res;
    lonely4 ~ intercept@lo_mu4 icept_lo@1 lonely3res anxdep3res;
    covariances:
    anxdep1 ~~ lonely1;
    anxdep2 ~~ lonely2;
    anxdep3 ~~ lonely3;
    anxdep4 ~~ lonely4;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(model1)

# FIT RICLPM MODEL WITH BETWEEN-PERSON PREDICTOR ----

model2 <- rblimp(
  data = lonely,
  latent = 'icept_ad icept_lo',
  model = ' 
    anxdep1res = anxdep1 - (ad_mu1 + icept_ad);  # definition variables
    anxdep2res = anxdep2 - (ad_mu2 + icept_ad);
    anxdep3res = anxdep3 - (ad_mu3 + icept_ad);
    lonely1res = lonely1 - (lo_mu1 + icept_lo);
    lonely2res = lonely2 - (lo_mu2 + icept_lo);
    lonely3res = lonely3 - (lo_mu3 + icept_lo);
    random_intercepts:
    female -> icept_ad icept_lo;
    icept_ad ~~ icept_lo;
    anxdep:
    anxdep1 ~ intercept@ad_mu1 icept_ad@1;
    anxdep2 ~ intercept@ad_mu2 icept_ad@1 anxdep1res lonely1res;
    anxdep3 ~ intercept@ad_mu3 icept_ad@1 anxdep2res lonely2res;
    anxdep4 ~ intercept@ad_mu4 icept_ad@1 anxdep3res lonely3res;
    lonely:
    lonely1 ~ intercept@lo_mu1 icept_lo@1;
    lonely2 ~ intercept@lo_mu2 icept_lo@1 lonely1res anxdep1res;
    lonely3 ~ intercept@lo_mu3 icept_lo@1 lonely2res anxdep2res;
    lonely4 ~ intercept@lo_mu4 icept_lo@1 lonely3res anxdep3res;
    covariances:
    anxdep1 ~~ lonely1;
    anxdep2 ~~ lonely2;
    anxdep3 ~~ lonely3;
    anxdep4 ~~ lonely4;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(model2)

# FIT RICLPM MODEL WITH GROUP-SPECIFIC EFFECTS ----

model3 <- rblimp(
  data = lonely,
  latent = 'icept_ad icept_lo',
  model = ' 
    anxdep1res = anxdep1 - (ad_mu1 + icept_ad);  # definition variables
    anxdep2res = anxdep2 - (ad_mu2 + icept_ad);
    anxdep3res = anxdep3 - (ad_mu3 + icept_ad);
    lonely1res = lonely1 - (lo_mu1 + icept_lo);
    lonely2res = lonely2 - (lo_mu2 + icept_lo);
    lonely3res = lonely3 - (lo_mu3 + icept_lo);
    random_intercepts:
    female -> icept_ad icept_lo;
    icept_ad ~~ icept_lo;
    anxdep:
    anxdep1 ~ intercept@ad_mu1 icept_ad@1;
    anxdep2 ~ intercept@ad_mu2 icept_ad@1 anxdep1res lonely1res lonely1res*female;
    anxdep3 ~ intercept@ad_mu3 icept_ad@1 anxdep2res lonely2res lonely2res*female;
    anxdep4 ~ intercept@ad_mu4 icept_ad@1 anxdep3res lonely3res lonely3res*female;
    lonely:
    lonely1 ~ intercept@lo_mu1 icept_lo@1;
    lonely2 ~ intercept@lo_mu2 icept_lo@1 lonely1res anxdep1res anxdep1res*female;
    lonely3 ~ intercept@lo_mu3 icept_lo@1 lonely2res anxdep2res anxdep2res*female;
    lonely4 ~ intercept@lo_mu4 icept_lo@1 lonely3res anxdep3res anxdep3res*female;
    covariances:
    anxdep1 ~~ lonely1;
    anxdep2 ~~ lonely2;
    anxdep3 ~~ lonely3;
    anxdep4 ~~ lonely4;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(model3)
