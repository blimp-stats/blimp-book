# MULTILEVEL MODEL WITH RANDOM SLOPES AND INTERACTIONS

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/trial_stacked.csv'

# create data frame from github data
trial <- read.csv(data_url)

# FIT REPEATED MEASURES MODEL ----

# mixed model specification
model1 <- rblimp(
  data = trial,
  nominal = 'week',
  clusterid = 'person',
  model = 'severity ~ intercept week',
  seed = 90291,
  burn = 10000,
  iter = 10000
)

output(model1)

# FIT LINEAR GROWTH MODEL WITH MIXED MODEL SPECIFICATION ----

# linear growth model
model2 <- rblimp(
  data = trial,
  clusterid = 'person',
  model = 'severity ~ intercept week | week;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)

output(model2)

# linear growth model with predictors
model3 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'person',
  center = 'grandmean = male',
  model = 'severity ~ intercept@b0 week@b1 drug@b01 drug*week@b11 male | week;',
  simple = 'week | drug',
  parameters = '
    mu6_drug0 = b0 + 6*b1;
    mu6_drug1 = (b0 + b01) + 6*(b1 + b11);
    mu6_diff = mu6_drug1 - mu6_drug0;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)

output(model3)
simple_plot(severity ~ week | drug, model3)

# FIT QUADRATIC GROWTH MODEL WITH MIXED MODEL SPECIFICATION ----

# quadratic fixed effect but no quadratic variance
model4 <- rblimp(
  data = trial,
  clusterid = 'person',
  model = 'severity ~ intercept week week^2 | week;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)

output(model4)

# quadratic fixed effect and quadratic variance
model5 <- rblimp(
  data = trial,
  clusterid = 'person',
  model = 'severity ~ intercept week week^2 | week week^2;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)

output(model5)

# quadratic fixed effect with predictor variables
model6 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'person',
  center = 'grandmean = male',
  model = 'severity ~ intercept@b0 week@b1 week^2@b2 drug@b01 drug*week@b11 drug*week^2@b21 male | week',
  parameters = '
    mu6_placebo = b0 + 6*b1 + 36*b2;
    mu6_drug = (b0 + b01) + 6*(b1 + b11) + 36*(b2 + b21);
    mu6_diff = mu6_drug - mu6_placebo;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)

output(model6)

# FIT LINEAR GROWTH MODEL WITH LATENT VARIABLE SPECIFICATION ----

# linear growth model
model7 <- rblimp(
  data = trial,
  clusterid = 'person',
  latent = 'person = icept linear;',
  model = '
    level2:
    intercept -> icept linear;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)

output(model7)

# linear growth model with predictors
model8 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'person',
  center = 'grandmean = male',
  latent = 'person = icept linear;',
  model = '
    level2:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear;',
  parameters = '
    mu6_drug0 = b0 + 6*b1;
    mu6_drug1 = (b0 + b01) + 6*(b1 + b11);
    mu6_diff = mu6_drug1 - mu6_drug0;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)

output(model8)

# FIT QUADRATIC GROWTH MODEL WITH LATENT VARIABLE SPECIFICATION ----

# quadratic fixed effect but no quadratic variance
model9 <- rblimp(
  data = trial,
  clusterid = 'person',
  latent = 'person = icept linear quad;',
  model = '
    level2:
    intercept -> icept linear quad;
    quad ~~ quad@.001;
    icept linear ~~ icept linear;
    level1:
    severity ~ intercept@icept week@linear week^2@quad;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model9)

# quadratic fixed effect and quadratic variance
model10 <- rblimp(
  data = trial,
  clusterid = 'person',
  latent = 'person = icept linear quad;',
  model = '
    level2:
    intercept -> icept linear quad;
    icept linear quad ~~ icept linear quad;
    level1:
    severity ~ intercept@icept week@linear week^2@quad;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model10)

# quadratic fixed effect with predictors
model11 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'person',
  latent = 'person = icept linear quad;',
  center = 'grandmean = male',
  model = '
    level2:
    icept ~ intercept@b0 drug@b01 male;
    linear ~ intercept@b1 drug@b11;
    quad ~ intercept@b2 drug@b21;
    quad ~~ quad@.001;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept week@linear week^2@quad;',
  parameters = '
    mu6_drug0 = b0 + 6*b1 + 36*b2;
    mu6_drug1 = (b0 + b01) + 6*(b1 + b11) + 36*(b2 + b21);
    mu6_diff = mu6_drug1 - mu6_drug0;',
  seed = 90291,
  burn = 30000,
  iter = 30000
)
output(model11)