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

# FIT REPEATED MEASURES MODEL WITH PREDICTOR ----

# mixed model specification
model2 <- rblimp(
  data = trial,
  nominal = 'week',
  clusterid = 'person',
  center = 'grandmean = male',
  model = 'severity ~ intercept week drug week*drug male',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model2)

# FIT LINEAR GROWTH MODEL ----

# mixed model specification
model3 <- rblimp(
  data = trial,
  clusterid = 'person',
  model = 'severity ~ intercept week | week;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model3)

# latent variable specification
model4 <- rblimp(
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
output(model4)

# FIT QUADRATIC GROWTH MODEL ----

# mixed model formulation
model5 <- rblimp(
  data = trial,
  clusterid = 'person',
  model = 'severity ~ intercept week week^2 | week week^2;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model5)

# latent variable specification
model6 <- rblimp(
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
output(model6)

# FIT QUADRATIC GROWTH MODEL WITH PREDICTOR ----

# mixed model formulation
model7 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'person',
  center = 'grandmean = male',
  model = 'severity ~ intercept week week^2 drug drug*week drug*week^2 male | week week^2',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model7)

# latent variable specification
model8 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'person',
  latent = 'person = icept linear quad;',
  center = 'grandmean = male',
  model = '
    level2:
    icept ~ intercept drug male;
    linear ~ intercept drug;
    quad ~ intercept drug;
    icept linear quad ~~ icept linear quad;
    level1:
    severity ~ intercept@icept week@linear week^2@quad;',
  seed = 90291,
  burn = 30000,
  iter = 30000
)
output(model8)