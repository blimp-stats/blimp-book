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
  clusterid = 'id',
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
  clusterid = 'id',
  center = 'grandmean = male',
  model = 'severity ~ intercept week drug week*drug male',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model2)

# FIT LINEAR GROWTH MODEL ----

# mixed model formulation
model3 <- rblimp(
  data = trial,
  clusterid = 'id',
  model = 'severity ~ intercept week | week;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model3)

# FIT QUADRATIC GROWTH MODEL ----

# mixed model formulation
model4 <- rblimp(
  data = trial,
  clusterid = 'id',
  model = 'severity ~ intercept week week^2 | week week^2;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model4)

# FIT QUADRATIC GROWTH MODEL WITH PREDICTOR ----

# mixed model formulation
model5 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'id',
  center = 'grandmean = male',
  model = 'severity ~ intercept week week^2 drug drug*week drug*week^2 | week week^2',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model5)



model2 <- rblimp(
  data = diary,
  clusterid = 'person',
  nominal = 'female',
  latent = 'person = ranicept ranslope;',
  center = 'groupmean = pain; grandmean = pain.mean stress female',
  model = '
    level2:
    ranicept ~ intercept stress pain.mean female;
    ranslope ~ intercept;
    ranicept ~~ ranslope;
    level1:
    posaff ~ intercept@ranicept pain@ranslope;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model2)

# FIT RANDOM SLOPE MODEL WITH INTERACTION EFFECTS ----

# mixed model specification
model3 <- rblimp(
  data = diary,
  clusterid = 'person',
  center = 'groupmean = pain; grandmean = pain.mean stress female',
  model = 'posaff ~ pain pain.mean stress female pain*stress pain.mean*stress | pain',
  simple = 'pain | stress; pain.mean | stress;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model3)

# plot conditional effects
simple_plot(posaff ~ pain | stress, model3)
simple_plot(posaff ~ pain.mean[person] | stress, model3)

# latent variable specification
model4 <- rblimp(
  data = diary,
  clusterid = 'person',
  nominal = 'female',
  latent = 'person = ranicept ranslope;',
  center = 'groupmean = pain; grandmean = pain.mean stress female',
  model = '
    level2:
    ranicept ~ intercept stress pain.mean female stress*pain.mean;
    ranslope ~ intercept stress;
    ranicept ~~ ranslope;
    level1:
    posaff ~ intercept@ranicept pain@ranslope;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model4)

# latent variable specification with conditional effects
model5 <- rblimp(
  data = diary,
  clusterid = 'person',
  nominal = 'female',
  latent = 'person = ranicept ranslope;',
  center = 'groupmean = pain; grandmean = pain.mean stress female',
  model = '
    level2:
    ranicept ~ intercept stress pain.mean female stress*pain.mean;
    ranslope ~ intercept@b1 stress@b2;
    ranicept ~~ ranslope;
    level1:
    posaff ~ intercept@ranicept pain@ranslope;
    predictor:
    stress ~ pain.mean female;',
  parameter = '
    painslp_low = b1 - b2*sqrt(stress.totalvar);
    painslp_mean = b1;
    painslp_hi = b1 + b2*sqrt(stress.totalvar);
  ',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model5)