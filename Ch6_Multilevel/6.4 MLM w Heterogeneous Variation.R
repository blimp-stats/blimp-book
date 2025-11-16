# MULTILEVEL MODEL WITH HETEROGENEOUS WITHIN-CLUSTER VARIATION

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/diary.csv'

# create data frame from github data
diary <- read.csv(data_url)

# FIT RANDOM SLOPE MODEL WITH HETERGENEOUS VARIATION ----

# mixed model specification with heterogeneous variation
model1 <- rblimp(
  data = diary,
  clusterid = 'person',
  center = 'groupmean = pain; grandmean = pain.mean stress female',
  model = 'posaff ~ pain pain.mean stress female pain*stress | pain',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  options = 'hev'
)
output(model1)

# FIT LOCATION SCALE MODEL ----

# latent variable specificaton
model2 <- rblimp(
  data = diary,
  clusterid = 'person',
  nominal = 'female',
  latent = 'person = ranicept ranslope logvar;',
  center = 'groupmean = pain; grandmean = pain.mean stress female',
  model = '
    level2:
    ranicept ~ intercept pain.mean stress female;
    ranslope ~ intercept stress;
    logvar ~ intercept stress;
    ranicept ranslope logvar ~~ ranicept ranslope logvar;
    level1:
    posaff ~ intercept@ranicept pain@ranslope;
    scale:
    var(posaff) ~ intercept@logvar pain;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model2)