# MULTILEVEL MODEL WITH DYNAMIC EFFECT

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/diary.csv'

# create data frame from github data
diary <- read.csv(data_url)

# FIT MODEL ----

set_blimp('/applications/blimp/blimp-nightly')

# dsem with lagged predictors
model <- rblimp(
  data = diary,
  clusterid = 'person; timeid: day;',
  latent = 'person = pain_icept posaff_icept',
  model = '
        pain ~ intercept@pain_icept (pain.lag - pain_icept) (posaff.lag - posaff_icept);
        posaff ~ intercept@posaff_icept (posaff.lag - posaff_icept) (pain.lag - pain_icept);
        pain ~~ posaff;
        intercept -> pain_icept posaff_icept;
        pain_icept ~~ posaff_icept;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model)