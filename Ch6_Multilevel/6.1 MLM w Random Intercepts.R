# RANDOM INTERCEPT REGRESSION

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/problemsolving2level.csv'

# create data frame from github data
probsolve <- read.csv(data_url)

# FIT MODEL WITH MIXED MODEL SPECIFICATION ----

# mixed model specification
model1 <- rblimp(
    data = probsolve,
    clusterid = 'school',
    nominal = 'condition frlunch',
    # fixed = 'probsolve1 condition',
    model = 'probsolvpost ~ probsolvpre stanmath frlunch probsolvpre.mean stanmath.mean frlunch.mean condition', 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(model1)

# FIT MODEL WITH LATENT VARIABLE SPECIFICATION ----

# random intercept as level-2 latent variable
model2 <- rblimp(
  data = probsolve,
  clusterid = 'school',
  nominal = 'condition frlunch',
  latent = 'school = ranicept',
  # fixed = 'probsolve1 condition',
  model = '
    ranicept ~ probsolvpre.mean stanmath.mean frlunch.mean condition;
    probsolvpost ~ intercept@ranicept probsolvpre stanmath frlunch;',
  parameters = '
    rsq_total = ranicept.coefvar / (ranicept.totalvar + probsolvpost.totalvar);
    rsq_between = ranicept.coefvar / ranicept.totalvar;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(model2)

# NEED TO REVIST TOTAL VARIANCE AND ADD EFFECT SIZES ----
