# MULTIPLE MEDIATOR MODEL

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/discrimination.csv'

# create data frame from github data
discrimination <- read.csv(data_url)

# FIT MODEL ----

# single mediator model
model <- rblimp(
  data = pain,
  nominal = 'pain male',
  # fixed = 'male',
  model = '
      mediation.models: 
      interfere ~ pain@apath male age;
      depress ~ interfere@bpath pain male age;',
  parameters = 'indirect = apath*bpath',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(model)

# plot distribution of indirect effect
posterior_plot(model, 'indirect')
