# SINGLE MEDIATOR MODEL

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/pain.csv'

# create data frame from github data
pain <- read.csv(data_url)

# FIT MODEL ----

# single mediator model
model <- rblimp(
    data = pain,
    nominal = 'pain male',
    # fixed = 'male',
    model = '
      mediation.models: 
      interfere ~ pain@a male age;
      depress ~ interfere@b pain male age;',
    parameters = 'indirect = a*b',
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(model)

# plot distribution of indirect effect
posterior_plot(model, 'indirect')
