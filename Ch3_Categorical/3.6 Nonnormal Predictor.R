# EXAMPLE 10.3 - Bayes Yeo-Johnson Transformation for a Nonnormal Predictor

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/alcoholuse.rda"
load(gzcon(url(data_url, open = "rb")))

# assuming normal alcage predictor
analysis1 <- rblimp(
  data = alcoholuse,
  ordinal = 'male college drinker',
  fixed = 'male age',
  model = 'logit(drinker) ~ alcage college age male', # automatic multivariate normal distribution for incomplete predictors and latent response scores 
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(analysis1)

# yeo-johnson transformation for nonnormal predictor
analysis2 <- rblimp(
    data = alcoholuse,
    ordinal = 'male college drinker',
    fixed = 'male age',
    model = '
      focal.model: 
      logit(drinker) ~ alcage college age male;
      predictor.models: 
      yjt(alcage - 16) ~ age male; # sequential specification for predictors 
      college ~ alcage age male', 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis2)
