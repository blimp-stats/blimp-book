# EXAMPLE 10.8 - Bayes Moderated Regression with Group by Latent Interaction

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/pain.rda"
load(gzcon(url(data_url, open = "rb")))

# basic model with group by latent interaction
analysis1 <- rblimp(
  data = pain,
  ordinal = 'male pain dep1:dep7 dis1:dis6',
  latent = 'ldepress ldisability',
  model = '
      focal.model: 
      ldisability ~ ldepress male ldepress*male pain; # label parameters in the focal regression model 
      factor.models: 
      ldepress -> dep1:dep7; # measurement models with first loading fixed to 1 
      ldisability -> dis1:dis6; 
      predictor.models: 
      pain male ldepress ~~ pain male ldepress;', # multivariate model for predictors 
  simple = 'ldepress | male', # simple effects by gender 
  seed = 90291,
  burn = 40000,
  iter = 10000)

output(analysis1)
simple_plot(ldisability ~ ldepress | male, analysis1)

# add auxiliary variables
analysis2 <- rblimp(
  data = pain,
  ordinal = 'male pain dep1:dep7 dis1:dis6',
  latent = 'ldepress ldisability',
  model = '
      focal.model: 
      ldisability ~ ldepress male ldepress*male pain; # label parameters in the focal regression model 
      factor.models: 
      ldepress -> dep1:dep7; # measurement models with first loading fixed to 1 
      ldisability -> dis1:dis6; 
      predictor.models: 
      pain male ldepress ~~ pain male ldepress; # multivariate model for predictors 
      control interfere ~ ldisability ldepress male pain;', # automatic sequential specification for auxiliary variables 
  simple = 'ldepress | male', # simple effects by gender 
  seed = 90291,
  burn = 40000,
  iter = 10000)

output(analysis2)
simple_plot(ldisability ~ ldepress | male, analysis2)
