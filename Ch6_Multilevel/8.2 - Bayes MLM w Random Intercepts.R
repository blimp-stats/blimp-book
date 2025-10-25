# EXAMPLE 8.2 - Bayes MLM w Random Intercepts

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/problemsolving2level.rda"
load(gzcon(url(data_url, open = "rb")))

# default prior2 (igamma ss = 0 and df = -2) with multivariate specification for incomplete predictors
analysis1 <- rblimp(
    data = problemsolving2level,
    clusterid = 'school',
    ordinal = 'frlunch',
    fixed = 'probsolve1 condition',
    model = 'probsolve2 ~ probsolve1 stanmath frlunch teachexp condition', # automatic multivariate distribution for incomplete predictors and latent response scores 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis1)

# prior1 (igamma ss = 1 and df = 2) with multivariate specification for incomplete predictors
analysis2 <- rblimp(
  data = problemsolving2level,
  clusterid = 'school',
  ordinal = 'frlunch',
  fixed = 'probsolve1 condition',
  model = 'probsolve2 ~ probsolve1 stanmath frlunch teachexp condition', # automatic multivariate distribution for incomplete predictors and latent response scores 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  options = 'prior1')

output(analysis2)

# prior3 (igamma ss = 0 and df = 0) with multivariate specification for incomplete predictors
analysis3 <- rblimp(
  data = problemsolving2level,
  clusterid = 'school',
  ordinal = 'frlunch',
  fixed = 'probsolve1 condition',
  model = 'probsolve2 ~ probsolve1 stanmath frlunch teachexp condition', # automatic multivariate distribution for incomplete predictors and latent response scores 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  options = 'prior3')

output(analysis3)

# default prior2 with sequential specification for incomplete predictors
analysis4 <- rblimp(
  data = problemsolving2level,
  clusterid = 'school',
  ordinal = 'frlunch',
  fixed = 'condition',
  model = '
      focal.model: probsolve2 ~ probsolve1 stanmath frlunch teachexp condition;
      predictor.models: stanmath frlunch probsolve1 teachexp ~ condition', # automatic sequential specification for level-2 variables followed by level-1 variables 
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(analysis4)
