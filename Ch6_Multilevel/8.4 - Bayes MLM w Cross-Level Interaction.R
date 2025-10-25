# EXAMPLE 8.4 - Bayes MLM w Cross-Level Interaction

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/employee.rda"
load(gzcon(url(data_url, open = "rb")))

# default prior2: wishart prior for level-2 covariance matrix with  ss = 0 and df = -(v + 1)
analysis1 <- rblimp(
    data = employee,
    clusterid = 'team',
    fixed = 'male',
    center = '
      groupmean = lmx;  # center at latent group means
      grandmean =  male climate cohesion',  # center at grand means
    model = 'empower ~ lmx male cohesion climate lmx*climate | lmx', # automatic multivariate distribution for incomplete predictors and latent response scores 
    simple = ' lmx | climate', # conditional effect of lmx at different levels of climate 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis1)
simple_plot(empower ~ lmx | climate, analysis1)

# prior1: wishart prior for level-2 covariance matrix with  ss = 0 and df = v + 1
analysis2 <- rblimp(
  data = employee,
  clusterid = 'team',
  fixed = 'male',
  center = '
      groupmean = lmx;  # center at latent group means
      grandmean =  male climate cohesion',  # center at grand means
  model = 'empower ~ lmx male cohesion climate lmx*climate | lmx', # automatic multivariate distribution for incomplete predictors and latent response scores 
  simple = ' lmx | climate', # conditional effect of lmx at different levels of climate 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  options = 'prior1')

output(analysis2)
simple_plot(empower ~ lmx | climate, analysis2)

# prior3: wishart prior for level-2 covariance matrix with ss = 0 and df = 0
analysis3 <- rblimp(
  data = employee,
  clusterid = 'team',
  fixed = 'male',
  center = '
      groupmean = lmx;  # center at latent group means
      grandmean =  male climate cohesion',  # center at grand means
  model = 'empower ~ lmx male cohesion climate lmx*climate | lmx', # automatic multivariate distribution for incomplete predictors and latent response scores 
  simple = ' lmx | climate', # conditional effect of lmx at different levels of climate 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  options = 'prior3')

output(analysis3)
simple_plot(empower ~ lmx | climate, analysis3)
