# EXAMPLE 8.3 - Bayes MLM w Random Slopes

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/diary.rda"
load(gzcon(url(data_url, open = "rb")))

# default prior2 (wishart prior for level-2 covariance matrix with  ss = 0 and df = -v - 1)
analysis1 <- rblimp(
    data = diary,
    clusterid = 'person',
    fixed = 'female',
    center = '
      groupmean = pain;  # center at latent group means
      grandmean = pain.mean sleep painaccept',  # center at grand means
    model = 'posaff ~ pain@b1 sleep pain.mean@b3 painaccept female | pain', # label within- and between-cluster slope coefficients 
    waldtest = 'b1 = b3', # test whether within- and between-cluster regressions differ
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis1)

# prior1 (wishart prior for level-2 covariance matrix with  ss = 0 and df = v + 1)
analysis2 <- rblimp(
  data = diary,
  clusterid = 'person',
  fixed = 'female',
  center = '
      groupmean = pain;  # center at latent group means
      grandmean = pain.mean sleep painaccept',  # center at grand means
  model = 'posaff ~ pain@b1 sleep pain.mean@b3 painaccept female | pain', # label within- and between-cluster slope coefficients 
  waldtest = 'b1 = b3', # test whether within- and between-cluster regressions differ
  seed = 90291,
  burn = 10000,
  iter = 10000,
  options = 'prior1')

output(analysis2)

# prior3 (wishart prior for level-2 covariance matrix with  ss = 0 and df = v + 1)
analysis3 <- rblimp(
  data = diary,
  clusterid = 'person',
  fixed = 'female',
  center = '
      groupmean = pain;  # center at latent group means
      grandmean = pain.mean sleep painaccept',  # center at grand means
  model = 'posaff ~ pain@b1 sleep pain.mean@b3 painaccept female | pain', # label within- and between-cluster slope coefficients 
  waldtest = 'b1 = b3', # test whether within- and between-cluster regressions differ
  seed = 90291,
  burn = 10000,
  iter = 10000,
  options = 'prior3')

output(analysis3)

# separation prior
analysis4 <- rblimp(
  data = diary,
  clusterid = 'person',
  latent = 'person = painslp',  # define random slope as a separate level-2 latent variable
  fixed = 'female',
  center = '
      groupmean = pain;  # center at latent group means
      grandmean = pain.mean sleep painaccept',  # center at grand means
  model = 'posaff ~ pain@b1 sleep pain.mean@b3 painaccept female pain*painslp@1', # label within- and between-cluster slope coefficients painslp ~ 1@0, # fix mean of random slope latent variable to 0 posaff[person] ~~ painslp', # correlation between random intercepts and random slope latent variable 
  waldtest = 'b1 = b3', # test whether within- and between-cluster regressions differ
  seed = 90291,
  burn = 10000,
  iter = 10000,
  options = 'use_phantom')

output(analysis4)
