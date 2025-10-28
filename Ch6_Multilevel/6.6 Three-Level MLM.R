# EXAMPLE 8.5 - Bayes 3-Level MLM w Cross-Level Interaction

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/problemsolving3level.rda"
load(gzcon(url(data_url, open = "rb")))


problemsolving3level[problemsolving3level == 999] <- NA
save(problemsolving3level, file = '~/desktop/problemsolving3level.rda')

# default prior2: wishart prior for level-2 covariance matrix with  ss = 0 and df = -(v + 1)
analysis1 <- rblimp(
    data = problemsolving3level,
    clusterid = 'school student',
    ordinal = 'frlunch condition',
    fixed = 'month7 condition',
    center = 'grandmean = stanmath frlunch teachexp',
    model = 'probsolve ~ month7 stanmath frlunch teachexp condition month7*condition | month7', # random intercepts and slopes at level-2 and level-3 
    simple = ' month7 | condition', # conditional effect of time within condition 
    seed = 90291,
    burn = 15000,
    iter = 20000)

output(analysis1)
simple_plot(probsolve ~ month7 | condition, analysis1)

# prior1: wishart prior for level-2 covariance matrix with  ss = 0 and df = v + 1
analysis2 <- rblimp(
  data = problemsolving3level,
  clusterid = 'school student',
  ordinal = 'frlunch condition',
  fixed = 'month7 condition',
  center = 'grandmean = stanmath frlunch teachexp',
  model = 'probsolve ~ month7 stanmath frlunch teachexp condition month7*condition | month7', # random intercepts and slopes at level-2 and level-3 
  simple = ' month7 | condition', # conditional effect of time within condition 
  seed = 90291,
  burn = 15000,
  iter = 20000,
  options = 'prior1')

output(analysis2)
simple_plot(probsolve ~ month7 | condition, analysis2)
