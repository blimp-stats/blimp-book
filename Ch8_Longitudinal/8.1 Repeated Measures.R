# REPEATED MEASURES ANALYSIS WITH UNSTRUCTURED CHANGE

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/trial_wide.csv'

# create data frame from github data
trial <- read.csv(data_url)

# FIT REPEATED MEASURES MODEL ----

model1 <- rblimp(
  data = trial,
  latent = 'subjects', # Define latent variable for subjects factor
  model = '
    # Random subjects factor with mean = 0 and loadings = 1
    subjects ~ intercept@0;
    severity0 ~ intercept@mu0 subjects@1; # @ labels the means and sets subject factor loadings to 1
    severity1 ~ intercept@mu1 subjects@1;
    severity2 ~ intercept@mu2 subjects@1;
    severity3 ~ intercept@mu3 subjects@1;
    severity4 ~ intercept@mu4 subjects@1;
    severity5 ~ intercept@mu5 subjects@1;
    severity6 ~ intercept@mu6 subjects@1;
  ',
  waldtest = c(
    # Test of null that all means are equal
    'mu0 = mu1; mu1 = mu2; mu2 = mu3; mu3 = mu4; mu4 = mu5; mu5 = mu6;',
    # Tests of pairwise comparisons
    'mu0 = mu1','mu1 = mu2','mu1 = mu3','mu2 = mu3','mu3 = mu4','mu4 = mu5','mu5 = mu6'),
  seed = 90291, # Integer random number seed
  burn = 10000, # Warm up iterations
  iter = 10000) # Iterations for analysis

output(model1)

# FIT REPEATED MEASURES MODEL WITH PREDICTOR ----

model2 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  latent = 'subjects', # Define latent variable for subjects factor
  center = 'male',
  model = '
    # Random subjects factor with mean = 0 and loadings = 1
    subjects ~ intercept@0;
    severity0 ~ intercept@mu0 drug@dif0 male@cov subjects@1; # @ labels the means and sets subject factor loadings to 1
    severity1 ~ intercept@mu1 drug@dif1 male@cov subjects@1;
    severity2 ~ intercept@mu2 drug@dif2 male@cov subjects@1;
    severity3 ~ intercept@mu3 drug@dif3 male@cov subjects@1;
    severity4 ~ intercept@mu4 drug@dif4 male@cov subjects@1;
    severity5 ~ intercept@mu5 drug@dif5 male@cov subjects@1;
    severity6 ~ intercept@mu6 drug@dif6 male@cov subjects@1;
  ',
  waldtest = 'dif0 = dif1; dif1 = dif2; dif2 = dif3; dif3 = dif4; dif4 = dif5; dif5 = dif6;', # group-by-time interaction
  seed = 90291, # Integer random number seed
  burn = 10000, # Warm up iterations
  iter = 10000) # Iterations for analysis

output(model2)



