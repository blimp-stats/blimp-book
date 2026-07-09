# MODERATED NON-LINEAR FACTOR ANALYSIS

# plotting functions
# source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source("/Users/craig/Documents/Claude/Projects/Blimp Book/rblimp_cleaned_functions.R")

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(ggplot2)
library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/adhd.csv'

# create data frame from github data
adhd <- read.csv(data_url)

#------------------------------------------------------------------------------#
# CFA ----
#------------------------------------------------------------------------------#

# one-factor cfa
mod1 <- rblimp(
  data = adhd,
  ordinal = 'attent1:attent9',
  latent = 'attention',
  model = '
    attention@1;
    attention -> attent1@load1 attent2:attent9;',
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(mod1)
standardized(mod1)

# cfa with correlated residuals
mod2 <- rblimp(
  data = adhd,
  ordinal = 'attent1:attent9',
  latent = 'attention',
  model = '
    attention@1;
    attention -> attent1@load1 attent2:attent9;
    attent2 ~~ attent8;
    attent7 ~~ attent9;',
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(mod2)
standardized(mod2)

#------------------------------------------------------------------------------#
# MODERATORS PREDICTING FACTOR MEAN AND VARIANCE ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = adhd,
  ordinal = 'study male adhd attent1:attent9',
  latent = 'attention',
  center = 'alphaeeg age',
  model = '
    structural:
    attention ~ alphaeeg alphaeeg^2 age age^2 study male adhd;
    var(attention) ~ 1@0 alphaeeg age study male adhd;
    measurement:
    attention -> attent1@load1 attent2:attent9;
    attent2 ~~ attent8;
    attent7 ~~ attent9;',
  seed = 90291,
  burn = 50000,
  iter = 50000)

# print output
output(mod3)
standardized(mod3)

#------------------------------------------------------------------------------#
# MODERATORS PREDICTING MEASUREMENT MODEL PARAMETERS ----
#------------------------------------------------------------------------------#

mod4 <- rblimp(
  data = adhd,
  ordinal = 'study male adhd attent1:attent9',
  latent = 'attention',
  center = 'alphaeeg age',
  model = '
    structural:
    attention ~ alphaeeg alphaeeg^2 age age^2 study male adhd;
    var(attention) ~ 1@0 alphaeeg age study male adhd;
    measurement:
    attention -> attent1@load1 attent2:attent9;
    attent2 ~~ attent8;
    attent7 ~~ attent9;
    # moderation effects    
    age study male adhd -> attent1:attent3 attent5 attent7:attent9;
    age*attention study*attention male*attention adhd*attention -> attent1:attent3 attent5 attent7:attent9;DEBUG: compact_output',
  seed = 90291,
  burn = 100000,
  iter = 100000)

# print output
output(mod4)
standardized(mod4)

#------------------------------------------------------------------------------#
# TRIMMED LOADINGS ----
#------------------------------------------------------------------------------#

mod5 <- rblimp(
  data    = adhd,
  ordinal = 'study male adhd attent1:attent9',
  latent  = 'attention',
  center  = 'alphaeeg age',
  model = '
  structural: 							
  attention ~ alphaeeg alphaeeg^2 age age^2 study male adhd; 
  var(attention) ~ 1@0 alphaeeg age study male adhd;  
  measurement: 						
  attention –> attent1@load1 attent2:attent9; 	
  attent2 ~~ attent8;  					
  attent7 ~~ attent9;  					
  age study male adhd -> 
    attent1:attent3 attent5 attent7:attent9; 	# all intercepts
  study*attention -> 
    attent2:attent3 attent7 attent8:attent9; 	  # retained interactions
  age*attention male*attention -> attent7;', 		# retained interactions
  seed = 90291, 
  burn = 100000, 
  iter = 100000)

# print output
output(mod5)
summary(mod5@estimates[,7])
mod5@estimates

#------------------------------------------------------------------------------#
# TRIMMED LOADINGS AND INTERCEPTS ----
#------------------------------------------------------------------------------#

mod6 <- rblimp(
  data    = adhd,
  ordinal = 'study male adhd attent1:attent9',
  latent  = 'attention',
  center  = 'alphaeeg age',
  model = '
    structural:
    attention ~ alphaeeg alphaeeg^2 age age^2 study male adhd;
    var(attention) ~ 1@0 alphaeeg age study male adhd;
    measurement:
    attention -> attent1@load1 attent2:attent9;
    attent2 ~~ attent8;
    attent7 ~~ attent9;
    # dif
    study -> attent1;
    adhd -> attent1;
    age -> attent2:attent3 attent8;
    study*attention study -> attent2:attent3 attent7:attent9;
    age*attention male*attention age male -> attent7;',
  seed = 90291, 
  burn = 100000, 
  iter = 100000)
