# RANDOM-INTERCEPT CROSS-LAGGED PANEL MODEL

# plotting functions
# source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source("/Users/craig/Dropbox/Claude/Projects/Blimp Book/rblimp_cleaned_functions.R")

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/loneliness.csv'

# create data frame from github data
loneliness <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT RICLPM MODEL ----
#------------------------------------------------------------------------------#

# unconstrained with occasion-specific slopes
mod1 <- rblimp(
  data = loneliness,             		# R data frame
  latent = 'anxdep lonely',          		# define latent variables
  model = '
    anxdep1w = anxdep1 - (a01 + anxdep);  	# definition variable
    anxdep2w = anxdep2 - (a02 + anxdep); 	# definition variable
    anxdep3w = anxdep3 - (a03 + anxdep); 	# definition variable
    lonely1w = lonely1 - (b01 + lonely); 	# definition variable
    lonely2w = lonely2 - (b02 + lonely); 	# definition variable
    lonely3w = lonely3 - (b03 + lonely); 	# definition variable
    between.person: 					# model block label
    anxdep ~~ lonely;  				# random intercepts
    within.person: 					# model block label
    anxdep1 ~ intercept@a01 anxdep@1; 		# equations with labels
    anxdep2 ~ intercept@a02 anxdep1w@a12 lonely1w@a22 anxdep@1;
    anxdep3 ~ intercept@a03 anxdep2w@a13 lonely2w@a23 anxdep@1;
    anxdep4 ~ intercept@a04 anxdep3w@a14 lonely3w@a24 anxdep@1;
    lonely1 ~ intercept@b01 lonely@1;
    lonely2 ~ intercept@b02 lonely1w@b12 anxdep1w@b22 lonely@1;
    lonely3 ~ intercept@b03 lonely2w@b13 anxdep2w@b23 lonely@1;
    lonely4 ~ intercept@b04 lonely3w@b14 anxdep3w@b24 lonely@1;  
    anxdep1 ~~ lonely1; 				# residual correlation
    anxdep2 ~~ lonely2; 				# residual correlation
    anxdep3 ~~ lonely3; 				# residual correlation
    anxdep4 ~~ lonely4;', 				# residual correlation
  waldtest = 'a12 = a13; a13 = a14; a22 = a23; a23 = a24; 
  b12 = b13; b13 = b14; b22 = b23; b23 = b24;',  # test equality of slopes
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000                			# analysis iterations
)

output(mod1)               		      # print output

# equality constraints on occasion-specific slopes
mod2 <- rblimp(
  data = loneliness,
  latent = 'anxdep lonely',
  model = ' 
    anxdep1w = anxdep1 - (a01 + anxdep);  # definition variables
    anxdep2w = anxdep2 - (a02 + anxdep);
    anxdep3w = anxdep3 - (a03 + anxdep);
    lonely1w = lonely1 - (b01 + lonely);
    lonely2w = lonely2 - (b02 + lonely);
    lonely3w = lonely3 - (b03 + lonely);
    random.intercepts:
    anxdep ~~ lonely;
    lagged.anxdep:
    anxdep1 ~ intercept@a01 anxdep@1;
    anxdep2 ~ intercept@a02 anxdep1w@a1 lonely1w@a2 anxdep@1;
    anxdep3 ~ intercept@a03 anxdep2w@a1 lonely2w@a2 anxdep@1;
    anxdep4 ~ intercept@a04 anxdep3w@a1 lonely3w@a2 anxdep@1;
    lagged.lonely:
    lonely1 ~ intercept@b01 lonely@1;
    lonely2 ~ intercept@b02 lonely1w@b1 anxdep1w@b2 lonely@1;
    lonely3 ~ intercept@b03 lonely2w@b1 anxdep2w@b2 lonely@1;
    lonely4 ~ intercept@b04 lonely3w@b1 anxdep3w@b2 lonely@1;
    covariances:
    anxdep1 ~~ lonely1;
    anxdep2 ~~ lonely2;
    anxdep3 ~~ lonely3;
    anxdep4 ~~ lonely4; 
    DEBUG: compact',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(mod2)


#------------------------------------------------------------------------------#
# FIT RICLPM MODEL WITH BETWEEN-PERSON PREDICTOR ----
#------------------------------------------------------------------------------#

model2 <- rblimp(
  data = lonely,
  latent = 'anxdep lonely',
  model = ' 
    anxdep1w = anxdep1 - (a01 + anxdep);  # definition variables
    anxdep2w = anxdep2 - (a02 + anxdep);
    anxdep3w = anxdep3 - (a03 + anxdep);
    lonely1w = lonely1 - (b01 + lonely);
    lonely2w = lonely2 - (b02 + lonely);
    lonely3w = lonely3 - (b03 + lonely);
    random_intercepts:
    female -> anxdep lonely;
    anxdep ~~ lonely;
    anxdep:
    anxdep1 ~ intercept@a01 anxdep@1;
    anxdep2 ~ intercept@a02 anxdep@1 anxdep1w lonely1w;
    anxdep3 ~ intercept@a03 anxdep@1 anxdep2w lonely2w;
    anxdep4 ~ intercept@a04 anxdep@1 anxdep3w lonely3w;
    lonely:
    lonely1 ~ intercept@b01 lonely@1;
    lonely2 ~ intercept@b02 lonely@1 lonely1w anxdep1w;
    lonely3 ~ intercept@b03 lonely@1 lonely2w anxdep2w;
    lonely4 ~ intercept@b04 lonely@1 lonely3w anxdep3w;
    covariances:
    anxdep1 ~~ lonely1;
    anxdep2 ~~ lonely2;
    anxdep3 ~~ lonely3;
    anxdep4 ~~ lonely4;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

output(model2)


#------------------------------------------------------------------------------#
# FIT RICLPM MODEL WITH GROUP-SPECIFIC EFFECTS ----
#------------------------------------------------------------------------------#

model3 <- rblimp(
  data = lonely,
  latent = 'anxdep lonely',
  model = ' 
    anxdep1w = anxdep1 - (a01 + anxdep);  # definition variables
    anxdep2w = anxdep2 - (a02 + anxdep);
    anxdep3w = anxdep3 - (a03 + anxdep);
    lonely1w = lonely1 - (b01 + lonely);
    lonely2w = lonely2 - (b02 + lonely);
    lonely3w = lonely3 - (b03 + lonely);
    random_intercepts:
    female -> anxdep lonely;
    anxdep ~~ lonely;
    anxdep:
    anxdep1 ~ intercept@a01 anxdep@1;
    anxdep2 ~ intercept@a02 anxdep@1 anxdep1w lonely1w lonely1w*female;
    anxdep3 ~ intercept@a03 anxdep@1 anxdep2w lonely2w lonely2w*female;
    anxdep4 ~ intercept@a04 anxdep@1 anxdep3w lonely3w lonely3w*female;
    lonely:
    lonely1 ~ intercept@b01 lonely@1;
    lonely2 ~ intercept@b02 lonely@1 lonely1w anxdep1w anxdep1w*female;
    lonely3 ~ intercept@b03 lonely@1 lonely2w anxdep2w anxdep2w*female;
    lonely4 ~ intercept@b04 lonely@1 lonely3w anxdep3w anxdep3w*female;
    covariances:
    anxdep1 ~~ lonely1;
    anxdep2 ~~ lonely2;
    anxdep3 ~~ lonely3;
    anxdep4 ~~ lonely4;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(model3)

