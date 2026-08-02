# MULTILEVEL LATENT VARIABLE MODEL WITH RANDOM SLOPES AND RANDOM VARIATION

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/diary.csv'

# create data frame from github data
diary <- read.csv(data_url)

#------------------------------------------------------------------------------#
# LOCATION-SCALE MODEL ----
#------------------------------------------------------------------------------#

# base model
mod1 <- rblimp(
  data = diary,             			# R data frame
  clusterid = 'person',          		# cluster-level identifier
  latent = 'person = b0j b1j g0j',          	# define latent variables
  center = '
    groupmean = pain;				# center at latent group means
    grandmean = pain.mean;',  			# center at grand means
  model = '
    level2: 						# model block label
    b0j ~ intercept pain.mean; 			# level-2 random intercept
    b1j ~ intercept; 				# level-2 random slope
    g0j ~ intercept pain.mean; 			# level-2 scale model
    b0j b1j g0j ~~ b0j b1j g0j;  		# random effect correlation
    level1: 						# model block label
    posaff ~ intercept@b0j pain@b1j; 		# level-1 model
    var(posaff) ~ intercept@g0j pain; DEBUG: compact;',	# level-1 scale model
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000                			# analysis iterations
)

output(mod1)               		      # print output
posterior_plot(mod1,'posaff')       # plot parameter distributions

# add r-square effect sizes
mod2 <- rblimp(
  data = diary,             			# R data frame
  clusterid = 'person',          		# cluster-level identifier
  latent = 'person = b0j b1j g0j',          	# define latent variables
  center = '
    groupmean = pain;				# center at latent group means
    grandmean = pain.mean;',  			# center at grand means
  model = '
    level2: 						# model block label
    b0j ~ intercept pain.mean; 			# level-2 random intercept
    b1j ~ intercept; 				# level-2 random slope
    g0j ~ intercept@logvar pain.mean; 			# level-2 scale model
    b0j b1j g0j ~~ b0j b1j g0j;  		# random effect correlation
    level1: 						# model block label
    posaff ~ intercept@b0j pain@b1j; 		# level-1 model
    var(posaff) ~ intercept@g0j pain; DEBUG: compact;',	# level-1 scale model
  parameters = '
    var_total = posaff.totalvar + b0j.totalvar;
    rsq_l2coeff = b0j.coefvar / var_total;
    rsq_l1coeff = posaff.coefvar / var_total;
    rsq_l2resid = b0j.residvar / var_total;
    rsq_l1resid = exp(logvar) / var_total;',
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000                			# analysis iterations
)

output(mod2)               		      # print output

#------------------------------------------------------------------------------#
# MULTILEVEL LATENT VARIABLE SPECIFICATION ----
#------------------------------------------------------------------------------#

# base model
mod1 <- rblimp(
  data = diary,             			# R data frame
  clusterid = 'person',          		# cluster-level identifier
  latent = 'person = b0j b1j g0j',          	# define latent variables
  center = '
    groupmean = pain;				# center at latent group means
    grandmean = pain.mean;',  			# center at grand means
  model = '
    level2: 						# model block label
    b0j ~ intercept pain.mean; 			# level-2 random intercept
    b1j ~ intercept; 				# level-2 random slope
    g0j ~ intercept pain.mean; 			# level-2 scale model
    b0j b1j g0j ~~ b0j b1j g0j;  		# random effect correlation
    level1: 						# model block label
    posaff ~ intercept@b0j pain@b1j; 		# level-1 model
    var(posaff) ~ intercept@g0j pain; DEBUG: compact;',	# level-1 scale model
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000                			# analysis iterations
)