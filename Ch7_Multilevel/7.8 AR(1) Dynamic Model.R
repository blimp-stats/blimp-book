# AR(1) DSEM WITH CARRYOVER

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
# FIT EMPTY MODEL FOR LEVEL-1 VARIABLES ----
#------------------------------------------------------------------------------#

# empty multivariate model for icc's
mod0 <- rblimp(
  data = diary,
  clusterid = 'person',
  model = '{ posaff pain } ~ intercept',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(mod0)

#------------------------------------------------------------------------------#
# AR(1) MODEL ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = diary,             			# R data frame
  clusterid = 'person',          		# cluster-level identifier
  timeid = 'day',          			# occasion-level identifer
  latent = 'person = b0j b1j',          	# define latent variables
  model = '
    level2: 						# model block label
    b0j ~ intercept; 			# level-2 random intercept
    b1j ~ intercept; 					# level-2 random slope
    b0j ~~ b1j;  					# random effect correlation
    level1: 						# model block label
    lag_posaff = posaff.lag – b0j; 			# definition variable
    posaff ~ intercept@b0j lag_posaff@b1j;', # level-1 model  
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000                			# analysis iterations
)

output(mod1)               		      # print output
posterior_plot(mod1)               		# plot parameter distributions

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = diary,             			# R data frame
  clusterid = 'person',          		# cluster-level identifier
  timeid = 'day',          			# occasion-level identifer
  latent = 'person = b0j b1j',          	# define latent variables
  model = '
    level2: 						# model block label
    b0j ~ intercept; 			# level-2 random intercept
    b1j ~ intercept; 					# level-2 random slope
    b0j ~~ b1j;  					# random effect correlation
    level1: 						# model block label
    lag_posaff = posaff.lag – b0j; 			# definition variable
    posaff ~ intercept@b0j lag_posaff@b1j;', # level-1 model  
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000,                			# analysis iterations
  nimps = 20                     # save 20 imputations
)

output(mod2)  # print output

distribution_plot(mod2)
residuals_plot(mod2)

