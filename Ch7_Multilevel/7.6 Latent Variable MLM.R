# MULTILEVEL LATENT VARIABLE MODEL WITH RANDOM SLOPES

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
# MULTILEVEL LATENT VARIABLE SPECIFICATION ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = diary,             			# R data frame
  clusterid = 'person',          		# cluster-level identifier
  latent = 'person = b0j b1j',          	# define latent variables
  center = '
    groupmean = pain;				# center at latent group means
    grandmean = pain.mean;',  			# center at grand means
  model = '
    level2: 						# model block label
    b0j ~ intercept pain.mean; 			# level-2 random intercept
    b1j ~ intercept; 				# level-2 random slope
    b0j ~~ b1j;  					# random effect correlation
    level1: 						# model block label
    posaff ~ intercept@b0j pain@b1j; DEBUG: compact_output', 	# level-2 model  
  parameters = '
    var_total = posaff.totalvar + b0j.totalvar;
    rsq_l2coeff = b0j.coefvar / var_total;
    rsq_l1coeff = posaff.coefvar / var_total;
    rsq_l2resid = b0j.residvar / var_total;
    rsq_l1resid = posaff.residvar / var_total;',
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000                			# analysis iterations
)

output(mod1)               		      # print output

# plot parameter distributions
posterior_plot(model3,'posaff')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = diary,             			# R data frame
  clusterid = 'person',          		# cluster-level identifier
  latent = 'person = b0j b1j',          	# define latent variables
  center = '
    groupmean = pain;				# center at latent group means
    grandmean = pain.mean;',  			# center at grand means
  model = '
    level2: 						# model block label
    b0j ~ intercept pain.mean; 			# level-2 random intercept
    b1j ~ intercept; 				# level-2 random slope
    b0j ~~ b1j;  					# random effect correlation
    level1: 						# model block label
    posaff ~ intercept@b0j pain@b1j; DEBUG: compact_output', 	# level-2 model  
  parameters = '
    var_total = posaff.totalvar + b0j.totalvar;
    rsq_l2coeff = b0j.coefvar / var_total;
    rsq_l1coeff = posaff.coefvar / var_total;
    rsq_l2u0 = b0j.residvar / var_total;
    rsq_l1e = posaff.residvar / var_total;',
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000,                			# analysis iterations
  nimps = 20
)

output(mod2)               		      # print output

distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot binned residuals

