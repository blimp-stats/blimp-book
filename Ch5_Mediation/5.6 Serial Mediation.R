# MULTIPLE MEDIATORS: SERIAL MEDIATION

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/discrimination.csv'

# create data frame from github data
discrimination <- read.csv(data_url)

#------------------------------------------------------------------------------#
# SERIAL MEDIATION MODEL ----
#------------------------------------------------------------------------------#

# unstandardized indirect effect
mod1 <- rblimp(
  data = discrimination,             		# R data frame
  ordinal = 'female',          			# binary and ordinal variables
  model = '  						# label slope parameters
    victim ~ discrim@a1v female age; 		# a path
    familism ~ victim@a1f discrim female age;  # a path
    internalize ~ familism@b1 victim discrim age female;',  # b path
  parameters = 'indirect = a1v * a1f * b1',	# compute indirect effect
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000                			# analysis iterations
)

output(mod1)               		      # print output
posterior_plot(mod1, 'indirect')       	# plot indirect effect

# add standardized indirect effect
mod2 <- rblimp(
  data = discrimination,             		# R data frame
  ordinal = 'female',          			# binary and ordinal variables
  model = '  						# label slope parameters
    mediation:
    victim ~ discrim@a1v female age; 		# a path
    familism ~ victim@a1f discrim female age;  # a path
    internalize ~ familism@b1 victim discrim age female;
    predictors:
    discrim ~ age female; DEBUG: compact_output',   				# predictor model
  parameters = '
    indirect = a1v * a1f * b1;
    sd_ratio = sqrt(discrim.totalvar / internalize.totalvar);  # sd ratio
    indirect_std = indirect * sd_ratio;',	# standardized indirect
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000                			# analysis iterations
)

output(mod2)               		      # print output
posterior_plot(mod2, 'indirect')       	# plot indirect effect
posterior_plot(mod2, 'indirect_std')       	# plot indirect effect

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = discrimination,             		# R data frame
  ordinal = 'female',          			# binary and ordinal variables
  model = '  						# label slope parameters
    victim ~ discrim@a1v female age; 		# a path
    familism ~ victim@a1f discrim female age;  # a path
    internalize ~ familism@b1 victim discrim age female;',  # b path
  parameters = 'indirect = a1v * a1f * b1',	# compute indirect effect
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000,               			# analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

distribution_plot(mod3)                          # plot observed and imputed distributions
residuals_plot(mod3)                             # plot residuals