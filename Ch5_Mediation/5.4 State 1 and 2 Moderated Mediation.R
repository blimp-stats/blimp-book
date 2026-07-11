# MEDIATION MODEL WITH FIRST- AND SECOND-STAGE MODERATION OF THE A AND B PATHS

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
# MODERATOR OF A AND B PATH ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = discrimination,             		# R data frame
  ordinal = 'female',          			# binary and ordinal variables
  transform = 'age16 = age – 16',  		# centered age variable
  model = '    					# label slope parameters
    victim ~ discrim@a1 age16 discrim*age16@a3 female;  # a path
    internalize ~ victim@b1 discrim age16 victim*age16@b4 female;', # b path 
  parameters = '
    indirect14 = (a1 - 2*a3)*(b1 - 2*b4); 	# conditional indirect effect
    indirect15 = (a1 - 1*a3)*(b1 - 1*b4);	# conditional indirect effect
    indirect16 = (a1 + 0*a3)*(b1 + 0*b4);	# conditional indirect effect
    indirect17 = (a1 + 1*a3)*(b1 + 1*b4);',	# conditional indirect effect
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000                			# analysis iterations
)

output(mod1)               		      # print output
posterior_plot(mod1, 'indirect14') 		# plot conditional indirect
posterior_plot(mod1, 'indirect15') 		# plot conditional indirect
posterior_plot(mod1, 'indirect16') 		# plot conditional indirect
posterior_plot(mod1, 'indirect17') 		# plot conditional indirect

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = discrimination,             		# R data frame
  ordinal = 'female',          			# binary and ordinal variables
  transform = 'age16 = age – 16',  		# centered age variable
  model = '    					# label slope parameters
    victim ~ discrim@a1 age16 discrim*age16@a3 female;  # a path
    internalize ~ victim@b1 discrim age16 victim*age16@b4 female;', # b path 
  parameters = '
    indirect14 = (a1 - 2*a3)*(b1 - 2*b4); 	# conditional indirect effect
    indirect15 = (a1 - 1*a3)*(b1 - 1*b4);	# conditional indirect effect
    indirect16 = (a1 + 0*a3)*(b1 + 0*b4);	# conditional indirect effect
    indirect17 = (a1 + 1*a3)*(b1 + 1*b4);',	# conditional indirect effect
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000,               			# analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot residuals
