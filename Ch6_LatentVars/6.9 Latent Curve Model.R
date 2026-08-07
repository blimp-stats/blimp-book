# LATENT CURVE growth GROWTH MODELS

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/schiztrial.csv'

# create data frame from github data
schiztrial <- read.csv(data_url)

#------------------------------------------------------------------------------#
# LINEAR GROWTH MODEL ----
#------------------------------------------------------------------------------#

# full specification
mod1 <- rblimp(
  data = schiztrial,             		# R data frame
  latent = 'baseline growth',          	# define latent variables
  model = '
    structural:  					# model block label
    baseline ~ intercept; 				# estimate the latent mean
    growth ~ intercept; 				# estimate the latent mean
    baseline ~~ growth; 				# factor correlation
    measurement: 					# model block label
    severity0 ~ intercept@0 baseline@1 growth@0;  # @ fixes parameters
    severity1 ~ intercept@0 baseline@1 growth@1;  # @ fixes parameters
    severity2 ~ intercept@0 baseline@1 growth@2;  # @ fixes parameters
    severity3 ~ intercept@0 baseline@1 growth@3;  # @ fixes parameters
    severity0:severity3@resvar;',		# set residual variances equal
  seed = 90291,               			# random number seed
  burn = 25000,               			# warm-up iterations
  iter = 25000                			# analysis iterations
)

# print output
output(mod1)              

# simplified specification
mod2 <- rblimp(
  data = schiztrial,
  latent = 'baseline growth',
  model = '
    structural:
    intercept -> baseline growth;
    baseline ~~ growth;
    measurement:
    baseline -> severity0@1 severity1@1 severity2@1 severity3@1; 
    growth -> severity0@0 severity1@1 severity2@2 severity3@3; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    severity0:severity3@resvar;
    DEBUG: compact',
  seed = 90291,
  burn = 25000,
  iter = 25000) 

# print output
output(mod2)

# plot parameter distributions
posterior_plot(mod2)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = schiztrial,
  latent = 'baseline growth',
  model = '
    structural:
    intercept -> baseline growth;
    baseline ~~ growth;
    measurement:
    baseline -> severity0@1 severity1@1 severity2@1 severity3@1; 
    growth -> severity0@0 severity1@1 severity2@2 severity3@3; 
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    severity0:severity3@resvar;
    DEBUG: compact',
  seed = 90291,
  burn = 25000,
  iter = 25000,
  nimps = 20) 

# print output
output(mod3)

distribution_plot(mod3)
residuals_plot(mod3)
