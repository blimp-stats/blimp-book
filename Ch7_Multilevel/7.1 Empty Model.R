# RANDOM INTERCEPT REGRESSION

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/mathprobsolve.csv'

# create data frame from github data
mathprobsolve <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT EMPTY MODEL FOR LEVEL-1 VARIABLES ----
#------------------------------------------------------------------------------#

# empty multivariate model for icc's
model1 <- rblimp(
  data = mathprobsolve,
  clusterid = 'school',
  ordinal = 'hispanic',
  model = '{ probsolve stanmath hispanic } ~ intercept | intercept',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model1)
