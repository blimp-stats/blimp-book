# NEGATIVE BINOMIAL REGRESSION FOR COUNT OUTCOMES

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(ggplot2)
library(patchwork)
library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflammation <- read.csv(data_url)

#------------------------------------------------------------------------------#
# NEGATIVE BINOMIAL REGRESSION MODEL ----
#------------------------------------------------------------------------------#

# regression model with a count outcome
mod1 <- rblimp(
  data = inflammation,             		             # R data frame
  ordinal = 'els female',          		             # binary and ordinal variables 
  count = 'hdd',                                   # count outcome with negative binomial model
  center = 'inflam els female age',	               # center predictors  
  model = 'hdd ~ inflam els female age;',          # regression model
  seed = 90291,               		                 # random number seed
  burn = 10000,               		                 # warm-up iterations
  iter = 10000)                		                 # analysis iteration

output(mod1)               		                     # print output
posterior_plot(mod1)                               # plot parameter distributions

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# regression model with a count outcome
mod2 <- rblimp(
  data = inflammation,             		             # R data frame
  ordinal = 'els female',          		             # binary and ordinal variables 
  count = 'hdd',                                   # count outcome with negative binomial model
  center = 'inflam els female age',	               # center predictors  
  model = 'hdd ~ inflam els female age;',          # regression model
  seed = 90291,               		                 # random number seed
  burn = 10000,               		                 # warm-up iterations
  iter = 10000,                                    # analysis iteration
  nimps = 20)                		                   # save 20 imputations  

output(mod2)               		                     # print output

source("/Users/craig/Documents/Claude/Projects/Blimp Book/rblimp_cleaned_functions.R")

# plot distributions and binned residuals
distribution_plot(mod2)
residuals_plot(mod2)
