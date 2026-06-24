# LATENT VARIABLE BY DISCRETE VARIABLE INTERACTION

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/workbehavior.csv'

# create data frame from github data
workbeh <- read.csv(data_url)

#------------------------------------------------------------------------------#
# LATENT BY BINARY INTERACTION ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = workbeh,                                 # R data frame
  ordinal = 'conrating workbeh1:workbeh6 orgcon1:orgcon6',  # binary and ordinal variables
  latent = 'orgconstr wrkbehave',                 # define latent variables
  model = '
    structural:                                   # model block label
    wrkbehave ~ orgconstr conrating orgconstr*conrating;  # latent product
    wrkbehave@1;                                  # fix residual variance to 1
    predictor:                                    # model block label
    orgconstr ~~ conrating;                       # latent correlation
    orgconstr@1;                                  # fix variance to 1
    measurement:                                  # model block label
    wrkbehave -> workbeh1@wload1 workbeh2:workbeh6;  # measurement model with estimated loadings
    orgconstr -> orgcon1@oload1 orgcon2:orgcon6; DEBUG: compact_output;', # measurement model with estimated loadings           
  simple = 'orgconstr | conrating',               # conditional effects
  seed = 90291,                                   # random number seed
  burn = 20000,                                   # warm-up iterations
  iter = 20000)                                   # analysis iterations

output(mod1)                                      # print output
posterior_plot(mod1, 'wrkbehave')                 # plot parameter distributions
round(standardized(mod1),3)                       # print standardized estimates in one table
simple_plot(wrkbehave ~ orgconstr | conrating, mod1) # plot conditional effects

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = workbeh,                                 # R data frame
  ordinal = 'conrating workbeh1:workbeh6 orgcon1:orgcon6',  # binary and ordinal variables
  latent = 'orgconstr wrkbehave',                 # define latent variables
  model = '
    structural:                                   # model block label
    wrkbehave ~ orgconstr conrating orgconstr*conrating;  # latent product
    wrkbehave@1;                                  # fix residual variance to 1
    predictor:                                    # model block label
    orgconstr ~~ conrating;                       # latent correlation
    orgconstr@1;                                  # fix variance to 1
    measurement:                                  # model block label
    wrkbehave -> workbeh1@wload1 workbeh2:workbeh6;  # measurement model with estimated loadings
    orgconstr -> orgcon1@oload1 orgcon2:orgcon6;', # measurement model with estimated loadings           
  simple = 'orgconstr | conrating',               # conditional effects
  seed = 90291,                                   # random number seed
  burn = 20000,                                   # warm-up iterations
  iter = 20000,                                   # analysis iterations
  nimps = 20                                      # save 20 imputed data sets
)

output(mod2)                                     # print output
distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot residuals
