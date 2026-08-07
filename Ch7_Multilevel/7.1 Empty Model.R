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
mod1 <- rblimp(
  data = mathprobsolve,                          # R data frame
  clusterid = 'school',                          # cluster-level identifier
  ordinal = 'hispanic',                          # binary and ordinal variables
  model = 'intercept -> probsolve stanmath hispanic',
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod1)                                     # print output

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = mathprobsolve,                          # R data frame
  clusterid = 'school',                          # cluster-level identifier
  ordinal = 'hispanic',                          # binary and ordinal variables
  model = 'intercept -> probsolve stanmath hispanic',
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod2)                                     # print output

distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot residuals

# save distribution plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/7.1 Distribution Plot.pdf", width = 8.5, height = 11)
plots <- distribution_plot(mod2)                 # plot observed and imputed distributions
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

# save residual plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/7.1 Residuals Plot.pdf", width = 8.5, height = 11)
plots <- residuals_plot(mod2)                    # plot residuals
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

