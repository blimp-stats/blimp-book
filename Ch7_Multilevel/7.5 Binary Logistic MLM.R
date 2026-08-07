# GENERALIZED LINEAR MIXED MODEL WITH DISCRETE OUTCOME

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/worksat.csv'

# create data frame from github data
worksat <- read.csv(data_url)

#------------------------------------------------------------------------------#
# ESTIMATE INTRACLASS CORRELATIONS ----
#------------------------------------------------------------------------------#

mod0 <- rblimp(
  data = worksat,                                # R data frame
  clusterid = 'team',                            # cluster-level identifier
  ordinal = 'turnover male',                     # binary and ordinal variables
  model = '{ turnover lmx male } ~ intercept',
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod0)                                     # print output

#------------------------------------------------------------------------------#
# FIT LOGISTIC MODEL WITH COMBINED MODEL SPECIFICATION ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = worksat,                                # R data frame
  clusterid = 'team',                            # cluster-level identifier
  ordinal = 'male',                              # binary and ordinal variables
  nominal = 'turnover',                          # nominal variables (auto dummy coded)
  center = 'groupmean = lmx; grandmean = lmx.mean climate',  # center predictors
  model = 'turnover ~ lmx male lmx.mean climate;',
  seed = 90291,                                  # random number seed
  burn = 25000,                                  # warm-up iterations
  iter = 25000)                                  # analysis iterations

output(mod1)                                     # print output

posterior_plot(mod1,'turnover')                  # plot parameter distributions

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = worksat,                                # R data frame
  clusterid = 'team',                            # cluster-level identifier
  ordinal = 'male',                              # binary and ordinal variables
  nominal = 'turnover',                          # nominal variables (auto dummy coded)
  center = 'groupmean = lmx; grandmean = lmx.mean climate',  # center predictors
  model = 'turnover ~ lmx male lmx.mean climate',
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod2)                                     # print output

distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot binned residuals

# save distribution plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/7.5 Distribution Plot.pdf", width = 8.5, height = 11)
plots <- distribution_plot(mod2)                 # plot observed and imputed distributions
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

# save residual plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/7.5 Residuals Plot.pdf", width = 8.5, height = 11)
plots <- residuals_plot(mod2)                    # plot binned residuals
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file
