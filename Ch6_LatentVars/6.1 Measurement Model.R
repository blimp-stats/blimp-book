# LATENT VARIABLE MODELS WITH CONTINUOUS VARIABLES

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflamm <- read.csv(data_url)

#------------------------------------------------------------------------------#
# LATENT VARIABLE REGRESSION MODEL ----
#------------------------------------------------------------------------------#

# factor mean fixed at 0 and first loading fixed to 1
mod1 <- rblimp(
  data = inflamm,                                # R data frame
  ordinal = 'els female',                        # binary and ordinal variables
  latent = 'inflamlat',                          # define latent variables
  center = 'age',                                # center predictors
  model = '
    inflamlat -> inflam_crp inflam_il6 inflam_tnf inflam_ifn; # measurement model with loading = 1
    els female age inflamlat ~~ els female age inflamlat;',  # correlations among predictors
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

# print output
output(mod1)                                     # print output
standardized(mod1)                               # print standardized estimates in one table

# factor mean and variance fixed at 0 and 1
mod2 <- rblimp(
  data = inflamm,                                # R data frame
  ordinal = 'els female',                        # binary and ordinal variables
  latent = 'inflamlat',                          # define latent variables
  center = 'age',                                # center predictors
  model = '
    inflamlat@1;                                 # fix latent variable variance to 1
    inflamlat -> inflam_crp@lo1 inflam_il6 inflam_tnf inflam_ifn; # measurement model with free loadings
    els female age inflamlat ~~ els female age inflamlat;',  # correlations among predictors
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

# print output
output(mod2)                                     # print output
standardized(mod2)                               # print standardized estimates in one table

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# factor mean and variance fixed at 0 and 1
mod3 <- rblimp(
  data = inflamm,                                # R data frame
  ordinal = 'els female',                        # binary and ordinal variables
  latent = 'inflamlat',                          # define latent variables
  center = 'age',                                # center predictors
  model = '
    inflamlat@1;                                 # fix latent variable variance to 1
    inflamlat -> inflam_crp@lo1 inflam_il6 inflam_tnf inflam_ifn; # measurement model with free loadings
    els female age inflamlat ~~ els female age inflamlat;',  # correlations among predictors
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod3)                                     # print output
standardized(mod3)                               # print standardized estimates in one table
distribution_plot(mod3)                          # plot observed and imputed distributions
residuals_plot(mod3)                             # plot residuals

# factor mean and variance fixed at 0 and 1
mod4 <- rblimp(
  data = inflamm,                                # R data frame
  ordinal = 'els female',                        # binary and ordinal variables
  latent = 'inflamlat',                          # define latent variables
  center = 'age',                                # center predictors
  model = '
    inflamlat@1;                                 # fix latent variable variance to 1
    inflamlat -> yjt(inflam_crp)@lo1 yjt(inflam_il6) inflam_tnf yjt(inflam_ifn); # measurement model with free loadings
    els female age inflamlat ~~ els female age inflamlat;',  # correlations among predictors
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod4)                                     # print output
standardized(mod4)                               # print standardized estimates in one table
distribution_plot(mod4)                          # plot observed and imputed distributions
residuals_plot(mod4)                             # plot residuals


dp <- distribution_plot(mod3) 
rp <- residuals_plot(mod3)

pdf("~/desktop/distributions_mod3.pdf", width = 8, height = 6)   # one plot per page
for (d in dp) print(d)
dev.off()

pdf("~/desktop/residuals_mod3.pdf", width = 8, height = 6)   # one plot per page
for (p in rp) print(p)
dev.off()

dp <- distribution_plot(mod4) 
rp <- residuals_plot(mod4)

pdf("~/desktop/distributions_mod4.pdf", width = 8, height = 6)   # one plot per page
for (d in dp) print(d)
dev.off()

pdf("~/desktop/residuals_mod4.pdf", width = 8, height = 6)   # one plot per page
for (p in rp) print(p)
dev.off()
