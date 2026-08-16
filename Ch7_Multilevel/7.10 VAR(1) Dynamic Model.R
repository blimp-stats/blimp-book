# VAR(1) DSEM WITH CARRYOVER AND SPILLOVER

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
  data = diary,                                  # R data frame
  clusterid = 'person',                          # cluster-level identifier
  model = 'intercept -> posaff pain',
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod0)                                     # print output

#------------------------------------------------------------------------------#
# VAR(1) MODEL ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = diary,                                  # R data frame
  clusterid = 'person',                          # cluster-level identifier
  timeid = 'day',                                # occasion-level identifer
  latent = 'person = b0c b1c b2c a0c a1c a2c',   # define latent variables
  model = '
    level2:                                      # model block label
    intercept -> b0c b1c b2c@b2_mean a0c a1c a2c@a2_mean;  # latent variable means
    b0c b1c b2c a0c a1c a2c ~~ b0c b1c b2c a0c a1c a2c;  # random effect correlations
    level1:                                      # model block label
    lag_posaff = posaff.lag - b0c;               # definition variable
    lag_pain = pain.lag - a0c;                   # definition variable
    posaff ~ intercept@b0c lag_posaff@b1c lag_pain@b2c;  # level-1 model
    pain ~ intercept@a0c lag_pain@a1c lag_posaff@a2c;  # level-1 model
    posaff ~~ pain;',                            # level-1 residual correlation
  parameters = '
    sdw_pain = sqrt(pain.totalvar);              # within-person SD
    sdw_posaff = sqrt(posaff.totalvar);          # within-person SD
    b2_std = b2_mean * sdw_pain / sdw_posaff;    # pain -> posaff spillover
    a2_std = a2_mean * sdw_posaff / sdw_pain;',  # posaff -> pain spillover
  seed = 90291,                                  # random number seed
  burn = 20000,                                  # warm-up iterations
  iter = 20000                                   # analysis iterations
)

output(mod1)                                     # print output
posterior_plot(mod1)                             # plot parameter distributions

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = diary,                                  # R data frame
  clusterid = 'person',                          # cluster-level identifier
  timeid = 'day',                                # occasion-level identifer
  latent = 'person = b0c b1c b2c a0c',           # define latent variables
  model = '
    level2:                                      # model block label
    intercept -> b0c b1c b2c a0c;                # latent variable means
    b0c b1c b2c a0c ~~ b0c b1c b2c a0c;          # random effect correlations
    level1:                                      # model block label
    lag_posaff = posaff.lag - b0c;               # definition variable
    lag_pain = pain.lag - a0c;                   # definition variable
    posaff ~ intercept@b0c lag_posaff@b1c lag_pain@b2c;  # level-1 model
    pain ~ intercept@a0c;',                      # level-1 model
  seed = 90291,                                  # random number seed
  burn = 20000,                                  # warm-up iterations
  iter = 20000,                                  # analysis iterations
  nimps = 20                                     # save 20 imputed data sets
)

output(mod2)                                     # print output

distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot residuals

# save distribution plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/7.10 Distribution Plot.pdf", width = 8.5, height = 11)
plots <- distribution_plot(mod2)                 # plot observed and imputed distributions
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

# save residual plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/7.10 Residuals Plot.pdf", width = 8.5, height = 11)
plots <- residuals_plot(mod2)                    # plot residuals
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

#------------------------------------------------------------------------------#
# VAR(1) MODEL WITH RANDOM VARIANCES ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = diary,                                  # R data frame
  clusterid = 'person',                          # cluster-level identifier
  timeid = 'day',                                # occasion-level identifer
  latent = 'person = b0c b1c b2c lnvar_paff a0c a1c a2c lnvar_pain',  # define latent variables
  model = '
    level2:                                      # model block label
    intercept -> b0c b1c b2c lnvar_paff a0c a1c a2c lnvar_pain;  # latent variable means
    b0c b1c b2c lnvar_paff a0c a1c a2c lnvar_pain ~~ b0c b1c b2c lnvar_paff a0c a1c a2c lnvar_pain;  # random effect correlations
    level1:                                      # model block label
    lag_posaff = posaff.lag - b0c;               # definition variable
    lag_pain = pain.lag - a0c;                   # definition variable
    posaff ~ intercept@b0c lag_posaff@b1c lag_pain@b2c;  # level-1 model
    var(posaff) ~ intercept@lnvar_paff;          # level-1 model
    pain ~ intercept@a0c lag_pain@a1c lag_posaff@a2c;  # level-1 model
    var(pain) ~ intercept@lnvar_pain;            # level-1 model
    posaff ~~ pain;',                            # level-1 residual correlation
  seed = 90291,                                  # random number seed
  burn = 20000,                                  # warm-up iterations
  iter = 20000                                   # analysis iterations
)

output(mod3)                                     # print output

#------------------------------------------------------------------------------#
# VAR(1) MODEL WITH CROSS-LEVEL INTERACTIONS ----
#------------------------------------------------------------------------------#

mod4 <- rblimp(
  data = diary,                                  # R data frame
  clusterid = 'person',                          # cluster-level identifier
  timeid = 'day',                                # occasion-level identifer
  latent = 'person = b0c b1c b2c a0c a1c a2c',   # define latent variables
  model = '
    level2:                                      # model block label
    intercept female -> b0c b1c b2c a0c a1c a2c; # latent variable means
    b0c b1c b2c a0c a1c a2c ~~ b0c b1c b2c a0c a1c a2c;  # random effect correlations
    level1:                                      # model block label
    lag_posaff = posaff.lag - b0c;               # definition variable
    lag_pain = pain.lag - a0c;                   # definition variable
    posaff ~ intercept@b0c lag_posaff@b1c lag_pain@b2c;  # level-1 model
    pain ~ intercept@a0c lag_pain@a1c lag_posaff@a2c;  # level-1 model
    posaff ~~ pain;',                            # level-1 residual correlation
  seed = 90291,                                  # random number seed
  burn = 20000,                                  # warm-up iterations
  iter = 20000                                   # analysis iterations
)

output(mod4)                                     # print output
posterior_plot(mod4)                             # plot parameter distributions

#------------------------------------------------------------------------------#
# VAR(1) MODEL WITH WITHIN-CLUSTER INTERACTIONS ----
#------------------------------------------------------------------------------#

mod5 <- rblimp(
  data = diary,                                  # R data frame
  clusterid = 'person',                          # cluster-level identifier
  timeid = 'day',                                # occasion-level identifer
  latent = 'person = b0c b1c b2c a0c a1c a2c',   # define latent variables
  model = '
    level2:                                      # model block label
    intercept -> b0c b1c b2c a0c a1c a2c;        # latent variable means
    b0c b1c b2c a0c a1c a2c ~~ b0c b1c b2c a0c a1c a2c;  # random effect correlations
    level1:                                      # model block label
    lag_posaff = posaff.lag - b0c;               # definition variable
    lag_pain = pain.lag - a0c;                   # definition variable
    posaff ~ intercept@b0c lag_posaff@b1c lag_pain@b2c lag_posaff*lag_pain;  # level-1 model
    pain ~ intercept@a0c lag_pain@a1c lag_posaff@a2c lag_pain*lag_posaff;  # level-1 model
    posaff ~~ pain;',                            # level-1 residual correlation
  seed = 90291,                                  # random number seed
  burn = 20000,                                  # warm-up iterations
  iter = 20000                                   # analysis iterations
)

output(mod5)                                     # print output
posterior_plot(mod5)                             # plot parameter distributions


