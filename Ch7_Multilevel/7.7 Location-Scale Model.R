# MULTILEVEL LATENT VARIABLE MODEL WITH RANDOM SLOPES AND RANDOM VARIATION

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
# LOCATION-SCALE MODEL ----
#------------------------------------------------------------------------------#

# base model
mod1 <- rblimp(
  data = diary,                                  # R data frame
  clusterid = 'person',                          # cluster-level identifier
  latent = 'person = b0c b1c g0c',               # define latent variables
  center = '
    groupmean = pain;                            # center at latent group means
    grandmean = pain.mean;',                     # center at grand means
  model = '
    level2:                                      # model block label
    b0c ~ intercept pain.mean;                   # level-2 random intercept
    b1c ~ intercept;                             # level-2 random slope
    g0c ~ intercept pain.mean;                   # level-2 scale model
    b0c b1c g0c ~~ b0c b1c g0c;                  # random effect correlation
    level1:                                      # model block label
    posaff ~ intercept@b0c pain@b1c;             # level-1 model
    var(posaff) ~ intercept@g0c pain;',          # level-1 scale model
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000                                   # analysis iterations
)

output(mod1)                                     # print output
posterior_plot(mod1,'posaff')                    # plot parameter distributions

# add r-square effect sizes
mod2 <- rblimp(
  data = diary,                                  # R data frame
  clusterid = 'person',                          # cluster-level identifier
  latent = 'person = b0c b1c g0c',               # define latent variables
  center = '
    groupmean = pain;                            # center at latent group means
    grandmean = pain.mean;',                     # center at grand means
  model = '
    level2:                                      # model block label
    b0c ~ intercept pain.mean;                   # level-2 random intercept
    b1c ~ intercept;                             # level-2 random slope
    g0c ~ intercept@logvar pain.mean;            # level-2 scale model
    b0c b1c g0c ~~ b0c b1c g0c;                  # random effect correlation
    level1:                                      # model block label
    posaff ~ intercept@b0c pain@b1c;             # level-1 model
    var(posaff) ~ intercept@g0c pain;',          # level-1 scale model
  parameters = '
    var_total = posaff.totalvar + b0c.totalvar;
    rsq_l2coeff = b0c.coefvar / var_total;
    rsq_l1coeff = posaff.coefvar / var_total;
    rsq_l2resid = b0c.residvar / var_total;
    rsq_l1resid = exp(logvar) / var_total;',
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000                                   # analysis iterations
)

output(mod2)                                     # print output

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = diary,                                  # R data frame
  clusterid = 'person',                          # cluster-level identifier
  latent = 'person = b0c b1c g0c',               # define latent variables
  center = '
    groupmean = pain;                            # center at latent group means
    grandmean = pain.mean;',                     # center at grand means
  model = '
    level2:                                      # model block label
    b0c ~ intercept pain.mean;                   # level-2 random intercept
    b1c ~ intercept;                             # level-2 random slope
    g0c ~ intercept pain.mean;                   # level-2 scale model
    b0c b1c g0c ~~ b0c b1c g0c;                  # random effect correlation
    level1:                                      # model block label
    posaff ~ intercept@b0c pain@b1c;             # level-1 model
    var(posaff) ~ intercept@g0c pain;',          # level-1 scale model
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod3)                                     # print output

distribution_plot(mod3)                          # plot observed and imputed distributions
residuals_plot(mod3)                             # plot residuals

# save distribution plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/7.7 Distribution Plot.pdf", width = 8.5, height = 11)
plots <- distribution_plot(mod3)                 # plot observed and imputed distributions
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

# save residual plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/7.7 Residuals Plot.pdf", width = 8.5, height = 11)
plots <- residuals_plot(mod3)                    # plot residuals
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

