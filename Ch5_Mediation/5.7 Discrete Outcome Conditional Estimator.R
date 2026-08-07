# MEDIATION WITH BINARY OUTCOME USING CONDITIONAL ESTIMATOR

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
# BINARY OUTCOME AND CONDITIONAL INDIRECT EFFECTS ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = worksat,                                # R data frame
  ordinal = 'turnover male relqual',             # binary and ordinal variables
  center  = 'male',                              # center predictors
  model = '
    jobsat ~ intercept@a0 relqual@a1 male;
    turnover ~ intercept@b0 jobsat@b1 relqual@b2 male;',
  parameters = '
    p0 = phi(b0 + b1*(a0 + a1*0) + b2*0);
    p1 = phi(b0 + b1*(a0 + a1*1) + b2*1);
    rate0 = normal_pdf(b0 + b1*(a0 + a1*0) + b2*0);
    rate1 = normal_pdf(b0 + b1*(a0 + a1*1) + b2*1);
    indirect0 = a1 * b1 * rate0;                 # conditional indirect effect at x = 0
    indirect1 = a1 * b1 * rate1;',               # conditional indirect effect at x = 1
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod1)                                     # print output

#------------------------------------------------------------------------------#
# EXTENSION: CONTINUOUS EXPLANATORY VARIABLE ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = worksat,                                # R data frame
  ordinal = 'turnover male',                     # binary and ordinal variables
  center  = 'male',                              # center predictors
  model = '
    jobsat ~ intercept@a0 lmx@a1 male;
    turnover ~ intercept@b0 jobsat@b1 lmx@b2 male;',
  simple = 'a1 * b1 * normal_pdf(b0 + b1*(a0 + a1*lmx) + b2*lmx) | lmx @ quartiles;',
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod2)                                     # print output

#------------------------------------------------------------------------------#
# EXTENSION: LATENT RESPONSE MEDIATOR ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = worksat,                                # R data frame
  ordinal = 'turnover male relqual jobsat',      # binary and ordinal variables
  center  = 'male',                              # center predictors
  model = '
    jobsat ~ intercept@a0 relqual@a1 male;
    turnover ~ intercept@b0 jobsat.latent@b1 relqual@b2 male;',
  parameters = '
    p0 = phi(b0 + b1*(a0 + a1*0) + b2*0);
    p1 = phi(b0 + b1*(a0 + a1*1) + b2*1);
    w0 = normal_pdf(b0 + b1*(a0 + a1*0) + b2*0);
    w1 = normal_pdf(b0 + b1*(a0 + a1*1) + b2*1);
    indirect0 = a1 * b1 * w0;                    # conditional indirect effect at x = 0
    indirect1 = a1 * b1 * w1;',                  # conditional indirect effect at x = 1
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod3)                                     # print output

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod4 <- rblimp(
  data = worksat,                                # R data frame
  ordinal = 'turnover male relqual',             # binary and ordinal variables
  center  = 'male',                              # center predictors
  model = '
    jobsat ~ intercept@a0 relqual@a1 male;
    turnover ~ intercept@b0 jobsat@b1 relqual@b2 male;',
  parameters = '
    p0 = phi(b0 + b1*(a0 + a1*0) + b2*0);
    p1 = phi(b0 + b1*(a0 + a1*1) + b2*1);
    rate0 = normal_pdf(b0 + b1*(a0 + a1*0) + b2*0);
    rate1 = normal_pdf(b0 + b1*(a0 + a1*1) + b2*1);
    indirect0 = a1 * b1 * rate0;                 # conditional indirect effect at x = 0
    indirect1 = a1 * b1 * rate1;',               # conditional indirect effect at x = 1
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod4)                                     # print output

distribution_plot(mod4)                          # plot observed and imputed distributions
residuals_plot(mod4)                             # plot binned residuals

# save distribution plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/5.7 Distribution Plot.pdf", width = 8.5, height = 11)
plots <- distribution_plot(mod4)                 # plot observed and imputed distributions
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

# save residual plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/5.7 Residuals Plot.pdf", width = 8.5, height = 11)
plots <- residuals_plot(mod4)                    # plot binned residuals
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

