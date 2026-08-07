# MEDIATION WITH DISCRETE MEDIATORS

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/alcoholuse.csv'

# create data frame from github data
alcoholuse <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT MODEL WITH LATENT RESPONSE MEDIATOR ----
#------------------------------------------------------------------------------#

# binary probit (latent response) model for mediator
mod1 <- rblimp(
  data = alcoholuse,                             # R data frame
  nominal = 'college male',                      # nominal variables (auto dummy coded)
  ordinal = 'alcearly',                          # binary and ordinal variables
  center  = 'college age',                       # center predictors
  # fixed = 'male age',
  model = '
    apath:
    alcearly ~ intercept@m_icept male@a college age;
    bpath:
    alcdays ~ intercept@y_icept alcearly.latent@b male@tau college age',
  parameters = 'indirect = a*b',
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod1)                                     # print output

posterior_plot(mod1)                             # plot parameter distributions

posterior_plot(mod1, 'indirect')                 # plot distribution of indirect effect

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

distribution_plot(mod1)                          # plot observed and imputed distributions
residuals_plot(mod1)                             # plot residuals

# save distribution plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/5.9 Distribution Plot mod1.pdf", width = 8.5, height = 11)
plots <- distribution_plot(mod1)                 # plot observed and imputed distributions
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

# save residual plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/5.9 Residuals Plot mod1.pdf", width = 8.5, height = 11)
plots <- residuals_plot(mod1)                    # plot residuals
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

#------------------------------------------------------------------------------#
# FIT MODEL WITH CONDITIONAL INDIRECT EFFECTS ----
#------------------------------------------------------------------------------#

# binary logistic model for mediator
mod2 <- rblimp(
  data = alcoholuse,                             # R data frame
  nominal = 'college male alcearly',             # nominal variables (auto dummy coded)
  center  = 'college age',                       # center predictors
  # fixed = 'male age',
  model = '
    apath:
    alcearly ~ intercept@m_icept male@a college age;
    bpath:
    alcdays ~ intercept@y_icept alcearly@b male@tau college age',
  parameters = '
    ind_female = a * b * exp( m_icept + a*0 ) /
       ( 1 + exp( m_icept + a*0 ) )^2;
    ind_male = a * b * exp( m_icept + a*1 ) /
     ( 1 + exp( m_icept + a*1 ) )^2;
    ind_diff   = ind_male - ind_female;',
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod2)                                     # print output

posterior_plot(mod2)                             # plot parameter distributions

posterior_plot(mod2, 'ind_male')                 # plot distribution of indirect effect
posterior_plot(mod2, 'ind_female')               # plot distribution of indirect effect
posterior_plot(mod2, 'ind_diff')                 # plot distribution of indirect effect

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot residuals

# save distribution plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/5.9 Distribution Plot mod2.pdf", width = 8.5, height = 11)
plots <- distribution_plot(mod2)                 # plot observed and imputed distributions
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

# save residual plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/5.9 Residuals Plot mod2.pdf", width = 8.5, height = 11)
plots <- residuals_plot(mod2)                    # plot residuals
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

