# MODERATED NON-LINEAR FACTOR ANALYSIS

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/adhd.csv'

# create data frame from github data
adhd <- read.csv(data_url)

#------------------------------------------------------------------------------#
# CFA ----
#------------------------------------------------------------------------------#

# one-factor cfa
mod1 <- rblimp(
  data = adhd,                                   # R data frame
  ordinal = 'attent1:attent9',                   # binary and ordinal variables
  latent = 'attention',                          # define latent variables
  model = '
    attention@1;
    attention -> attent1@load1 attent2:attent9',
  seed = 90291,                                  # random number seed
  burn = 20000,                                  # warm-up iterations
  iter = 20000)                                  # analysis iterations

output(mod1)                                     # print output
standardized(mod1)

# cfa with correlated residuals
mod2 <- rblimp(
  data = adhd,                                   # R data frame
  ordinal = 'attent1:attent9',                   # binary and ordinal variables
  latent = 'attention',                          # define latent variables
  model = '
    attention@1;
    attention -> attent1@load1 attent2:attent9;
    attent2 ~~ attent8;
    attent7 ~~ attent9;',
  seed = 90291,                                  # random number seed
  burn = 20000,                                  # warm-up iterations
  iter = 20000)                                  # analysis iterations

output(mod2)                                     # print output
standardized(mod2)

#------------------------------------------------------------------------------#
# MODERATORS PREDICTING FACTOR MEAN AND VARIANCE ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = adhd,                                   # R data frame
  ordinal = 'study male adhd attent1:attent9',   # binary and ordinal variables
  latent = 'attention',                          # define latent variables
  center = 'alphaeeg age',                       # center predictors
  model = '
    structural:
    attention ~ alphaeeg alphaeeg^2 age age^2 study male adhd;
    var(attention) ~ 1@0 alphaeeg age study male adhd;
    measurement:
    attention -> attent1@load1 attent2:attent9;
    attent2 ~~ attent8;
    attent7 ~~ attent9;',
  seed = 90291,                                  # random number seed
  burn = 50000,                                  # warm-up iterations
  iter = 50000)                                  # analysis iterations

output(mod3)                                     # print output
standardized(mod3)

#------------------------------------------------------------------------------#
# MODERATORS PREDICTING MEASUREMENT MODEL PARAMETERS ----
#------------------------------------------------------------------------------#

mod4 <- rblimp(
  data = adhd,                                   # R data frame
  ordinal = 'study male adhd attent1:attent9',   # binary and ordinal variables
  latent = 'attention',                          # define latent variables
  center = 'alphaeeg age',                       # center predictors
  model = '
    structural:
    attention ~ alphaeeg alphaeeg^2 age age^2 study male adhd;
    var(attention) ~ 1@0 alphaeeg age study male adhd;
    measurement:
    attention -> attent1@load1 attent2:attent9;
    attent2 ~~ attent8;
    attent7 ~~ attent9;
    # moderation effects
    age study male adhd -> attent1:attent3 attent5 attent7:attent9;
    age*attention study*attention male*attention adhd*attention -> attent1:attent3 attent5 attent7:attent9;',
  seed = 90291,                                  # random number seed
  burn = 100000,                                 # warm-up iterations
  iter = 100000)                                 # analysis iterations

output(mod4)                                     # print output
standardized(mod4)

#------------------------------------------------------------------------------#
# TRIMMED LOADINGS ----
#------------------------------------------------------------------------------#

mod5 <- rblimp(
  data    = adhd,                                # R data frame
  ordinal = 'study male adhd attent1:attent9',   # binary and ordinal variables
  latent  = 'attention',                         # define latent variables
  center  = 'alphaeeg age',                      # center predictors
  model = '
  structural:
  attention ~ alphaeeg alphaeeg^2 age age^2 study male adhd;
  var(attention) ~ 1@0 alphaeeg age study male adhd;
  measurement:
  attention -> attent1@load1 attent2:attent9;
  attent2 ~~ attent8;
  attent7 ~~ attent9;
  age study male adhd ->
    attent1:attent3 attent5 attent7:attent9;     # all intercepts
  study*attention ->
    attent2:attent3 attent7 attent8:attent9;     # retained interactions
  age*attention male*attention -> attent7;',     # retained interactions
  seed = 90291,                                  # random number seed
  burn = 100000,                                 # warm-up iterations
  iter = 100000)                                 # analysis iterations

output(mod5)                                     # print output
summary(mod5@estimates[,7])
mod5@estimates

#------------------------------------------------------------------------------#
# TRIMMED LOADINGS AND INTERCEPTS ----
#------------------------------------------------------------------------------#

mod6 <- rblimp(
  data    = adhd,                                # R data frame
  ordinal = 'study male adhd attent1:attent9',   # binary and ordinal variables
  latent  = 'attention',                         # define latent variables
  center  = 'alphaeeg age',                      # center predictors
  model = '
    structural:
    attention ~ alphaeeg alphaeeg^2 age age^2 study male adhd;
    var(attention) ~ 1@0 alphaeeg age study male adhd;
    measurement:
    attention -> attent1@load1 attent2:attent9;
    attent2 ~~ attent8;
    attent7 ~~ attent9;
    # dif
    study -> attent1;
    adhd -> attent1;
    age -> attent2:attent3 attent8;
    study*attention study -> attent2:attent3 attent7:attent9;
    age*attention male*attention age male -> attent7;',
  seed = 90291,                                  # random number seed
  burn = 100000,                                 # warm-up iterations
  iter = 100000)                                 # analysis iterations

output(mod6)                                     # print output

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod7 <- rblimp(
  data    = adhd,                                # R data frame
  ordinal = 'study male adhd attent1:attent9',   # binary and ordinal variables
  latent  = 'attention',                         # define latent variables
  center  = 'alphaeeg age',                      # center predictors
  model = '
    structural:
    attention ~ alphaeeg alphaeeg^2 age age^2 study male adhd;
    var(attention) ~ 1@0 alphaeeg age study male adhd;
    measurement:
    attention -> attent1@load1 attent2:attent9;
    attent2 ~~ attent8;
    attent7 ~~ attent9;
    # dif
    study -> attent1;
    adhd -> attent1;
    age -> attent2:attent3 attent8;
    study*attention study -> attent2:attent3 attent7:attent9;
    age*attention male*attention age male -> attent7;',
  seed = 90291,                                  # random number seed
  burn = 100000,                                 # warm-up iterations
  iter = 100000,                                 # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod7)                                     # print output

distribution_plot(mod7)                          # plot observed and imputed distributions
residuals_plot(mod7)                             # plot residuals

# save distribution plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/6.7 Distribution Plot.pdf", width = 8.5, height = 11)
plots <- distribution_plot(mod7)                 # plot observed and imputed distributions
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

# save residual plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/6.7 Residuals Plot.pdf", width = 8.5, height = 11)
plots <- residuals_plot(mod7)                    # plot residuals
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

