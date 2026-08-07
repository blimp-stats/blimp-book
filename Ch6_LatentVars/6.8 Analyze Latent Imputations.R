# ANALYZE MNLFA-GENERATED IMPUTATIONS

# plotting functions
# source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source("/Users/craig/Dropbox/Claude/Projects/Blimp Book/rblimp_cleaned_functions.R")

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(ggplot2)
library(mitml)
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
# FINAL MNLFA ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
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

output(mod1)                                     # print output
names(mod1)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

distribution_plot(mod1)                          # plot observed and imputed distributions
residuals_plot(mod1)                             # plot residuals

# save distribution plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/6.9 Distribution Plot.pdf", width = 8.5, height = 11)
plots <- distribution_plot(mod1)                 # plot observed and imputed distributions
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

# save residual plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/6.9 Residuals Plot.pdf", width = 8.5, height = 11)
plots <- residuals_plot(mod1)                    # plot residuals
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

#------------------------------------------------------------------------------#
# FIT MODEL TO MULTIPLY IMPUTED LATENT SCORES ----
#------------------------------------------------------------------------------#

# mitml list
implist <- as.mitml(mod1)

# pooled grand means
mean_alphaeeg <- mean(sapply(implist, function(d) mean(d$alphaeeg)))
mean_age      <- mean(sapply(implist, function(d) mean(d$age)))

# analysis and pooling with mitml
est_mi <- with(implist, lm(attention.latent ~ I(alphaeeg - mean_alphaeeg) + I((alphaeeg - mean_alphaeeg)^2)
                           + I(age - mean_age) + I((age - mean_age)^2) + male))
testEstimates(est_mi, extra.pars = T, df.com = 939 - 6)
