# MODERATED NON-LINEAR FACTOR ANALYSIS

# plotting functions
# source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source("/Users/craig/Documents/Claude/Projects/Blimp Book/rblimp_cleaned_functions.R")

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
  data    = adhd,
  ordinal = 'study male adhd attent1:attent9',
  latent  = 'attention',
  center  = 'alphaeeg age',
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
  seed = 90291, 
  burn = 100000, 
  iter = 100000,
  nimps = 20)

output(mod1)
names(mod1)

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