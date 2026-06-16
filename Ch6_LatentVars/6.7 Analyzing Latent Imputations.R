 # FIT REGRESSION MODEL TO MULTIPLY IMPUTED LATENT SCORES ----

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

 #------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
 #------------------------------------------------------------------------------#

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
# FIT MODEL WITH MODERATED MEASUREMENT INTERCEPTS AND LOADINGS ----
#------------------------------------------------------------------------------#

# factor mean and conditional variance fixed at 0 and 1
model <- rblimp(
  data = adhd,
  ordinal = 'male adhd study swan1:swan9',
  latent = 'attent',
  # fixed = 'study',
  center = 'alphaEEG age', 
  model = '
    structural:
    attent <- alphaEEG alphaEEG^2 age age^2 male adhd study;
    var(attent) <- 1@0 alphaEEG alphaEEG^2 age age^2 male adhd study;
    invariant:
    attent -> swan4@lo4 swan5 swan6 swan9;
    noninvariant:
    attent alphaEEG alphaEEG^2 age age^2 male adhd study 
    attent*alphaEEG attent*age attent*male attent*adhd attent*study -> swan1@lo1 swan2 swan3 swan7 swan8;
    correlations:
    swan2 ~~ swan8;
    swan7 ~~ swan9;
    predictors:
    yjt(age-10) ~ intercept male adhd study;
    yjt(alphaEEG) ~ intercept male adhd study;
    alphaEEG age male adhd study ~~ alphaEEG age male adhd study;
    ',
  seed = 90291,
  burn = 70000,
  iter = 70000,
  nimps = 20)

#------------------------------------------------------------------------------#
# FIT MODEL TO MULTIPLY IMPUTED LATENT SCORES ----
#------------------------------------------------------------------------------#

# mitml list
implist <- as.mitml(model)

# pooled grand means
mean_alphaEEG <- mean(unlist(lapply(implist, function(data) mean(data$alphaEEG))))
mean_age <- mean(unlist(lapply(implist, function(data) mean(data$age))))

# analysis and pooling with mitml
estimates <- with(implist, lm(attent.latent ~ I(alphaEEG - mean_alphaEEG) + I((alphaEEG - mean_alphaEEG)^2) 
                            + I(age - mean_age) + I((age - mean_age)^2) + male))
testEstimates(estimates, extra.pars = T, df.com = 939 - 6)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot latent scores against predictors
bivariate_plot(x_vars = c('alphaEEG','age','male'), y_vars = 'attent.latent', model = model)

