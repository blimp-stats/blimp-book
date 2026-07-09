# ANALYZE LATENT IMPUTATIONS

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(mitml)

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# read data from directory that contains this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
impdata <- read.csv("imps.csv")

#------------------------------------------------------------------------------#
# FIT MODEL TO MULTIPLY IMPUTED LATENT SCORES ----
#------------------------------------------------------------------------------#

# mitml list
implist <- split(impdata, impdata$imp)
implist <- as.mitml.list(implist)


# pooled grand means
mean_alphaeeg <- mean(sapply(implist, function(d) mean(d$alphaeeg)))
mean_age      <- mean(sapply(implist, function(d) mean(d$age)))

# analysis and pooling with mitml
est_mi <- with(implist, lm(attention.latent ~ I(alphaeeg - mean_alphaeeg) + I((alphaeeg - mean_alphaeeg)^2) 
                           + I(age - mean_age) + I((age - mean_age)^2) + male))
testEstimates(est_mi, extra.pars = T, df.com = 939 - 6)