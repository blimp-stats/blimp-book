# BRIAN NOTES ----
# options = hev gives VERY different estimates of the condition effect with latent variable specification
# the two approaches are effectively equivalent without options = hev

# RANDOM INTERCEPT REGRESSION WITH HETEROGENEOUS WITHIN-CLUSTER VARIATION

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/problemsolving2level.csv'

# create data frame from github data
probsolve <- read.csv(data_url)

# FIT MODEL WITH COMBINED MODEL SPECIFICATION ----

# mixed model specification with heterogeneous within-cluster variation
model <- rblimp(
    data = probsolve,
    clusterid = 'school',
    nominal = 'condition frlunch',
    center = 'groupmean = probsolvpre stanmath frlunch',
    # fixed = 'probsolve1 condition',
    model = 'probsolvpost ~ intercept@mu0 probsolvpre stanmath frlunch probsolvpre.mean stanmath.mean frlunch.mean condition@diff', 
    parameters = 'd = diff / sqrt(probsolvpost.totalvar);',
    seed = 90291,
    burn = 10000,
    iter = 10000,
    options = 'hev',
    nimps = 20)

# print output
output(model)

# plot parameter distributions
posterior_plot(model,'probsolvpost')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot distributions, observed vs. imputed scores, and residuals
distribution_plot(model)
imputed_vs_observed_plot(model)
residuals_plot(model)
