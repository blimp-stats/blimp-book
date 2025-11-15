# INTERACTION INVOLVING A CONTINUOUS MODERATOR

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/reading.csv'

# create data frame from github data
reading <- read.csv(data_url)

# plotting function
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# FIT ADDITIVE MODEL ----

# linear regression with additive terms only
model1 <- rblimp(
  data = reading, 
  nominal = 'male hispanic',
  center = 'read1 lrnprob1',
  model = 'read9 ~ read1 lrnprob1 male hispanic',  
  seed = 90291,                                              
  burn = 10000,                                              
  iter = 10000,
  nimps = 20)                                                

# print output
output(model1)

# GRAPHICAL DIAGNOSTICS ----

# plot distributions, observed vs. imputed scores, and residuals
imputation_plot(model1)
imputed_vs_observed_plot(model1)
residuals_plot(model1)

# FIT INTERACTIVE MODEL ----

# linear regression with continuous moderator
model2 <- rblimp(
  data = reading, 
  nominal = 'male hispanic',
  center = 'read1 lrnprob1',
  model = 'read9 ~ read1 lrnprob1 read1*lrnprob1 male hispanic',  
  simple = 'read1 | lrnprob1',      
  seed = 90291,                                              
  burn = 10000,                                              
  iter = 10000)                                                

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2,'read9')

# plot conditional effects and johnson-neyman regions of significance
simple_plot(read9 ~ read1 | lrnprob1, model2)
jn_plot(read9 ~ read1 | lrnprob1, model2)

