# BRIAN NOTES ----
# simple command currently does not work with explicit dummy codes (needed for wald test)

# INTERACTION INVOLVING A CATEGORICAL MODERATOR

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/reading.csv'

# create data frame from github data
reading <- read.csv(data_url)

# FIT MODEL WITH A BINARY MODERATOR ----

# linear regression with a binary moderator
model1 <- rblimp(
  data = reading, 
  nominal = 'male hispanic lrnprob1atrisk',
  center = 'read1',
  model = 'read9 ~ read1 lrnprob1atrisk read1*lrnprob1atrisk male hispanic',  
  simple = 'read1 | lrnprob1atrisk',      
  seed = 90291,                                              
  burn = 10000,                                              
  iter = 10000)                                                

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1,'read9')

# plot conditional effects
simple_plot(read9 ~ read1 | lrnprob1atrisk.1, model1)

# FIT MODEL WITH A MULTICATEGORICAL MODERATOR ----

# moderated regression with explicit dummy codes and interaction test
model2 <- rblimp(
  data = reading, 
  nominal = 'lrnprob1risk male hispanic',
  center = 'read1',
  model = 'read9 ~ read1 lrnprob1risk.2 lrnprob1risk.3 read1*lrnprob1risk.2@slpdiff2 read1*lrnprob1risk.3@@slpdiff3 male hispanic',  
  waldtest = 'slpdiff2:slpdiff3 = 0',
  seed = 90291,                                              
  burn = 10000,                                              
  iter = 10000)                                                

# print output
output(model2)

# linear regression with multicategorical moderator
model3 <- rblimp(
  data = reading, 
  nominal = 'lrnprob1risk male hispanic',
  center = 'read1',
  model = 'read9 ~ read1 lrnprob1risk read1*lrnprob1risk male hispanic',  
  waldtest = 'read9 ~ read1 lrnprob1risk male hispanic',
  simple = 'read1 | lrnprob1risk',      
  seed = 90291,                                              
  burn = 10000,                                              
  iter = 10000)                                                

# print output
output(model3)

# plot conditional effects
simple_plot(read9 ~ read1 | lrnprob1risk.2 + lrnprob1risk.3, model3)



