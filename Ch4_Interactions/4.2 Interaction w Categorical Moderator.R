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

# SUMMARIZE DATA ----

dfSummary(reading)
describe(reading)

# FIT MODEL WITH BINARY MODERATOR ----

# linear regression with a binary moderator
model1 <- rblimp(
  data = reading, 
  ordinal = 'lrnprobatrisk1',
  nominal = 'male hispanic',
  center = 'read1',
  model = 'read9 ~ read1 lrnprobatrisk1 read1*lrnprobatrisk1 male hispanic',  
  simple = 'read1 | lrnprobatrisk1',      
  seed = 90291,                                              
  burn = 10000,                                              
  iter = 10000)                                                

# summarize results
output(model1)

# plot conditional effects
simple_plot(read9 ~ read1 | lrnprobatrisk1, model1)

# FIT MODEL WITH MULTICATEGORICAL MODERATOR ----

# linear regression with multicategorical moderator
model2 <- rblimp(
  data = reading, 
  nominal = 'lrnprobgrp1 male hispanic',
  center = 'read1',
  model = 'read9 ~ read1 lrnprobgrp1 read1*lrnprobgrp1 male hispanic',  
  waldtest = 'read9 ~ read1 lrnprobgrp1 male hispanic',
  simple = 'read1 | lrnprobgrp1',      
  seed = 90291,                                              
  burn = 10000,                                              
  iter = 10000)                                                

# summarize results
output(model2)

# plot conditional effects
simple_plot(read9 ~ read1 | lrnprobgrp1.2 + lrnprobgrp1.3, model2)

# NOTE: SIMPLE COMMAND DOES NOT WORK WITH . SUFFIX CONVENTION ----

# linear regression with continuous moderator
model3 <- rblimp(
  data = reading, 
  nominal = 'lrnprobgrp1 male hispanic',
  center = 'read1',
  model = 'read9 ~ intercept@b0 read1@b1 lrnprobgrp1.2@b2 lrnprobgrp1.3@b3 read1*lrnprobgrp1.2@b4 read1*lrnprobgrp1.3@b5 male hispanic',  
  waldtest = 'b4:b5 = 0',
  parameters = '
    icept_g1 = b0;
    readslp_g1 = b1;
    icept_g2 = b0 + b2;
    readslp_g2 = b1 + b4;
    icept_g3 = b0 + b3;
    readslp_g3 = b1 + b5',
  seed = 90291,                                              
  burn = 10000,                                              
  iter = 10000)                                                

# summarize results
output(model3)

