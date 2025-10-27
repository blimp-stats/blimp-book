# INTERACTION INVOLVING A CONTINUOUS MODERATOR

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/reading.csv'

# SUMMARIZE DATA ----

dfSummary(reading)
describe(reading)

# FIT MODEL ----

# linear regression with continuous moderator
model <- rblimp(
  data = reading, 
  nominal = 'male hispanic',
  center = 'read1 lrnprob1',
  model = 'read9 ~ read1 lrnprob1 read1*lrnprob1 male hispanic',  
  simple = 'read1 | lrnprob1',      
  seed = 90291,                                              
  burn = 10000,                                              
  iter = 10000)                                                

# summarize results
output(model)

# plot conditional effects and johnson-neyman regions of significance
simple_plot(read9 ~ read1 | lrnprob1, model)
jn_plot(read9 ~ read1 | lrnprob1, model)

