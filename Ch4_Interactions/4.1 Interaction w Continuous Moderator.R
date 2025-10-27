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

reading$lrnprob1_grp <- with(reading, ifelse(
  is.na(lrnprob1), NA,
  ifelse(lrnprob1 >= 63, 3,
         ifelse(lrnprob1 >= 55 & lrnprob1 <= 62, 2,
                ifelse(lrnprob1 <= 54, 1, NA)
         )
  )
))


# SUMMARIZE DATA ----

dfSummary(reading)
describe(reading)

# FIT MODEL ----

# linear regression with continuous moderator
model <- rblimp(
  data = reading, 
  nominal = 'atrisk',
  center = 'read1 lrnprob1',
  model = 'read9 ~ read1 lrnprob1 atrisk read1*lrnprob1',  
  simple = 'read1 | lrnprob1',      
  seed = 90291,                                              
  burn = 10000,                                              
  iter = 10000)                                                

# summarize results
output(model)

# plot conditional effects and johnson-neyman regions of significance
simple_plot(read9 ~ read1 | lrnprob1, model)
jn_plot(read9 ~ read1 | lrnprob1, model)

