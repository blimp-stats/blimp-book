# MODERATED NONLINEAR FACTOR ANALYSIS (MNLFA)

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/adhd.csv'

# create data frame from github data
adhd <- read.csv(data_url)

# FIT MODEL ----