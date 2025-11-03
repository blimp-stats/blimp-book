# RANDOM INTERCEPT CROSS-LAGGED PANEL MODEL

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/loneliness.csv'

# create data frame from github data
lonely <- read.csv(data_url)