# INTERACTION INVOLVING A CATEGORICAL MODERATOR

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/math.csv'

# create data frame from github data
math <- read.csv(data_url)

# FIT MODEL ----

# curvilinear regression
model <- rblimp(
    data = math,
    ordinal = 'frlunch male',
    fixed = 'male mathpre',
    center = 'anxiety',
    model = 'mathpost ~ anxiety anxiety^2 frlunch mathpre male',
    seed = 12345,
    burn = 10000,
    iter = 10000)

output(model)

# ADD SOME DISPLAY TO PLOT CURVILINEAR EFFECT ----
