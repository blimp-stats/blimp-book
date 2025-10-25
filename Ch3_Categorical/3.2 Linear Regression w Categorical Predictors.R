# LINEAR REGRESSION WITH CATEGORICAL PREDICTORS ----

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/smoking.csv'

# create data frame from github data
smoking <- read.csv(data_url)

# FIT MODEL ----

model <- rblimp(
    data = smoking,
    ordinal = 'parsmoke',
    nominal = 'educ',
    center = 'age',
    model = 'intensity ~ parsmoke educ age', 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(model)
