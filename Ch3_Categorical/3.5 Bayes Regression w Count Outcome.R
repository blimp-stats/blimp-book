# NEGATIVE BINOMIAL REGRESSION FOR A COUNT OUTCOME ----

# LOAD R PACKAGES ----

library(rblimp)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/alcoholuse.csv'

# create data frame from github data
alcoholuse <- read.csv(data_url)

# FIT MODEL ----

# negative binomial regression
model <- rblimp(
    data = alcoholuse,
    nominal = 'college male',
    count = 'alcdays',
    # fixed = 'male',
    model = 'alcdays ~ alcage college age male', 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(model)
