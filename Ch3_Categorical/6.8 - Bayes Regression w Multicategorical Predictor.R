# EXAMPLE 6.8 - Bayes Regression w Multicategorical Predictor

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/smoking.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = smoking,
    ordinal = 'parsmoke',
    nominal = 'educ',
    fixed = 'age',
    center = 'age',
    model = 'intensity ~ parsmoke educ age', # automatic multivariate distribution for incomplete predictors and latent response scores 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis)
