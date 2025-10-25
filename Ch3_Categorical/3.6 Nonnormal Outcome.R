# EXAMPLE 10.4 - Bayes Yeo-Johnson Transformation for a Nonnormal Outcome

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/smoking.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = smoking,
    ordinal = 'parsmoke',
    fixed = 'age',
    center = 'income age',
    model = 'yjt(intensity - 9) ~ parsmoke income age', # automatic multivariate normal distribution for incomplete predictors and latent response scores 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis)
