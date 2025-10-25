# EXAMPLE 6.7 - Bayes Multinomial Logistic Regression

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/pain.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = pain,
    nominal = 'paingrps',
    fixed = 'control male',
    model = 'logit(paingrps) ~ exercise control male', # lowest code is the reference group, 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis)
