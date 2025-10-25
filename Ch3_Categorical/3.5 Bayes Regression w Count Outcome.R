# EXAMPLE 10.10 - Bayes Regression w Count Outcome

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/alcoholuse.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = alcoholuse,
    ordinal = 'college male',
    count = 'alcdays',
    fixed = 'male',
    model = 'alcdays ~ alcage college age male', 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis)
