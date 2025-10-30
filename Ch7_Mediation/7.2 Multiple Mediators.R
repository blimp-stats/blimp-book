# MULTIPLE MEDIATOR MODEL

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/discrimination.csv'

# create data frame from github data
discrimination <- read.csv(data_url)

# FIT MULTIPLE MEDIATOR MODEL ----

model1 <- rblimp(
  data = discrimination,
  nominal = 'female',
  model = '
    victim ~ discrim@a1 female age;
    familism ~ discrim@a2 female age;
    victim ~~ familism;
    internalize ~ discrim victim@b1 familism@b2 female age;',
  parameters = '
    ind_via_vic = a1*b1;
    ind_via_fam = a2*b2;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model1)

# plot distribution of indirect effects
posterior_plot(model1, 'ind_via_vic')
posterior_plot(model1, 'ind_via_fam')

# FIT SERIAL MEDIATOR MODEL ----

model2 <- rblimp(
  data = discrimination,
  nominal = 'female',
  model = '
    victim ~ discrim@a1 female age;
    familism ~ discrim@a2 victim@d female age;
    internalize ~ discrim victim@b1 familism@b2 female age;',
  parameters = '
    ind_via_vic_fam = a1*d*b2;
    ind_via_vic = a1*b1;
    ind_via_fam = a2*b2;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model2)

# plot distribution of indirect effects
posterior_plot(model2, 'ind_via_vic_fam')
posterior_plot(model2, 'ind_via_vic')
posterior_plot(model2, 'ind_via_fam')
