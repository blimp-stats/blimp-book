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

# FIT SINGLE MEDIATOR MODEL ----

model1 <- rblimp(
  data = discrimination,
  nominal = 'female',
  model = '
    apath:
    familism ~ discrim@a female age;
    bpath:
    internalize ~ discrim familism@b female age;',
  parameters = '
    indirect = a*b;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model1)

# plot distribution of indirect effect
posterior_plot(model1, 'indirect')

# FIT MODEL WITH MODERATED A PATH ----

# sex as a moderator of the a path
model2 <- rblimp(
  data = discrimination,
  nominal = 'female',
  model = '
    apath:
    familism ~ discrim@a_male female age discrim*female@a_dif;
    bpath:
    internalize ~ discrim familism@b female age;',
  parameters = '
    ind_male = a_male*b;
    ind_female = (a_male + a_dif)*b;
    ind_diff = ind_female - ind_male;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model2)

# plot distribution of indirect effects
posterior_plot(model2, 'ind_male')
posterior_plot(model2, 'ind_female')
posterior_plot(model2, 'ind_diff')

# age as a moderator of the a path
model3 <- rblimp(
  data = discrimination,
  nominal = 'female',
  center = 'age',
  model = '
    apath:
    familism ~ discrim@a_atmean female age discrim*age@a_dif;
    bpath:
    internalize ~ discrim familism@b female age;
    predictor:
    age ~ female;',
  parameters = '
    sd_mod = sqrt(age.totalvar);
    ind_lomod = (a_atmean - a_dif*sd_mod)*b;
    ind_atmean = a_atmean*b;
    ind_himod = (a_atmean + a_dif*sd_mod)*b;
    ind_hivslo = ind_himod - ind_lomod;
  ',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model3)

# plot distribution of indirect effects
posterior_plot(model3, 'ind_lomod')
posterior_plot(model3, 'ind_atmean')
posterior_plot(model3, 'ind_himod')
posterior_plot(model3, 'ind_hivslo')

# FIT MODEL WITH MODERATED B PATH ----

# sex as a moderator of the b path
model4 <- rblimp(
  data = discrimination,
  nominal = 'female',
  model = '
    apath:
    familism ~ discrim@a female age;
    bpath:
    internalize ~ discrim familism@b_male female age familism*female@b_dif;',
  parameters = '
    ind_male = a*b_male;
    ind_female = a*(b_male + b_dif);
    ind_diff = ind_female - ind_male;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model4)

# plot distribution of indirect effects
posterior_plot(model4, 'ind_male')
posterior_plot(model4, 'ind_female')
posterior_plot(model4, 'ind_diff')

# age as a moderator of the a path
model5 <- rblimp(
  data = discrimination,
  nominal = 'female',
  center = 'age',
  model = '
    apath:
    familism ~ discrim@a female age;
    bpath:
    internalize ~ discrim familism@b_atmean female age familism*age@b_dif;
    predictor:
    age ~ female;',
  parameters = '
    sd_mod = sqrt(age.totalvar);
    ind_lomod = a*(b_atmean - b_dif*sd_mod);
    ind_atmean = a*b_atmean;
    ind_himod = a*(b_atmean + b_dif*sd_mod);
    ind_hivslo = ind_himod - ind_lomod;
  ',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model5)

# plot distribution of indirect effects
posterior_plot(model5, 'ind_lomod')
posterior_plot(model5, 'ind_atmean')
posterior_plot(model5, 'ind_himod')
posterior_plot(model5, 'ind_hivslo')

# FIT MODEL WITH MODERATED A AND B PATHs ----

# sex as a moderator of the a and b path
model6 <- rblimp(
  data = discrimination,
  nominal = 'female',
  model = '
    apath:
    familism ~ discrim@a_male female age discrim*female@a_dif;
    bpath:
    internalize ~ discrim familism@b_male female age familism*female@b_dif;',
  parameters = '
    ind_male = a_male*b_male;
    ind_female = (a_male + a_dif)*(b_male + b_dif);
    ind_diff = ind_female - ind_male;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model6)

# plot distribution of indirect effects
posterior_plot(model6, 'ind_male')
posterior_plot(model6, 'ind_female')
posterior_plot(model6, 'ind_diff')

# age as a moderator of the a path
model7 <- rblimp(
  data = discrimination,
  nominal = 'female',
  center = 'age',
  model = '
    apath:
    familism ~ discrim@a_atmean female age discrim*age@a_dif;
    bpath:
    internalize ~ discrim familism@b_atmean female age familism*age@b_dif;
    predictor:
    age ~ female;',
  parameters = '
    sd_mod = sqrt(age.totalvar);
    ind_lomod = (a_atmean - a_dif*sd_mod)*(b_atmean - b_dif*sd_mod);
    ind_atmean = a_atmean*b_atmean;
    ind_himod = (a_atmean + a_dif*sd_mod)*(b_atmean + b_dif*sd_mod);
    ind_hivslo = ind_himod - ind_lomod;
  ',
  seed = 90291,
  burn = 10000,
  iter = 10000
)
output(model7)

# plot distribution of indirect effects
posterior_plot(model7, 'ind_lomod')
posterior_plot(model7, 'ind_atmean')
posterior_plot(model7, 'ind_himod')
posterior_plot(model7, 'ind_hivslo')



