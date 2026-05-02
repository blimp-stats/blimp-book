# LINEAR REGRESSION

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')
set_blimp('/applications/blimp/blimp')

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflamm <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT LINEAR REGRESSION MODEL ----
#------------------------------------------------------------------------------#

# raw score predictors
model1 <- rblimp(
  data = inflamm,
  nominal = 'female els',
  model = 'dpdd ~ female els age inflam_sum', 
  seed = 90291,
  chains = 4,
  burn = 10000,
  iter = 10000)

# trace plot of model parameters
trace_plot(model1, 'dpdd') + ggplot2::xlim(0, 50) + ggplot2::theme_minimal()

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1,'dpdd')

#------------------------------------------------------------------------------#
# FIT MODEL WITH CENTERED PREDICTORS ----
#------------------------------------------------------------------------------#

# mean-centered predictors
mymodel <- rblimp(
  data = inflammation,             		             # R data frame
  ordinal = 'female els',          		             # binary and ordinal variables         
  center = 'female els age inflam_sum',	           # center predictors  
	model = 'dpdd ~ female els age inflam_sum;',     # regression model
  seed = 90291,               		                 # random number seed
  burn = 10000,               		                 # warm-up iterations
  iter = 10000                		                 # analysis iterations
)
output(mymodel)               		                 # print output


# trace plot of model parameters
trace_plot(model2, 'dpdd') + ggplot2::xlim(0, 250) + ggplot2::theme_minimal()

# print output
output(model2)

# plot parameter distributions
posterior_plot(model2,'dpdd')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model2)

# plot distributions and residuals
univariate_plot(vars = c('dpdd','dpdd.residual'), model2)

# plot standardized residuals vs. predicted values
bivariate_plot(dpdd.residual ~ dpdd.predicted, standardize = 'y', model = model2)

# plot standardized residuals vs. numeric predictors
bivariate_plot(y_vars = 'dpdd.residual', 
               x_vars = c('age','inflam_sum'),
               standardize = 'y',
               model = model2)



