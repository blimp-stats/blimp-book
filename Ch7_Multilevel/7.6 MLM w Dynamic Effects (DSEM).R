# MULTILEVEL MODEL WITH LAGGED (DYNAMIC) EFFECTS

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/diary.csv'

# create data frame from github data
diary <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT DSEM MODEL ----
#------------------------------------------------------------------------------#

# dsem with lagged predictors
model1 <- rblimp(
  data = diary,
  clusterid = 'person', 
  timeid = 'day',
  latent = 'person = pain_icept posaff_icept;',
  model = '
        pain_lag = pain.lag - pain_icept;
        posaff_lag = posaff.lag - posaff_icept;
        pain ~ intercept@pain_icept pain_lag posaff_lag;
        posaff ~ intercept@posaff_icept posaff_lag pain_lag;
        pain ~~ posaff;
        intercept -> pain_icept posaff_icept;
        pain_icept ~~ posaff_icept;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1,'posaff')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model1)

# plot distributions and residuals
univariate_plot(vars = c('posaff_icept.latent','posaff_icept.residual','pain_icept.latent','pain_icept.residual','posaff.residual','pain.residual'), model1)

# plot standardized level-1 residuals vs. level-1 predictors
bivariate_plot(x_vars = c('pain_lag','posaff_lag'), y_vars = 'posaff.residual', standardize = 'y', model = model1)
bivariate_plot(x_vars = c('pain_lag','posaff_lag'), y_vars = 'pain.residual', standardize = 'y', model = model1)

#------------------------------------------------------------------------------#
# FIT DSEM MODEL WITH RANDOM WITHIN-CLUSTER VARIATION ----
#------------------------------------------------------------------------------#

# location-scale model for random variation
model2 <- rblimp(
  data = diary,
  clusterid = 'person', 
  timeid = 'day',
  latent = 'person = pain_icept posaff_icept pain_logvar posaff_logvar;',
  model = '
        pain_lag = pain.lag - pain_icept;
        posaff_lag = posaff.lag - posaff_icept;
        dv1:
        pain ~ intercept@pain_icept pain_lag posaff_lag;
        var(pain) ~ intercept@pain_logvar;
        dv2:
        posaff ~ intercept@posaff_icept posaff_lag pain_lag;
        var(posaff) ~ intercept@posaff_logvar;
        pain ~~ posaff;
        level2:
        intercept -> pain_icept posaff_icept pain_logvar posaff_logvar;
        pain_icept posaff_icept pain_logvar posaff_logvar ~~ pain_icept posaff_icept pain_logvar posaff_logvar;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model2)

# plot distributions and residuals
univariate_plot(vars = c('posaff_icept.latent','posaff_icept.residual','pain_logvar.latent','pain_logvar.residual',
                         'pain_icept.latent','pain_icept.residual','posaff.residual','posaff_logvar.latent','posaff_logvar.residual',
                         'pain.residual'), model2)

# plot standardized level-1 residuals vs. level-1 predictors
bivariate_plot(x_vars = c('pain_lag','posaff_lag'), y_vars = 'posaff.residual', standardize = 'y', model = model2)
bivariate_plot(x_vars = c('pain_lag','posaff_lag'), y_vars = 'pain.residual', standardize = 'y', model = model2)

#------------------------------------------------------------------------------#
# FIT DSEM MODEL WITH RANDOM SLOPES ----
#------------------------------------------------------------------------------#

# random slopes for autoregressive and cross-lagged effects
model3 <- rblimp(
  data = diary,
  clusterid = 'person', 
  timeid = 'day',
  latent = 'person = pain_icept pain_auto pain_clag pain_logvar posaff_icept posaff_auto posaff_clag posaff_logvar;',
  model = '
        pain_lag = pain.lag - pain_icept;
        posaff_lag = posaff.lag - posaff_icept;
        dv1:
        pain ~ intercept@pain_icept pain_lag@pain_auto posaff_lag@posaff_clag;
        var(pain) ~ intercept@pain_logvar;
        dv2:
        posaff ~ intercept@posaff_icept posaff_lag@posaff_auto pain_lag@pain_clag;
        var(posaff) ~ intercept@posaff_logvar;
        pain ~~ posaff;
        level2:
        intercept -> pain_icept pain_auto pain_clag pain_logvar posaff_icept posaff_auto posaff_clag posaff_logvar;
        pain_icept pain_auto pain_clag pain_logvar posaff_icept posaff_auto posaff_clag posaff_logvar ~~
          pain_icept pain_auto pain_clag pain_logvar posaff_icept posaff_auto posaff_clag posaff_logvar;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model3)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# plot imputed vs. observed values
imputation_plot(model3)

# plot distributions and residuals
residuals <- paste0(c('pain_icept','pain_auto','pain_clag','pain_logvar','pain','posaff_icept','posaff_auto','posaff_clag','posaff_logvar','posaff'), '.residual')
univariate_plot(vars = residuals, model3)

# plot standardized level-1 residuals vs. level-1 predictors
bivariate_plot(x_vars = c('pain_lag','posaff_lag'), y_vars = 'posaff.residual', standardize = 'y', model = model3)
bivariate_plot(x_vars = c('pain_lag','posaff_lag'), y_vars = 'pain.residual', standardize = 'y', model = model3)

