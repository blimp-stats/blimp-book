# MULTILEVEL MODEL WITH LAGGED (DYNAMIC) EFFECTS

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)
set_blimp('/applications/blimp/blimp-nightly')

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/diary.csv'

# create data frame from github data
diary <- read.csv(data_url)

# FIT DSEM MODEL ----

# dsem with lagged predictors
model1 <- rblimp(
  data = diary,
  clusterid = 'person; timeid: day;',
  latent = 'person = pain_ranicept posaff_ranicept;',
  model = '
        pain_lag = pain.lag - pain_ranicept;
        posaff_lag = posaff.lag - posaff_ranicept;
        pain ~ intercept@pain_ranicept pain_lag posaff_lag;
        posaff ~ intercept@posaff_ranicept posaff_lag pain_lag;
        pain ~~ posaff;
        intercept -> pain_ranicept posaff_ranicept;
        pain_ranicept ~~ posaff_ranicept;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model1)

# plot parameter distributions
posterior_plot(model1,'posaff')

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model1)

# plot distributions and residuals
univariate_plot(vars = c('posaff_ranicept.latent','posaff_ranicept.residual','pain_ranicept.latent','pain_ranicept.residual','posaff.residual','pain.residual'), model1)

# plot standardized level-1 residuals vs. level-1 predictors
bivariate_plot(x_vars = c('pain_lag','posaff_lag'), y_vars = 'posaff.residual', standardize = 'y', model = model1)
bivariate_plot(x_vars = c('pain_lag','posaff_lag'), y_vars = 'pain.residual', standardize = 'y', model = model1)

# FIT DSEM MODEL WITH RANDOM WITHIN-CLUSTER VARIATION ----

# location-scale model for random variation
model2 <- rblimp(
  data = diary,
  clusterid = 'person; timeid: day;',
  latent = 'person = pain_ranicept posaff_ranicept pain_logvar posaff_logvar;',
  model = '
        pain_lag = pain.lag - pain_ranicept;
        posaff_lag = posaff.lag - posaff_ranicept;
        dv1:
        pain ~ intercept@pain_ranicept pain_lag posaff_lag;
        var(pain) ~ intercept@pain_logvar;
        dv2:
        posaff ~ intercept@posaff_ranicept posaff_lag pain_lag;
        var(posaff) ~ intercept@posaff_logvar;
        pain ~~ posaff;
        level2:
        intercept -> pain_ranicept posaff_ranicept pain_logvar posaff_logvar;
        pain_ranicept posaff_ranicept pain_logvar posaff_logvar ~~ pain_ranicept posaff_ranicept pain_logvar posaff_logvar;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model2)

# plot distributions and residuals
univariate_plot(vars = c('posaff_ranicept.latent','posaff_ranicept.residual','pain_logvar.latent','pain_logvar.residual',
                         'pain_ranicept.latent','pain_ranicept.residual','posaff.residual','posaff_logvar.latent','posaff_logvar.residual',
                         'pain.residual'), model2)

# plot standardized level-1 residuals vs. level-1 predictors
bivariate_plot(x_vars = c('pain_lag','posaff_lag'), y_vars = 'posaff.residual', standardize = 'y', model = model2)
bivariate_plot(x_vars = c('pain_lag','posaff_lag'), y_vars = 'pain.residual', standardize = 'y', model = model2)

# FIT DSEM MODEL WITH RANDOM SLOPES ----

# random slopes for autoregressive and cross-lagged effects
model3 <- rblimp(
  data = diary,
  clusterid = 'person; timeid: day;',
  latent = 'person = pain_ranicept pain_auto pain_clag pain_logvar posaff_ranicept posaff_auto posaff_clag posaff_logvar;',
  model = '
        pain_lag = pain.lag - pain_ranicept;
        posaff_lag = posaff.lag - posaff_ranicept;
        dv1:
        pain ~ intercept@pain_ranicept pain_lag@pain_auto posaff_lag@posaff_clag;
        var(pain) ~ intercept@pain_logvar;
        dv2:
        posaff ~ intercept@posaff_ranicept posaff_lag@posaff_auto pain_lag@pain_clag;
        var(posaff) ~ intercept@posaff_logvar;
        pain ~~ posaff;
        level2:
        intercept -> pain_ranicept pain_auto pain_clag pain_logvar posaff_ranicept posaff_auto posaff_clag posaff_logvar;
        pain_ranicept pain_auto pain_clag pain_logvar posaff_ranicept posaff_auto posaff_clag posaff_logvar ~~
          pain_ranicept pain_auto pain_clag pain_logvar posaff_ranicept posaff_auto posaff_clag posaff_logvar;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model3)

# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----

# plot imputed vs. observed values
imputation_plot(model3)

# plot distributions and residuals
residuals <- paste0(c('pain_ranicept','pain_auto','pain_clag','pain_logvar','pain','posaff_ranicept','posaff_auto','posaff_clag','posaff_logvar','posaff'), '.residual')
univariate_plot(vars = residuals, model3)

# plot standardized level-1 residuals vs. level-1 predictors
bivariate_plot(x_vars = c('pain_lag','posaff_lag'), y_vars = 'posaff.residual', standardize = 'y', model = model3)
bivariate_plot(x_vars = c('pain_lag','posaff_lag'), y_vars = 'pain.residual', standardize = 'y', model = model3)

