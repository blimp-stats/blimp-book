# LOGISTIC REGRESSION FOR BINARY OUTCOMES

# plotting functions
# source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source("/Users/craig/Documents/Claude/Projects/Blimp Book/rblimp_cleaned_functions.R")

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(ggplot2)
library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/alcohol.csv'

# create data frame from github data
alcohol <- read.csv(data_url)

#------------------------------------------------------------------------------#
# BINARY LOGISTIC MODEL ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = alcohol,                                # R data frame
  ordinal = 'college male',                      # binary and ordinal variables
  nominal = 'drinker',                           # nominal variables (auto dummy coded)
  center = 'agetryalc age',                      # center predictors
  model = 'drinker ~ agetryalc college age male', # regression model
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod1)                                     # print output
posterior_plot(mod1, 'drinker')                  # plot parameter distributions

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = alcohol,                                # R data frame
  ordinal = 'college male',                      # binary and ordinal variables
  nominal = 'drinker',                           # nominal variables (auto dummy coded)
  center = 'agetryalc age',                      # center predictors
  model = 'drinker ~ agetryalc college age male', # regression model
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod2)                                     # print output

distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot binned residuals

#------------------------------------------------------------------------------#
# MARGINAL PREDICTED PROBABILITIES ----
#------------------------------------------------------------------------------#

# average marginal predicted probabilities by college x male
library(mitml)

implist <- as.mitml(mod2)                        # mitml-compatible list of data frames
probs <- with(implist,                           # fit lm to each data set
              lm(drinker.1.probability ~ 0 +
                   I((male == 0) * (college == 0)) +
                   I((male == 0) * (college == 1)) +
                   I((male == 1) * (college == 0)) +
                   I((male == 1) * (college == 1))))

testEstimates(probs)                             # pool estimates and standard errors

#------------------------------------------------------------------------------#
# LOGISTIC MODELS WITH CURVILINEAR AGE EFFECTS ----
#------------------------------------------------------------------------------#

# quadratic age effect
mod3 <- rblimp(
  data = alcohol,                                # R data frame
  ordinal = 'college male',                      # binary and ordinal variables
  nominal = 'drinker',                           # nominal variables (auto dummy coded)
  center = 'agetryalc age',                      # center predictors
  model = 'drinker ~ agetryalc college age age^2 male', # regression with quadratic age
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

residuals_plot(mod3)                             # plot binned residuals

# cubic age effect
mod4 <- rblimp(
  data = alcohol,                                # R data frame
  ordinal = 'college male',                      # binary and ordinal variables
  nominal = 'drinker',                           # binary outcome (auto dummy coded)
  center = 'agetryalc age',                      # center predictors
  model = 'drinker ~ agetryalc college age age^2 age^3 male', # regression with cubic age
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

residuals_plot(mod4)                             # plot binned residuals

#------------------------------------------------------------------------------#
# BOOK FIGURE THEME ----
#------------------------------------------------------------------------------#

library(patchwork)

book_theme <- ggplot2::theme(
  text              = ggplot2::element_text(family = "Minion Pro", size = 18),
  axis.text         = ggplot2::element_text(color = "black", size = 18),
  axis.line         = ggplot2::element_line(color = "black", linewidth = 0.5, lineend = "square"),
  axis.ticks        = ggplot2::element_line(color = "black", linewidth = 0.5),
  axis.ticks.length = grid::unit(4, "pt"),
  legend.text       = ggplot2::element_text(size = 18),
  legend.title      = ggplot2::element_text(size = 18),
  plot.tag          = ggplot2::element_text(face = "bold", size = 22),
  legend.position   = "bottom"
)

#------------------------------------------------------------------------------#
# FIGURE 3.5: DISTRIBUTIONS ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(
  mod2,
  observed_color = "grey60",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 18,
  line_width     = 0.5
)

fig3_5 <- dp$agetryalc +
  plot_layout(guides = "collect") +
  plot_annotation() &
  book_theme &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 3.5.pdf",
  plot     = fig3_5,
  width    = 11,
  height   = 8.5,
  units    = "in",
  device   = cairo_pdf
)

#------------------------------------------------------------------------------#
# FIGURE 3.6: PREDICTED PROBABILITIES ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(
  mod2,
  observed_color = "grey60",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 18,
  line_width     = 0.5
)

rp <- residuals_plot(
  mod2,
  point_color  = "grey40",
  curve_color  = "black",
  font_size    = 18,
  line_width   = 0.6,
  label_family = "Minion Pro"
)

fig3_6 <- dp$drinker.1.probability / rp$drinker.college.probability +
  plot_annotation(tag_levels = "A") &
  book_theme &
  ggplot2::labs(title = NULL) &
  ggplot2::scale_color_manual(values = rep("black", 7))

ggplot2::ggsave(
  filename = "~/desktop/Figure 3.6.pdf",
  plot     = fig3_6,
  width    = 8.5,
  height   = 11,
  units    = "in",
  device   = cairo_pdf
)

#------------------------------------------------------------------------------#
# FIGURE 3.7: RESIDUAL VS. PREDICTED + RESIDUAL VS. PREDICTORS ----
#------------------------------------------------------------------------------#

rp <- residuals_plot(
  mod2,
  point_color  = "grey40",
  curve_color  = "black",
  font_size    = 18,
  line_width   = 0.6,
  label_family = "Minion Pro"
)

fig3_7 <- rp$drinker.1.binned / rp$drinker.1.agetryalc / rp$drinker.1.age +
  plot_annotation(tag_levels = "A") &
  book_theme &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 3.7.pdf",
  plot     = fig3_7,
  width    = 8.5,
  height   = 11,
  units    = "in",
  device   = cairo_pdf
)
