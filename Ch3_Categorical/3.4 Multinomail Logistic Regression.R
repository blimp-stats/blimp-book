# BRIAN NOTES ----
# The .residual behavior with logistic models is incorrect
# No predicted values for logistic outcomes

# LOGISTIC REGRESSION FOR BINARY AND MULTICATEGORICAL OUTCOMES ----

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(ggplot2)
library(patchwork)
library(rblimp)

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/alcohol.csv'

# create data frame from github data
alcohol <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT MULTINOMIAL LOGISTIC MODEL ----
#------------------------------------------------------------------------------#

# multinomial logistic regression
mod1 <- rblimp(
  data = alcohol,
  ordinal = 'college male',
  nominal = 'drinkfreq',
  center = 'agetryalc age',
  model = 'drinkfreq ~ agetryalc college age male', 
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(mod1)

# plot parameter distributions
posterior_plot(mod1,'drinkfreq')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# multinomial logistic regression
mod2 <- rblimp(
  data = alcohol,
  ordinal = 'college male',
  nominal = 'drinkfreq',
  center = 'agetryalc age',
  model = 'drinkfreq ~ agetryalc college age male', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

mod2o <- rblimp(
  data = alcohol,
  ordinal = 'drinkfreq college male',
  # nominal = 'drinkfreq',
  center = 'agetryalc age',
  model = 'drinkfreq ~ agetryalc college age male', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

names(mod2o)
names(mod2)

# print output
output(mod2)

source("/Users/craig/Documents/Claude/Projects/Blimp Book/rblimp_cleaned_functions.R")

# plot distributions and binned residuals
distribution_plot(mod2)
residuals_plot(mod2)
residuals_plot(mod2o)

mod2@

#------------------------------------------------------------------------------#
# BOOK FIGURE THEME ----
#------------------------------------------------------------------------------#

book_theme <- ggplot2::theme(
  text              = ggplot2::element_text(family = "Minion Pro"),
  axis.text         = ggplot2::element_text(color = "black"),
  axis.line         = ggplot2::element_line(color = "black", linewidth = 0.5),
  axis.ticks        = ggplot2::element_line(color = "black", linewidth = 0.5),
  axis.ticks.length = grid::unit(4, "pt"),
  plot.tag          = ggplot2::element_text(face = "bold", size = 22),
  legend.position   = "bottom"
)

#------------------------------------------------------------------------------#
# FIGURE 3.6: DISTRIBUTIONS ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(
  mod3,
  observed_color = "grey60",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 16,
  line_width     = 0.5
)

fig3_6 <- dp$agetryalc +
  plot_layout(guides = "collect") +
  plot_annotation() &
  book_theme &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 3.6.pdf",
  plot     = fig3_6,
  width    = 11,
  height   = 8.5,
  units    = "in",
  device   = cairo_pdf
)