# LOGISTIC REGRESSION FOR BINARY OUTCOMES ----

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
# FIT BINARY LOGISTIC MODEL ----
#------------------------------------------------------------------------------#

# binary logistic regression
mod1 <- rblimp(
  data = alcohol,
  ordinal = 'college male',
  nominal = 'drinker',
  center = 'age',
  model = 'drinker ~ agetryalc college age male', 
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(mod1)

# plot parameter distributions
posterior_plot(mod1,'drinker')

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# binary logistic regression
mod2 <- rblimp(
  data = alcohol,
  ordinal = 'college male',
  nominal = 'drinker',
  center = 'age',
  model = 'drinker ~ agetryalc college age male; DEBUG: compact_output', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

mod2q <- rblimp(
  data = alcohol,
  ordinal = 'college male',
  nominal = 'drinker',
  center = 'age',
  model = 'drinker ~ agetryalc college age age^2 age^3 male', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# mod2p <- rblimp(
#   data = alcohol,
#   ordinal = 'college male drinker',
#   model = 'drinker ~ agetryalc college age male',
#   seed = 90291,
#   burn = 10000,
#   iter = 10000,
#   nimps = 20)
# output(mod2p)

# print output
output(mod2)
posterior_plot(mod2)

names(mod2)



source("/Users/craig/Documents/Claude/Projects/Blimp Book/rblimp_cleaned_functions.R")

# plot distributions
distribution_plot(mod2)
residuals_plot(mod2)
residuals_plot(mod2q)

# average marginal predicted probabilities by college x male
library(mitml)

probs <- with(as.mitml(mod2),
              lm(drinker.1.probability ~ 0 +
                   I((male == 0) * (college == 0)) +
                   I((male == 0) * (college == 1)) +
                   I((male == 1) * (college == 0)) +
                   I((male == 1) * (college == 1))))

testEstimates(probs)


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
  mod2,
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

#------------------------------------------------------------------------------#
# FIGURE 3.7: DISTRIBUTIONS ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(
  mod2,
  observed_color = "grey60",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 16,
  line_width     = 0.5
)

fig3_7 <- dp$drinker.1.probability +
  book_theme &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 3.7.pdf",
  plot     = fig3_7,
  width    = 11,
  height   = 8.5,
  units    = "in",
  device   = cairo_pdf
)