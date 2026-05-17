# LINEAR REGRESSION WITH CATEGORICAL PREDICTORS ----

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/smoking.csv'

# create data frame from github data
smoking <- read.csv(data_url)

#------------------------------------------------------------------------------#
# LINEAR REGRESSION MODEL ----
#------------------------------------------------------------------------------#

# linear regression with automatic dummy coding
mod1 <- rblimp(
    data = smoking,
    ordinal = 'parsmoke',
    nominal = 'educ',
    center = 'age',
    model = 'smoking ~ parsmoke educ age', 
    seed = 90291,
    burn = 10000,
    iter = 10000)

# print output
output(mod1)

#------------------------------------------------------------------------------#
# GROUP MEANS AND CONTRASTS ----
#------------------------------------------------------------------------------#

# linear regression with explicit dummy coding
mod2 <- rblimp(
  data = smoking,
  ordinal = 'parsmoke',
  nominal = 'educ',
  center = 'age',
  model = 'smoking ~ intercept@b0 parsmoke@b1 educ.2@b2 educ.3@b3 age',
  parameters = '
    p_parsmoke = .452;
    mean_educ1 = b0 + (p_parsmoke * b1);
    mean_educ2 = b0 + b2 + (p_parsmoke * b1);
    mean_educ3 = b0 + b3 + (p_parsmoke * b1);
    educ2_vs_educ3 = mean_educ3 – mean_educ2;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(mod2)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# save multiply imputed data sets
mod3 <- rblimp(
  data = smoking,
  ordinal = 'parsmoke',
  nominal = 'educ',
  center = 'age',
  model = 'smoking ~ parsmoke educ age', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)               		           

output(mod3)

# plot distributions
posterior_plot(mod3)
distribution_plot(mod3)
residuals_plot(mod3)

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
# FIGURE 3.1 ----
#------------------------------------------------------------------------------#

# linear regression with automatic dummy coding
modelx <- rblimp(
  data = smoking,
  ordinal = 'parsmoke',
  nominal = 'educ',
  center = 'age',
  model = 'intensity ~ parsmoke educ age', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(modelx)

summary(modelx@average_imp)

summarytools::freq(modelx@average_imp$parsmoke)

dp <- distribution_plot(
  modelx,
  observed_color = "grey60",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 16,
  line_width     = 0.5
)

fig3_1A <- dp$parsmoke
fig3_1B <- dp$parsmoke.latent +
  geom_vline(xintercept = 0, linewidth = 0.5)

fig3_1 <- (fig3_1A / fig3_1B) +
  plot_annotation(tag_levels = "A") &
  book_theme &
  ggplot2::labs(title = NULL) &
  ggplot2::theme(legend.position = "none")

ggplot2::ggsave(
  filename = "~/desktop/Figure 3.1.pdf",
  plot     = fig3_1,
  width    = 8.5,
  height   = 11,
  units    = "in",
  device   = cairo_pdf
)
