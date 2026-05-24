# LINEAR REGRESSION WITH CATEGORICAL PREDICTORS

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/smoking.csv'

# create data frame from github data
smoking <- read.csv(data_url)

#------------------------------------------------------------------------------#
# LINEAR REGRESSION MODEL ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = smoking,                                # R data frame
  ordinal = 'parsmoke',                          # binary and ordinal variables
  nominal = 'educ',                              # nominal variables (auto dummy coded)
  center = 'age',                                # center predictors
  model = 'cigsperday ~ parsmoke educ age',      # regression model
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod1)                                     # print output
posterior_plot(mod1, 'cigsperday')               # plot parameter distributions

#------------------------------------------------------------------------------#
# GROUP MEANS AND CONTRASTS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = smoking,                                # R data frame
  ordinal = 'parsmoke',                          # binary and ordinal variables
  nominal = 'educ',                              # nominal variables (auto dummy coded)
  center = 'age',                                # center predictors
  model = 'cigsperday ~ intercept@b0 parsmoke@b1 educ.2@b2 educ.3@b3 age', # label dummy codes
  parameters = '
    p_parsmoke = .452;                           # proportion of smokers
    mean_educ1 = b0 + (p_parsmoke * b1);         # group 1 mean
    mean_educ2 = b0 + b2 + (p_parsmoke * b1);    # group 2 mean
    mean_educ3 = b0 + b3 + (p_parsmoke * b1);    # group 3 mean
    educ2_vs_educ3 = mean_educ3 – mean_educ2;',  # group 3 vs. 2 contrast
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod2)                                     # print output

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = smoking,                                # R data frame
  ordinal = 'parsmoke',                          # binary and ordinal variables
  nominal = 'educ',                              # nominal variables (auto dummy coded)
  center = 'age',                                # center predictors
  model = 'cigsperday ~ parsmoke educ age',      # regression model
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod3)                                     # print output

distribution_plot(mod3)                          # plot observed and imputed distributions
residuals_plot(mod3)                             # plot residuals

#------------------------------------------------------------------------------#
# BOOK FIGURE THEME ----
#------------------------------------------------------------------------------#

library(patchwork)

book_theme <- ggplot2::theme(
  text              = ggplot2::element_text(family = "Minion Pro", size = 18),
  axis.text         = ggplot2::element_text(color = "black", size = 18),
  axis.line         = ggplot2::element_line(color = "black", linewidth = 0.5),
  axis.ticks        = ggplot2::element_line(color = "black", linewidth = 0.5),
  axis.ticks.length = grid::unit(4, "pt"),
  legend.text       = ggplot2::element_text(size = 18),
  legend.title      = ggplot2::element_text(size = 18),
  plot.tag          = ggplot2::element_text(face = "bold", size = 22),
  legend.position   = "bottom"
)

#------------------------------------------------------------------------------#
# FIGURE 3.1 ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(
  mod3,
  observed_color = "grey60",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 18,
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

#------------------------------------------------------------------------------#
# FIGURE 3.5: DISTRIBUTIONS ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(
  mod3,
  observed_color = "grey60",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 18,
  line_width     = 0.5
)

fig3_5 <- dp$cigsperday +
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
