# LINEAR REGRESSION

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflammation <- read.csv(data_url)

#------------------------------------------------------------------------------#
# LINEAR REGRESSION MODEL ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = inflammation,                           # R data frame
  ordinal = 'els female',                        # binary and ordinal variables
  center = 'inflam age',                         # center predictors
  model = 'dpdd ~ inflam els female age',        # regression model
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod1)                                     # print output
posterior_plot(mod1, 'dpdd')                     # plot parameter distributions

#------------------------------------------------------------------------------#
# CUSTOM WALD TEST ----
#------------------------------------------------------------------------------#

# method 1: parameter labels
mod2 <- rblimp(
  data = inflammation,                           # R data frame
  ordinal = 'els female',                        # binary and ordinal variables
  center = 'inflam age',                         # center predictors
  model = 'dpdd ~ inflam@b1 els@b2 female age',  # labeled slopes
  seed = 90291,                                  # random number seed
  waldtest = 'b1 = 0; b2 = 0',                   # wald test
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod2)                                     # print output

# method 2: nested model
mod3 <- rblimp(
  data = inflammation,                           # R data frame
  ordinal = 'els female',                        # binary and ordinal variables
  center = 'inflam age',                         # center predictors
  model = 'dpdd ~ inflam els female age',        # regression model
  seed = 90291,                                  # random number seed
  waldtest = 'dpdd ~ female age',                # wald test as nested model
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod3)                                     # print output

# multiple wald tests
mod4 <- rblimp(
  data = inflammation,                           # R data frame
  ordinal = 'els female',                        # binary and ordinal variables
  center = 'inflam age',                         # center predictors
  model = 'dpdd ~ inflam@b1 els@b2 female@b3 age@b4',  # labeled slopes
  waldtest = c('b1:b2 = 0', 'b3:b4 = 0'),        # vector of tests
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod4)                                     # print output

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod5 <- rblimp(
  data = inflammation,                           # R data frame
  ordinal = 'els female',                        # binary and ordinal variables
  center = 'inflam age',                         # center predictors
  model = 'dpdd ~ inflam els female age',        # regression model
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod5)                                     # print output

distribution_plot(mod5)                          # plot observed and imputed distributions
residuals_plot(mod5)                             # plot residuals

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
# FIGURE 2.2: SINGLE-CHAIN TRACE PLOT ----
#------------------------------------------------------------------------------#

# fit single-chain model for trace plot diagnostic
mod_1chain <- rblimp(
  data    = inflammation,
  ordinal = 'els female',
  center  = 'inflam age',
  model   = 'dpdd ~ inflam els female age',
  seed    = 90291,
  chains  = 1,
  burn    = 10000,
  iter    = 10000
)
output(mod_1chain)

tp <- trace_plot(mod_1chain, 3)
chain_mean <- mean(ggplot2::layer_data(tp)$y)   # full series

fig2_2 <- tp +
  ggplot2::geom_hline(yintercept = chain_mean,
                      linetype  = "dashed",
                      color     = "grey40") +
  ggplot2::xlim(0, 100) +
  ggplot2::scale_color_manual(values = "black") +
  ggplot2::ylab("Slope Parameter") +
  ggplot2::labs(title = NULL, subtitle = NULL) +
  ggplot2::theme_classic(base_size = 20, base_family = "Minion Pro") +
  book_theme +
  ggplot2::theme(
    text            = ggplot2::element_text(size = 20),
    axis.title      = ggplot2::element_text(size = 20),
    axis.text       = ggplot2::element_text(size = 20),
    legend.position = "none"
  )

ggplot2::ggsave(
  filename = "~/desktop/Figure 2.2.pdf",
  plot     = fig2_2,
  width    = 11,
  height   = 8.5,
  units    = "in",
  device   = cairo_pdf
)

#------------------------------------------------------------------------------#
# FIGURE 2.5: POSTERIOR PLOTS ----
#------------------------------------------------------------------------------#
panel_format <- list(
  # ggplot2::scale_fill_manual(values = "grey80"),
  ggplot2::scale_color_manual(values = "black"),
  ggplot2::labs(title = NULL, subtitle = NULL),
  ggplot2::theme_classic(base_size = 18, base_family = "Minion Pro"),
  book_theme,
  ggplot2::scale_fill_manual(values = NA),
  ggplot2::theme(legend.position = "none")
)

fig2_5a <- posterior_plot(mod5, 3,  line_width = 0.5) + panel_format
fig2_5b <- posterior_plot(mod5, 12, line_width = 0.5) + panel_format

fig2_5 <- (fig2_5a / fig2_5b) +
  plot_annotation(tag_levels = "A") &
  book_theme &
  ggplot2::labs(title = NULL) &
  ggplot2::theme(legend.position = "none")

ggplot2::ggsave(
  filename = "~/desktop/Figure 2.5.pdf",
  plot     = fig2_5,
  width    = 8.5,
  height   = 11,
  units    = "in",
  device   = cairo_pdf
)

#------------------------------------------------------------------------------#
# FIGURE 2.6: TRACE PLOTS ----
#------------------------------------------------------------------------------#

fig2_6a <- trace_plot(mod1, 3) +
  ggplot2::xlim(9500, 10000) +
  ggplot2::scale_color_manual(values = c("black", "grey30", "grey55", "grey80")) +
  ggplot2::ylab("Slope Parameter") +
  ggplot2::labs(title = NULL, subtitle = NULL) +
  ggplot2::theme_classic(base_size = 18, base_family = "Minion Pro") +
  book_theme +
  ggplot2::theme(legend.position = "none")

# different example that doesn't converge
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/convergence%20problem%20data.csv'
noconverge <- read.csv(data_url)

failure <- rblimp(
  data = noconverge,
  nominal = 'genderid race',
  model = 'sleep ~ age genderid race',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  options = 'pinfo'
)
output(failure)

fig2_6b <- trace_plot(failure, 63) +
  ggplot2::xlim(9500, 10000) +
  ggplot2::scale_color_manual(values = c("black", "grey30", "grey55", "grey80")) +
  ggplot2::ylab("Intercept Parameter") +
  ggplot2::labs(title = NULL, subtitle = NULL) +
  ggplot2::theme_classic(base_size = 18, base_family = "Minion Pro") +
  book_theme +
  ggplot2::theme(legend.position = "none")

fig2_6 <- (fig2_6a / fig2_6b) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  book_theme &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 2.6.pdf",
  plot     = fig2_6,
  width    = 8.5,
  height   = 11,
  units    = "in",
  device   = cairo_pdf
)

#------------------------------------------------------------------------------#
# FIGURE 2.7: DISTRIBUTIONS ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(
  mod5,
  observed_color = "grey60",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 18,
  line_width     = 0.5
)

fig2_7 <- (dp$dpdd / dp$inflam) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  book_theme &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 2.7.pdf",
  plot     = fig2_7,
  width    = 8.5,
  height   = 11,
  units    = "in",
  device   = cairo_pdf
)

#------------------------------------------------------------------------------#
# FIGURE 2.8: RESIDUAL DISTRIBUTION + STANDARDIZED RESIDUAL INDEX ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(
  mod5,
  vars           = "dpdd.residual",
  observed_color = "grey40",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 18,
  line_width     = 0.6
)

rp <- residuals_plot(
  mod5,
  point_color  = "grey40",
  curve_color  = "black",
  font_size    = 18,
  line_width   = 0.6,
  label_family = "Minion Pro"
)

fig2_8 <- dp$dpdd.residual / rp$dpdd.index +
  plot_annotation(tag_levels = "A") &
  book_theme &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 2.8.pdf",
  plot     = fig2_8,
  width    = 8.5,
  height   = 11,
  units    = "in",
  device   = cairo_pdf
)

#------------------------------------------------------------------------------#
# FIGURE 2.9: RESIDUAL VS. PREDICTED + RESIDUAL VS. PREDICTORS ----
#------------------------------------------------------------------------------#

rp <- residuals_plot(
  mod5,
  point_color  = "grey40",
  curve_color  = "black",
  font_size    = 18,
  line_width   = 0.6,
  label_family = "Minion Pro"
)

fig2_9 <- rp$dpdd.predicted / rp$dpdd.inflam / rp$dpdd.age +
  plot_annotation(tag_levels = "A") &
  book_theme &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 2.9.pdf",
  plot     = fig2_9,
  width    = 8.5,
  height   = 11,
  units    = "in",
  device   = cairo_pdf
)

#------------------------------------------------------------------------------#
# FIGURE 2.10: COOK'S D AND LEVERAGE ----
#------------------------------------------------------------------------------#

rp <- residuals_plot(
  mod5,
  point_color  = "grey40",
  curve_color  = "black",
  font_size    = 18,
  line_width   = 0.6,
  label_family = "Minion Pro"
)

fig2_10 <- rp$dpdd.leverage / rp$dpdd.cooks +
  plot_annotation(tag_levels = "A") &
  book_theme &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 2.10.pdf",
  plot     = fig2_10,
  width    = 11,
  height   = 8.5,
  units    = "in",
  device   = cairo_pdf
)
