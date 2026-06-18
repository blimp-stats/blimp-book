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
  waldtest = 'dpdd ~ female age',                # wald test as nested model
  seed = 90291,                                  # random number seed
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
library(ggplot2)
library(ragg)

book_theme <- theme_classic(base_size = 18, base_family = "Minion Pro") +
  theme(
    text              = element_text(family = "Minion Pro", size = 18),
    axis.text         = element_text(color = "black", size = 18),
    axis.line         = element_line(color = "black", linewidth = 0.5, lineend = "square"),
    axis.ticks        = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(4, "pt"),
    legend.text       = element_text(size = 18),
    legend.title      = element_text(size = 18),
    plot.tag          = element_text(face = "bold", size = 22),
    legend.position   = "bottom"
  )

# uppercase only all-lowercase word tokens (variable names); leave "Centered", "~", etc.
.upcase_vars <- function(s) {
  if (!is.character(s) || length(s) != 1) return(s)
  toks <- strsplit(s, " ", fixed = TRUE)[[1]]
  is_var <- grepl("[a-z]", toks) & !grepl("[A-Z]", toks)   # has lowercase, no uppercase
  toks[is_var] <- toupper(toks[is_var])
  paste(toks, collapse = " ")
}

caps_axes <- structure(list(), class = "caps_axes")
ggplot_add.caps_axes <- function(object, plot, ...) {
  plot$labels$x <- .upcase_vars(plot$labels$x)
  plot$labels$y <- .upcase_vars(plot$labels$y)
  plot
}

save_fig <- function(plot, name, width = 8.5, height = 11,
                     dir = fig_dir, dpi = 600) {
  pdf_path <- file.path('/Users/craig/Dropbox/Research/Applied Data Modeling in Blimp/Figures', paste0(name, ".pdf"))
  png_path <- file.path('/Users/craig/Dropbox/Research/Applied Data Modeling in Blimp/Figures', paste0(name, ".png"))
  ggsave(pdf_path, plot, width = width, height = height,
         units = "in", device = cairo_pdf)
  ggsave(png_path, plot, width = width, height = height,
         units = "in", dpi = dpi, device = agg_png)
  message("Wrote:\n  ", pdf_path, "\n  ", png_path)   # confirms exact paths
  invisible(c(pdf_path, png_path))
}

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
chain_mean <- mean(layer_data(tp)$y)   # full series

fig2_2 <- tp +
  geom_hline(yintercept = chain_mean,
                      linetype  = "dashed",
                      color     = "grey40") +
  xlim(0, 100) +
  scale_color_manual(values = "black") +
  ylab("Slope Parameter") +
  labs(title = NULL, subtitle = NULL) +
  book_theme +
  theme(
    text            = element_text(size = 20),
    axis.title      = element_text(size = 20),
    axis.text       = element_text(size = 20),
    legend.position = "none"
  )

save_fig(fig2_2, "Figure 2.2", width = 11, height = 8.5)

#------------------------------------------------------------------------------#
# FIGURE 2.5: POSTERIOR PLOTS ----
#------------------------------------------------------------------------------#
panel_format <- list(
  # scale_fill_manual(values = "grey80"),
  scale_color_manual(values = "black"),
  labs(title = NULL, subtitle = NULL),
  book_theme,
  scale_fill_manual(values = NA),
  theme(legend.position = "none")
)

fig2_5a <- posterior_plot(mod5, 3,  line_width = 0.5) + panel_format
fig2_5b <- posterior_plot(mod5, 12, line_width = 0.5) + panel_format

fig2_5 <- (fig2_5a / fig2_5b) +
  plot_annotation(tag_levels = "A") &
  book_theme &
  labs(title = NULL) &
  theme(legend.position = "none")

save_fig(fig2_5, "Figure 2.5", width = 8.5, height = 11)

#------------------------------------------------------------------------------#
# FIGURE 2.6: TRACE PLOTS ----
#------------------------------------------------------------------------------#

fig2_6a <- trace_plot(mod1, 3) +
  xlim(9500, 10000) +
  scale_color_manual(values = c("black", "grey30", "grey55", "grey80")) +
  ylab("Slope Parameter") +
  labs(title = NULL, subtitle = NULL) +
  book_theme +
  theme(legend.position = "none")

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
  xlim(9500, 10000) +
  scale_color_manual(values = c("black", "grey30", "grey55", "grey80")) +
  ylab("Intercept Parameter") +
  labs(title = NULL, subtitle = NULL) +
  book_theme +
  theme(legend.position = "none")

fig2_6 <- (fig2_6a / fig2_6b) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  book_theme &
  labs(title = NULL)

save_fig(fig2_6, "Figure 2.6", width = 8.5, height = 11)

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
  caps_axes &
  labs(title = NULL)

save_fig(fig2_7, "Figure 2.7", width = 8.5, height = 11)

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
  label_family = "Minion Pro",
  point_size = 0.4, point_alpha = 0.3
)

fig2_8 <- dp$dpdd.residual / rp$dpdd.index +
  plot_annotation(tag_levels = "A") &
  book_theme &
  caps_axes &
  labs(title = NULL)

save_fig(fig2_8, "Figure 2.8", width = 8.5, height = 11)

#------------------------------------------------------------------------------#
# FIGURE 2.9: RESIDUAL VS. PREDICTED + RESIDUAL VS. PREDICTORS ----
#------------------------------------------------------------------------------#

rp <- residuals_plot(
  mod5,
  point_color  = "grey40",
  curve_color  = "black",
  font_size    = 18,
  line_width   = 0.6,
  label_family = "Minion Pro",
  point_size = 0.4, point_alpha = 0.3
)

fig2_9 <- rp$dpdd.predicted / rp$dpdd.inflam / rp$dpdd.age +
  plot_annotation(tag_levels = "A") &
  book_theme &
  caps_axes &
  labs(title = NULL)

save_fig(fig2_9, "Figure 2.9", width = 8.5, height = 11)

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
  caps_axes &
  labs(title = NULL)

save_fig(fig2_10, "Figure 2.10", width = 11, height = 8.5)
