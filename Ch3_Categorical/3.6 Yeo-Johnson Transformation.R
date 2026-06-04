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
# LINEAR REGRESSION WITH CONSTANT VARIANCE ----
#------------------------------------------------------------------------------#

mod0 <- rblimp(
  data = inflammation,                           # R data frame
  ordinal = 'els female',                        # binary and ordinal variables
  center = 'inflam age',                         # center predictors
  model = 'dpdd ~ inflam els female age',        # regression model
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets                                 

output(mod0)                                     # print output

residuals_plot(mod0)                             # plot residuals


#------------------------------------------------------------------------------#
# LINEAR REGRESSION WITH YEO-JOHNSON TRANSFORMATION ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = inflammation,                           # R data frame
  transform = 'dpddcent = dpdd - median(dpdd)',  # compute new centered dv
  ordinal = 'els female',                        # binary and ordinal variables
  center = 'inflam age',                         # center predictors
  model = 'yjt(dpddcent) ~ inflam els female age', # regression model w yeo-johnson transform
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod1)                                     # print output
posterior_plot(mod1, 'dpddcent')                 # plot parameter distributions

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = inflammation,                           # R data frame
  transform = 'dpddcent = dpdd - median(dpdd)',  # compute new centered dv
  ordinal = 'els female',                        # binary and ordinal variables
  center = 'inflam age',                         # center predictors
  model = 'yjt(dpddcent) ~ inflam els female age', # regression model w yeo-johnson transform
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod2)               		                   # print output

distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot residuals

#------------------------------------------------------------------------------#
# LINEAR REGRESSION WITH YEO-JOHNSON TRANSFORMED PREDICTOR ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = inflammation,                           # R data frame
  transform = 'dpddcent = dpdd - median(dpdd)',  # compute new centered dv
  ordinal = 'els female',                        # binary and ordinal variables
  center = 'inflam age',                         # center predictors
  model = '
    yjt(dpddcent) ~ inflam els female age;       # outcome w yeo-johnson transform
    yjt(inflam) ~ els female age',               # predictor w yeo-johnson transform
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod3)                                     # print output
posterior_plot(mod3, 'dpddcent')                 # plot parameter distributions

#------------------------------------------------------------------------------#
# LINEAR REGRESSION WITH NORMALIZING TRANSFORMATIONS ----
#------------------------------------------------------------------------------#

# log transform
mod4 <- rblimp(
  data = inflammation,                           # R data frame
  ordinal = 'els female',                        # binary and ordinal variables
  center = 'inflam age',                         # center predictors
  model = '
  dpddnorm = ln(dpdd + 1);                       # definition variable adds transform to data
  ln(dpdd + 1) ~ inflam els female age',         # regression model w yeo-johnson transform
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

distribution_plot(mod4)                          # plot observed and imputed distributions
residuals_plot(mod4)                             # plot residuals

# inverse  transform
mod5 <- rblimp(
  data = inflammation,                           # R data frame
  ordinal = 'els female',                        # binary and ordinal variables
  center = 'inflam age',                         # center predictors
  model = '
  dpddnorm = (1 / (dpdd + 1));                   # # definition variable adds transform to data
  (1 / (dpdd + 1)) ~ inflam els female age',     # regression model w inverse transform
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

distribution_plot(mod5)                          # plot observed and imputed distributions
residuals_plot(mod5)                             # plot residuals

# square root transform
mod6 <- rblimp(
  data = inflammation,                           # R data frame
  ordinal = 'els female',                        # binary and ordinal variables
  center = 'inflam age',                         # center predictors
  model = '
  dpddnorm = sqrt(dpdd);                         # # definition variable adds transform to data
  sqrt(dpdd) ~ inflam els female age',           # regression model w yeo-johnson transform
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

distribution_plot(mod6)                          # plot observed and imputed distributions
residuals_plot(mod6)                             # plot residuals

# inverse square root transform
mod7 <- rblimp(
  data = inflammation,                           # R data frame
  ordinal = 'els female',                        # binary and ordinal variables
  center = 'inflam age',                         # center predictors
  model = '
  dpddnorm = (1 / sqrt(dpdd + 1));               # # definition variable adds transform to data
  (1 / sqrt(dpdd + 1)) ~ inflam els female age', # regression model w yeo-johnson transform
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

distribution_plot(mod7)                          # plot observed and imputed distributions
residuals_plot(mod7)                             # plot residuals

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


#------------------------------------------------------------------------------#
# FIGURE 3.12: DISTRIBUTIONS ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(
  mod2,
  observed_color = "grey60",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 18,
  line_width     = 0.5
)

fig3_12 <- dp$dpddcent.yjt / dp$dpddcent +
  plot_annotation(tag_levels = "A") &
  book_theme &
  caps_axes &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 3.12.pdf",
  plot     = fig3_12,
  width    = 8.5,
  height   = 11,
  units    = "in",
  device   = cairo_pdf
)

#------------------------------------------------------------------------------#
# FIGURE 3.13: RESIDUAL VS. PREDICTED + RESIDUAL VS. PREDICTORS ----
#------------------------------------------------------------------------------#

rp <- residuals_plot(
  mod2,
  point_color  = "grey40",
  curve_color  = "black",
  font_size    = 18,
  line_width   = 0.6,
  label_family = "Minion Pro"
)

fig3_13 <- rp$dpddcent.index / rp$dpddcent.inflam / rp$dpddcent.age +
  plot_annotation(tag_levels = "A") &
  book_theme &
  caps_axes &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 3.13.pdf",
  plot     = fig3_13,
  width    = 8.5,
  height   = 11,
  units    = "in",
  device   = cairo_pdf
)









#------------------------------------------------------------------------------#
# FIGURE 2.8: RESIDUAL VS. PREDICTED + RESIDUAL VS. PREDICTORS ----
#------------------------------------------------------------------------------#

rp <- residuals_plot(
  mod2,
  point_color  = "grey40",
  curve_color  = "black",
  font_size    = 18,
  line_width   = 0.6,
  label_family = "Minion Pro"
)

fig3_10 <- rp$dpdd.index / rp$dpdd.inflam / rp$dpdd.age +
  plot_annotation(tag_levels = "A") &
  book_theme &
  caps_axes &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 3.10.pdf",
  plot     = fig3_10,
  width    = 8.5,
  height   = 11,
  units    = "in",
  device   = cairo_pdf
)