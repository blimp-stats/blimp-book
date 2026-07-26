# MULTILEVEL LINEAR GROWTH MODEL

# plotting functions
# source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source("/Users/craig/Dropbox/Claude/Projects/Blimp Book/rblimp_cleaned_functions.R")

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/medtrial.csv'

# create data frame from github data
medtrial <- read.csv(data_url)

summary(medtrial)

#------------------------------------------------------------------------------#
# PLOT MEANS ----
#------------------------------------------------------------------------------#

ggplot(medtrial, aes(x = visit, y = severity)) +
  stat_summary(fun = mean, geom = "line",  na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE)

#------------------------------------------------------------------------------#
# INTRACLASS CORRELATION ----
#------------------------------------------------------------------------------#

mod0 <- rblimp(
  data = medtrial,
  clusterid = 'person',
  model = 'severity ~ intercept',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(mod0)

#------------------------------------------------------------------------------#
# LINEAR GROWTH MODEL ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = medtrial,
  clusterid = 'person',
  model = 'severity ~ intercept visit | visit; DEBUG: compact_output',
  seed = 90291,
  burn = 25000,
  iter = 25000)

# print output
output(mod1)

# plot parameter distributions
posterior_plot(mod1)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = medtrial,
  clusterid = 'person',
  model = 'severity ~ intercept visit | visit',
  seed = 90291,
  burn = 25000,
  iter = 25000,
  nimps = 20)

distribution_plot(mod2)
residuals_plot(mod2)

#------------------------------------------------------------------------------#
# QUADRATIC GROWTH MODEL ----
#------------------------------------------------------------------------------#

# fixed quadratic effect
mod3 <- rblimp(
  data = medtrial,
  clusterid = 'person',
  model = 'severity ~ intercept visit visit^2 | visit; DEBUG: compact_output',
  seed = 90291,
  burn = 25000,
  iter = 25000)

# print output
output(mod3)

# random quadratic effect
mod4 <- rblimp(
  data = medtrial,
  clusterid = 'person',
  model = 'severity ~ intercept visit visit^2 | visit visit^2',
  seed = 90291,
  burn = 30000,
  iter = 30000)

# print output
output(mod4)

#------------------------------------------------------------------------------#
# LINEAR GROWTH MODEL (LATENT SPECIFICATION) ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = medtrial,
  clusterid = 'person',
  latent = 'person = b0j b1j',
  model = '
    level2:
    b0j ~ intercept;
    b1j ~ intercept;
    b0j ~~ b1j;
    level1:
    severity ~ intercept@b0j visit@b1j;',
  seed = 90291,
  burn = 20000,
  iter = 20000,
  nimps = 20)

# print output
output(mod3)

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
# FIGURE 7.5: MEANS ----
#------------------------------------------------------------------------------#

fig7_5 <- ggplot(medtrial, aes(x = visit, y = severity)) +
  stat_summary(fun = mean, geom = "line",  na.rm = TRUE, color = "black") +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE, color = "black", size = 2.5) +
  labs(x = "visit", y = "severity") +
  coord_cartesian(ylim = c(3, 7)) +
  book_theme +
  caps_axes

save_fig(fig7_5, "Figure 7.5", width = 11, height = 8.5)

#------------------------------------------------------------------------------#
# FIGURE 7.6: RESIDUAL PLOT ----
#------------------------------------------------------------------------------#

rp <- residuals_plot(
  mod2,
  point_color  = "grey50",
  curve_color  = "black",
  font_size    = 18,
  line_width   = 0.6,
  label_family = "Minion Pro"
)

fig7_6 <- rp$severity.visit +
  book_theme &
  caps_axes &
  labs(title = NULL)

save_fig(fig7_6, "Figure 7.6", width = 11, height = 8.5)
