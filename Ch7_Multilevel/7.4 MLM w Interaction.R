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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/trial_stacked.csv'

# create data frame from github data
trial <- read.csv(data_url)

#------------------------------------------------------------------------------#
# PLOT MEANS ----
#------------------------------------------------------------------------------#

ggplot(trial, aes(x = visit, y = severity, color = drug, group = drug)) +
  stat_summary(fun = mean, geom = "line",  na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE)

#------------------------------------------------------------------------------#
# LINEAR GROWTH MODEL ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = trial,
  clusterid = 'person',
  model = 'severity ~ intercept visit | visit;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(mod1)

# plot parameter distributions
posterior_plot(mod1)

#------------------------------------------------------------------------------#
# LINEAR GROWTH MODEL WITH PREDICTORS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'person',
  center = 'grandmean = male',
  model = 'severity ~ intercept@icept_d0 visit@slp_d0 drug@icept_diff drug*visit@slp_diff male | visit',
  simple = 'visit | drug',
  parameters = '
    mu3_drug0 = icept_d0 + slp_d0*3;
    mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(mod2)

# plot parameter distributions
posterior_plot(mod2)

# plot conditional growth curves
simple_plot(severity ~ visit | drug, mod2)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#





#------------------------------------------------------------------------------#
# FIT LINEAR GROWTH MODEL (LATENT SPECIFICATION) ----
#------------------------------------------------------------------------------#

# linear growth model
model2 <- rblimp(
  data = trial,
  clusterid = 'person',
  latent = 'person = icept linear',
  model = '
    level2:
    intercept -> icept linear;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept visit@linear;',
  seed = 90291,
  burn = 20000,
  iter = 20000,
  nimps = 20)

# print output
output(model2)

# linear growth model with predictors
model4 <- rblimp(
  data = trial,
  ordinal = 'male drug',
  clusterid = 'person',
  center = 'grandmean = male',
  latent = 'person = icept linear;',
  model = '
    level2:
    icept ~ intercept@icept_d0 drug@icept_diff male;
    linear ~ intercept@slp_d0 drug@slp_diff;
    icept ~~ linear;
    level1:
    severity ~ intercept@icept visit@linear;',
  parameters = '
    mu3_drug0 = icept_d0 + slp_d0*3;
    mu3_drug1 = (icept_d0 + icept_diff) + (slp_d0 + slp_diff)*3;
    mu3_diff = mu3_drug1 - mu3_drug0;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model4)

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

fig7_5 <- ggplot(trial, aes(x = visit, y = severity)) +
  stat_summary(fun = mean, geom = "line",  na.rm = TRUE, color = "black") +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE, color = "black", size = 2.5) +
  labs(x = "visit", y = "severity") +
  coord_cartesian(ylim = c(3, 7)) +
  book_theme +
  caps_axes


fig7_5 <- ggplot(trial,
                 aes(x = visit, y = severity, linetype = factor(drug), group = drug)) +
  stat_summary(fun = mean, geom = "line",  na.rm = TRUE, color = "black") +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE, color = "black", size = 2.5) +
  scale_linetype_manual(name   = "Drug",
                        values = c("solid", "dashed"),
                        labels = c("Placebo", "Drug")) +
  labs(x = "visit", y = "severity") +
  coord_cartesian(ylim = c(3, 7)) +
  book_theme +
  caps_axes

save_fig(fig7_5, "Figure 7.5", width = 11, height = 8.5)
