# MEDIATION WITH BINARY OUTCOME USING CONDITIONAL ESTIMATOR

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/worksat.csv'

# create data frame from github data
worksat <- read.csv(data_url)

#------------------------------------------------------------------------------#
# BINARY OUTCOME AND TOTAL NATURAL INDIRECT EFFECT ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = worksat,
  ordinal = 'turnover male suprelqual',
  center  = 'male',
  model = '
    jobsat ~ intercept@a0 suprelqual@a1 male;
    jobsat@resvar_m;
    turnover ~ intercept@b0 jobsat@b1 suprelqual@b2 male;',
  parameters = '
    sd_lat = sqrt(b1^2 * resvar_m + 1);
    p_obs = phi((b0 + b1*(a0 + a1*1) + b2*1) / sd_lat);
    p_cfact = phi((b0 + b1*(a0 + a1*0) + b2*1) / sd_lat);
    tnie    = p_obs - p_cfact;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(mod1)
posterior_plot(mod1, 'tnie')

#------------------------------------------------------------------------------#
# EXTENSION: CONTINUOUS EXPLANATORY VARIABLE ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = worksat,
  ordinal = 'turnover male',
  center  = 'male lmx',
  model = '
    mediation:
    jobsat ~ intercept@a0 lmx@a1 male;
    jobsat@resvar_m;
    turnover ~ intercept@b0 jobsat@b1 lmx@b2 male;
    predictor:
    lmx ~ male; DEBUG: compact_output',
  parameters = '
    sd_lmx = sqrt(lmx.totalvar);
    sd_lat = sqrt(b1^2 * resvar_m + 1);
    p_obs   = phi((b0 + b2*sd_lmx + b1*(a0 + a1*sd_lmx)) / sd_lat); 
    p_cfact = phi((b0 + b2*sd_lmx + b1*(a0 + a1*0)) / sd_lat); 
    tnie    = p_obs - p_cfact;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(mod2)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

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
# FIGURE 5.7: POSTERIOR PLOT ----
#------------------------------------------------------------------------------#

panel_format <- list(
  scale_color_manual(values = "black"),
  labs(title = NULL, subtitle = NULL),
  book_theme,
  scale_fill_manual(values = NA),
  theme(legend.position = "none"),
  scale_y_continuous(breaks = NULL)
)

mod3 <- rblimp(
  data = worksat,
  ordinal = 'turnover male suprelqual',
  center  = 'male',
  model = '
    jobsat ~ intercept@a0 suprelqual@a1 male;
    jobsat@resvar_m;
    turnover ~ intercept@b0 jobsat@b1 suprelqual@b2 male;',
  parameters = '
    sd_lat = sqrt(b1^2 * resvar_m + 1);
    p_obs = phi((b0 + b1*(a0 + a1*1) + b2*1) / sd_lat);
    p_cfact = phi((b0 + b1*(a0 + a1*0) + b2*1) / sd_lat);
    tnie    = p_obs - p_cfact;
    w0 = normal_pdf(b0 + b1*(a0 + a1*0) + b2*0);
    w1 = normal_pdf(b0 + b1*(a0 + a1*1) + b2*1);
    indirect0 = a1 * b1 * w0;    # conditional indirect effect at x = 0
    indirect1 = a1 * b1 * w1;',  # conditional indirect effect at x = 1
  seed = 90291,
  burn = 10000,
  iter = 10000)

fig5_9a <- posterior_plot(mod3, 'indirect0', line_width = 0.5) + panel_format +
  labs(x = "Conditional Indirect Effect (Not High Quality Relationship)") +
  scale_x_continuous(breaks = seq(-.2, 0, 0.05)) +
  coord_cartesian(xlim = c(-.20, 0))

fig5_9b <- posterior_plot(mod3, 'indirect1', line_width = 0.5) + panel_format +
  labs(x = "Conditional Indirect Effect (High Quality Relationship)") +
  scale_x_continuous(breaks = seq(-.2, 0, 0.05)) +
  coord_cartesian(xlim = c(-.20, 0))

fig5_9c <- posterior_plot(mod3, 'tnie', line_width = 0.5) + panel_format +
  labs(x = "Total Natural Indirect Effect") +
  scale_x_continuous(breaks = seq(-.2, 0, 0.05)) +
  coord_cartesian(xlim = c(-.20, 0))

fig5_9 <- (fig5_9a / fig5_9b / fig5_9c) +
  plot_annotation(tag_levels = "A") &
  book_theme &
  labs(title = NULL) &
  theme(legend.position = "none")

save_fig(fig5_9, "Figure 5.9", width = 8.5, height = 11)
