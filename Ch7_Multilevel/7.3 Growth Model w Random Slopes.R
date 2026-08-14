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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/medtriallong.csv'

# create data frame from github data
medtrial <- read.csv(data_url)

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
  data = medtrial,                               # R data frame
  clusterid = 'person',                          # cluster-level identifier
  model = 'severity ~ intercept',
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod0)                                     # print output

#------------------------------------------------------------------------------#
# LINEAR GROWTH MODEL ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = medtrial,                               # R data frame
  clusterid = 'person',                          # cluster-level identifier
  model = 'severity ~ intercept visit | visit;',
  seed = 90291,                                  # random number seed
  burn = 25000,                                  # warm-up iterations
  iter = 25000)                                  # analysis iterations

output(mod1)                                     # print output

posterior_plot(mod1)                             # plot parameter distributions

#------------------------------------------------------------------------------#
# PLOT TRAJECTORY ----
#------------------------------------------------------------------------------#

# plot average predicted values across mcmc iterations by time
ggplot(mod1@average_imp, aes(x = visit, y = severity.predicted)) +
  stat_summary(fun = mean, geom = "line",  na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = medtrial,                               # R data frame
  clusterid = 'person',                          # cluster-level identifier
  model = 'severity ~ intercept visit | visit',
  seed = 90291,                                  # random number seed
  burn = 25000,                                  # warm-up iterations
  iter = 25000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod2)                                     # print output

distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot residuals

# save distribution plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/7.3 Distribution Plot.pdf", width = 8.5, height = 11)
plots <- distribution_plot(mod2)                 # plot observed and imputed distributions
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

# save residual plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/7.3 Residuals Plot.pdf", width = 8.5, height = 11)
plots <- residuals_plot(mod2)                    # plot residuals
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

#------------------------------------------------------------------------------#
# QUADRATIC GROWTH MODEL ----
#------------------------------------------------------------------------------#

# fixed quadratic effect
mod3 <- rblimp(
  data = medtrial,                               # R data frame
  clusterid = 'person',                          # cluster-level identifier
  model = 'severity ~ intercept visit visit^2 | visit;',
  seed = 90291,                                  # random number seed
  burn = 25000,                                  # warm-up iterations
  iter = 25000)                                  # analysis iterations

output(mod3)                                     # print output

# random quadratic effect
mod4 <- rblimp(
  data = medtrial,                               # R data frame
  clusterid = 'person',                          # cluster-level identifier
  model = 'severity ~ intercept visit visit^2 | visit visit^2',
  seed = 90291,                                  # random number seed
  burn = 30000,                                  # warm-up iterations
  iter = 30000)                                  # analysis iterations

output(mod4)                                     # print output

#------------------------------------------------------------------------------#
# LINEAR GROWTH MODEL (LATENT SPECIFICATION) ----
#------------------------------------------------------------------------------#

mod5 <- rblimp(
  data = medtrial,                               # R data frame
  clusterid = 'person',                          # cluster-level identifier
  latent = 'person = b0j b1j',                   # define latent variables
  model = '
    level2:
    b0j ~ intercept;
    b1j ~ intercept;
    b0j ~~ b1j;
    level1:
    severity ~ intercept@b0j visit@b1j;',
  seed = 90291,                                  # random number seed
  burn = 20000,                                  # warm-up iterations
  iter = 20000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod5)                                     # print output

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

fig7_5 <- ggplot() +
  # observed means = points connected by a dashed line
  stat_summary(data = medtrial, aes(visit, severity),
               fun = mean, geom = "line",  na.rm = TRUE, linetype = "dashed") +
  stat_summary(data = medtrial, aes(visit, severity),
               fun = mean, geom = "point", na.rm = TRUE, size = 2.5) +
  # predicted trajectory = solid line
  stat_summary(data = mod1@average_imp, aes(visit, severity.predicted),
               fun = mean, geom = "line",  na.rm = TRUE) +
  coord_cartesian(ylim = c(3.5, 7.5)) +
  labs(x = "visit", y = "severity") +
  book_theme + caps_axes

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
