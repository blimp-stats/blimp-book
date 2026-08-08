# LATENT CURVE MODELS

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/schiztrial.csv'

# create data frame from github data
schiztrial <- read.csv(data_url)

#------------------------------------------------------------------------------#
# PLOT MEANS ----
#------------------------------------------------------------------------------#

plotdat <- reshape(
  schiztrial,
  varying = c("severity0", "severity1", "severity2", "severity3"),
  v.names = "severity",
  timevar = "visit",
  times = 0:3,
  direction = "long"
)

ggplot(plotdat, aes(x = visit, y = severity)) +
  stat_summary(fun = mean, geom = "line",  na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE)

#------------------------------------------------------------------------------#
# LINEAR GROWTH MODEL ----
#------------------------------------------------------------------------------#

# full specification
mod1 <- rblimp(
  data = schiztrial,                             # R data frame
  latent = 'baseline linear',                    # define latent variables
  model = '
    structural:                                  # model block label
    baseline ~ intercept;                        # estimate the latent mean
    linear ~ intercept;                          # estimate the latent mean
    baseline ~~ linear;                          # factor correlation
    measurement:                                 # model block label
    severity0 ~ intercept@0 baseline@1 linear@0; # @ fixes parameters
    severity1 ~ intercept@0 baseline@1 linear@1; # @ fixes parameters
    severity2 ~ intercept@0 baseline@1 linear@2; # @ fixes parameters
    severity3 ~ intercept@0 baseline@1 linear@3; # @ fixes parameters
    severity0:severity3@resvar;',                # set residual variances equal
  seed = 90291,                                  # random number seed
  burn = 25000,                                  # warm-up iterations
  iter = 25000                                   # analysis iterations
)

output(mod1)                                     # print output

# simplified specification
mod2 <- rblimp(
  data = schiztrial,                             # R data frame
  latent = 'baseline linear',                    # define latent variables
  model = '
    structural:
    intercept -> baseline linear;
    baseline ~~ linear;
    measurement:
    baseline -> severity0@1 severity1@1 severity2@1 severity3@1;
    linear -> severity0@0 severity1@1 severity2@2 severity3@3;
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    severity0:severity3@resvar;',
  seed = 90291,                                  # random number seed
  burn = 25000,                                  # warm-up iterations
  iter = 25000)                                  # analysis iterations

output(mod2)                                     # print output

posterior_plot(mod2)                             # plot parameter distributions

#------------------------------------------------------------------------------#
# PLOT TRAJECTORY ----
#------------------------------------------------------------------------------#

# reshape singly-imputed data
plottraj <- reshape(
  mod2@average_imp,
  varying = c("severity0.predicted", "severity1.predicted", "severity2.predicted", "severity3.predicted"),
  v.names = "severity.predicted",
  timevar = "visit",
  times = 0:3,
  direction = "long"
)

# plot average predicted values across mcmc iterations by time
ggplot(plottraj, aes(x = visit, y = severity.predicted)) +
  stat_summary(fun = mean, geom = "line",  na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE)

#------------------------------------------------------------------------------#
# LINEAR MODEL WITH PREDICTORS ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = schiztrial,                             # R data frame
  latent = 'baseline linear',                    # define latent variables
  model = '
    structural:
    baseline ~ intercept drug male;
    linear ~ intercept drug;
    baseline ~~ linear;
    measurement:
    baseline -> severity0@1 severity1@1 severity2@1 severity3@1;
    linear -> severity0@0 severity1@1 severity2@2 severity3@3;
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    severity0:severity3@resvar;',
  seed = 90291,                                  # random number seed
  burn = 25000,                                  # warm-up iterations
  iter = 25000)                                  # analysis iterations

output(mod3)                                     # print output

#------------------------------------------------------------------------------#
# PLOT TRAJECTORIES ----
#------------------------------------------------------------------------------#

# reshape singly-imputed data
plottrajtx <- reshape(
  mod3@average_imp,
  varying = c("severity0.predicted", "severity1.predicted", "severity2.predicted", "severity3.predicted"),
  v.names = "severity.predicted",
  timevar = "visit",
  times = 0:3,
  direction = "long"
)

# plot average predicted values across mcmc iterations by time
ggplot(plottrajtx, aes(x = visit, y = severity.predicted, color = factor(drug), group = drug)) +
  stat_summary(fun = mean, geom = "line",  na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod4 <- rblimp(
  data = schiztrial,                             # R data frame
  latent = 'baseline linear',                    # define latent variables
  model = '
    structural:
    intercept -> baseline linear;
    baseline ~~ linear;
    measurement:
    baseline -> severity0@1 severity1@1 severity2@1 severity3@1;
    linear -> severity0@0 severity1@1 severity2@2 severity3@3;
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    severity0:severity3@resvar;',
  seed = 90291,                                  # random number seed
  burn = 25000,                                  # warm-up iterations
  iter = 25000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod4)                                     # print output

distribution_plot(mod4)                          # plot observed and imputed distributions
residuals_plot(mod4)                             # plot residuals

# save distribution plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/6.9 Distribution Plot.pdf", width = 8.5, height = 11)
plots <- distribution_plot(mod5)                 # plot observed and imputed distributions
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

# save residual plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/6.9 Residuals Plot.pdf", width = 8.5, height = 11)
plots <- residuals_plot(mod5)                    # plot residuals
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

#------------------------------------------------------------------------------#
# QUADRATIC GROWTH MODEL ----
#------------------------------------------------------------------------------#

mod5 <- rblimp(
  data = schiztrial,                             # R data frame
  latent = 'baseline linear quad',               # define latent variables
  model = '
    structural:
    intercept -> baseline linear quad;
    baseline linear quad ~~ baseline linear quad;
    measurement:
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    baseline -> severity0@1 severity1@1 severity2@1 severity3@1;
    linear -> severity0@0 severity1@1 severity2@2 severity3@3;
    quad -> severity0@0 severity1@1 severity2@4 severity3@9;
    severity0:severity3@resvar;',
  seed = 90291,                                  # random number seed
  burn = 30000,                                  # warm-up iterations
  iter = 30000)                                  # analysis iterations

output(mod5)                                     # print output

#------------------------------------------------------------------------------#
# LATENT BASIS GROWTH MODEL ----
#------------------------------------------------------------------------------#

mod6 <- rblimp(
  data = schiztrial,                             # R data frame
  latent = 'baseline growth',               # define latent variables
  model = '
    structural:
    intercept -> baseline growth;
    baseline ~~ growth;
    measurement:
    intercept -> severity0@0 severity1@0 severity2@0 severity3@0;
    baseline -> severity0@1 severity1@1 severity2@1 severity3@1;
    growth -> severity0@0 severity1 severity2 severity3@1;
    severity0:severity3@resvar;',
  seed = 90291,                                  # random number seed
  burn = 30000,                                  # warm-up iterations
  iter = 30000)                                  # analysis iterations

output(mod6)                                     # print output

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
# FIGURE 6.11 ----
#------------------------------------------------------------------------------#

# make the plots

fig6_11a <- ggplot() +
  # observed means = points connected by a dashed line
  stat_summary(data = plotdat, aes(visit, severity),
               fun = mean, geom = "line",  na.rm = TRUE, linetype = "dashed") +
  stat_summary(data = plotdat, aes(visit, severity),
               fun = mean, geom = "point", na.rm = TRUE, size = 2.5) +
  # predicted trajectory = solid line
  stat_summary(data = plottraj, aes(visit, severity.predicted),
               fun = mean, geom = "line",  na.rm = TRUE) +
  coord_cartesian(ylim = c(3.5, 7.5)) +
  labs(x = "visit", y = "severity") +
  book_theme + caps_axes

fig6_11b <- ggplot(plotdat, aes(x = visit, y = severity,
                                linetype = factor(drug), group = drug)) +
  # observed means: two different dash patterns
  stat_summary(fun = mean, geom = "line",  na.rm = TRUE, color = "black") +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE, color = "black", size = 2.5) +
  # predicted trajectories: both solid (constant linetype overrides the mapping)
  stat_summary(data = plottrajtx, aes(y = severity.predicted),
               fun = mean, geom = "line", na.rm = TRUE, linetype = "solid") +
  scale_linetype_manual(values = c("dashed", "dotted"),
                        labels = c("Placebo", "Medication"), name = "DRUG") +
  labs(x = "visit", y = "severity") +
  coord_cartesian(ylim = c(3.5, 7.5)) +
  book_theme + caps_axes

fig6_11 <-
  (fig6_11a / fig6_11b) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  book_theme &
  caps_axes &
  labs(title = NULL)

save_fig(fig6_11, "Figure 6.11", width = 8.5, height = 11)
