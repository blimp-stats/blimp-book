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

#------------------------------------------------------------------------------#
# PLOT MEANS ----
#------------------------------------------------------------------------------#

ggplot(medtrial, aes(x = visit, y = severity, color = factor(drug), group = drug)) +
  stat_summary(fun = mean, geom = "line",  na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE)

#------------------------------------------------------------------------------#
# LINEAR GROWTH MODEL WITH GROUP-BY-TIME INTERACTION ----
#------------------------------------------------------------------------------#

# basic model
mod1 <- rblimp(
  data = medtrial,
  clusterid = 'person',
  ordinal = 'drug male',
  center = 'grandmean = male',
  model = 'severity ~ visit drug visit*drug male  | visit; DEBUG: compact_output',
  simple = 'visit | drug',
  seed = 90291,
  burn = 25000,
  iter = 25000)

# print output
output(mod1)

simple_plot(severity ~ visit | drug, mod1)

# model with endpoint mean difference
mod1 <- rblimp(
  data = medtrial,
  clusterid = 'person',
  ordinal = 'drug male',
  model = 'severity ~ intercept@b0 visit@b1 drug@b2 male@b3 visit*drug@b4 | visit; DEBUG: compact_output',
  simple = 'visit | drug',
  parameters = '
    maleprob = .458;
    drug0 = b0 + b1*3 + b3*maleprob;
    drug1 = (b0 + b2) + (b1 + b4)*3 + b3*maleprob;
    meandiff = drug1 - drug0;',
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
  ordinal = 'drug male',
  model = 'severity ~ intercept visit drug visit*drug male  | visit; DEBUG: compact_output',
  simple = 'visit | drug',
  seed = 90291,
  burn = 25000,
  iter = 25000,
  nimps = 20)

distribution_plot(mod2)
residuals_plot(mod2)

#------------------------------------------------------------------------------#
#  LINEAR GROWTH MODEL WITH GROUP-BY-TIME INTERACTION (LATENT SPECIFICATION) ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = medtrial,
  clusterid = 'person',
  ordinal = 'drug male',
  latent = 'person = b0j b1j',
  model = '
    level2:
    b0j ~ intercept male drug;
    b1j ~ intercept drug;
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
# FIGURE 7.7 ----
#------------------------------------------------------------------------------#

# make the plots
fig7_7a <-
  ggplot(medtrial, aes(x = visit, y = severity,
                       linetype = factor(drug), group = drug)) +
  stat_summary(fun = mean, geom = "line",  na.rm = TRUE, color = "black") +
  stat_summary(fun = mean, geom = "point", na.rm = TRUE, color = "black", size = 2.5) +
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("Placebo", "Medication"),
                        name   = "Drug") +
  labs(x = "visit", y = "severity") +
  coord_cartesian(ylim = c(3, 7)) +
  book_theme +
  caps_axes

fig7_7b <- simple_plot(severity ~ visit | drug, mod1)


# linetype by moderator level on the line layers only
for (i in which(vapply(fig7_7b$layers,
                       function(l) inherits(l$geom, "GeomLine"), logical(1)))) {
  q <- fig7_7b$layers[[i]]$mapping[["colour"]]
  if (is.null(q)) q <- fig7_7b$mapping[["colour"]]
  fig7_7b$layers[[i]]$mapping[["linetype"]] <- q
}

# make the two CI ribbons a bit more visible
for (i in which(vapply(fig7_7b$layers,
                       function(l) inherits(l$geom, "GeomRibbon"), logical(1)))) {
  fig7_7b$layers[[i]]$aes_params$alpha <- 0.35
}

fig7_7b <- fig7_7b +
  scale_colour_manual(values = c("black", "black"), guide = "none") +
  scale_fill_manual(values = c("grey70", "grey30"), guide = "none") +
  scale_linetype_manual(
    values = c("dashed", "solid"),
    name   = "Drug",                       # <- match A exactly
    labels = c("Placebo", "Medication")    # <- match A exactly
  ) +
  coord_cartesian(ylim = c(3, 7)) +        # <- same limits as A
  labs(title = NULL, subtitle = NULL) +
  book_theme +
  caps_axes +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(colour = "black", linewidth = 0.5),
    legend.position  = "bottom"
  )


fig7_7 <-
  (fig7_7a / fig7_7b) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  book_theme &
  caps_axes &
  labs(title = NULL)

save_fig(fig7_7, "Figure 7.7", width = 8.5, height = 11)
