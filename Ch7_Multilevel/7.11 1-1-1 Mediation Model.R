# 1-1-1 MEDIATION 

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/diary.csv'

# create data frame from github data
diary <- read.csv(data_url)

#------------------------------------------------------------------------------#
# FIT EMPTY MODEL FOR LEVEL-1 VARIABLES ----
#------------------------------------------------------------------------------#

# empty multivariate model for icc's
mod0 <- rblimp(
  data = diary,
  clusterid = 'person',
  model = 'intercept -> sleep pain posaff',
  seed = 90291,
  burn = 10000,
  iter = 10000)
output(mod0)

#------------------------------------------------------------------------------#
# 1-1-1 MODEL (WITHIN ONLY) ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = diary,             			# R data frame
  clusterid = 'person',          		# cluster-level identifier
  latent = 'person = a0c a1c b0c b1c b2c',   # define latent variables
  center = 'groupmean = sleep',      		# center at latent group means       
  model = '
    level2: 						# model block label
    intercept -> a0c a1c@a1_mean b0c b1c@b1_mean b2c; # latent means
    a0c b0c  b2c a1c b1c ~~ a0c b0c b2c;  # correlations
    a1c ~~ b1c@a1b1_cor; 				# random slope correlation
    level1: 						# model block label
    pain ~ intercept@a0c sleep@a1c; 			# level-1 model
    posaff ~ intercept@b0c (pain - a0c)@b1c sleep@b2c;
    DEBUG: compact',  # level-1 model
  parameters =  '
    a1b1_cov = a1b1_cor * sqrt(a1c.totalvar * b1c.totalvar);  # covariance
    indirect = a1_mean * b1_mean + a1b1_cov;',  # compute indirect effect
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000                			# analysis iterations
)

output(mod1)               		      # print output
posterior_plot(mod1, 'indirect')             # plot indirect effect

#------------------------------------------------------------------------------#
# 1-1-1 MODEL (WITHIN + BETWEEN) ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = diary,             			# R data frame
  clusterid = 'person',          		# cluster-level identifier
  latent = 'person = a0c a1c b0c b1c b2c',   # define latent variables
  center = '
    groupmean = sleep;
    grandmean = sleep.mean;',      		# center at latent group means       
  model = '
    level2: 						# model block label
    intercept -> a0c a1c@a1_mean b0c b1c@b1_mean b2c; # latent means
    a0c ~ sleep.mean@a2;
    b0c ~ a0c@b3 sleep.mean;
    a1c ~~ b1c@a1b1_cor; 				# random slope correlation
    level1: 						# model block label
    pain ~ intercept@a0c sleep@a1c; 			# level-1 model
    posaff ~ intercept@b0c (pain - a0c)@b1c sleep@b2c;
    DEBUG: compact;
  ',  # level-1 model
  parameters =  '
    a1b1_cov = a1b1_cor * sqrt(a1c.totalvar * b1c.totalvar);  # covariance
    indirect_w = a1_mean * b1_mean + a1b1_cov; # compute indirect effect
    indirect_b = a2 * b3;',  # compute indirect effect
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000                			# analysis iterations
)

output(mod2)               		      # print output
posterior_plot(mod2, 'indirect_w')             # plot indirect effect
posterior_plot(mod2, 'indirect_b')             # plot indirect effect

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
# FIGURE 5.2: POSTERIOR PLOT ----
#------------------------------------------------------------------------------#

panel_format <- list(
  scale_color_manual(values = "black"),
  labs(title = NULL, subtitle = NULL),
  book_theme,
  scale_fill_manual(values = NA),
  theme(legend.position = "none"),
  scale_y_continuous(breaks = NULL)
)

fig7_11a <- posterior_plot(mod2, 'indirect_w', line_width = 0.5) + panel_format +
  labs(x = "Indirect Effect (Within-Person)") +
  scale_x_continuous(breaks = seq(-.01, 0.04, 0.01)) +
  coord_cartesian(xlim = c(-.01, 0.04))

fig7_11b <- posterior_plot(mod2, 'indirect_b', line_width = 0.5) + panel_format +
  labs(x = "Indirect Effect (Between-Person)") +
  scale_x_continuous(breaks = seq(-.10, 0.10, 0.05)) +
  coord_cartesian(xlim = c(-.10, 0.10))

fig7_11 <- (fig7_11a / fig7_11b) +
  plot_annotation(tag_levels = "A") &
  book_theme &
  labs(title = NULL) &
  theme(legend.position = "none")

save_fig(fig7_11, "Figure 7.11", width = 8.5, height = 11)
