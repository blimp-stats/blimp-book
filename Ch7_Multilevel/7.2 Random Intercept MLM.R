# RANDOM INTERCEPT REGRESSION

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/mathprobsolve.csv'

# create data frame from github data
mathprobsolve <- read.csv(data_url)

#------------------------------------------------------------------------------#
# RANDOM INTERCEPT MODEL ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = mathprobsolve,             		# R data frame
  clusterid = 'school',          		# cluster-level identifier
  ordinal = 'hispanic intschool',          		# binary and ordinal variables
  center = '
    groupmean = stanmath hispanic;		# center at latent group means
    grandmean = stanmath.mean hispanic.mean;',  # center at grand means
  model = 'probsolve ~ stanmath hispanic stanmath.mean
    hispanic.mean intschool | intercept;',	# combined model
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000                			# analysis iterations
)

output(mod1)

#------------------------------------------------------------------------------#
# HETEROGENEOUS WITHIN-CLUSTER VARIATION MODEL ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = mathprobsolve,             		# R data frame
  clusterid = 'school',          		# cluster-level identifier
  ordinal = 'hispanic intschool',          		# binary and ordinal variables
  center = '
    groupmean = stanmath hispanic;		# center at latent group means
    grandmean = stanmath.mean hispanic.mean;',  # center at grand means
  model = 'probsolve ~ stanmath hispanic stanmath.mean
    hispanic.mean intschool | intercept; DEBUG: compact_output',	# combined model
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000,                			# analysis iterations
  options = 'hev'
)

output(mod2)

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = mathprobsolve,             		# R data frame
  clusterid = 'school',          		# cluster-level identifier
  ordinal = 'hispanic intschool',          		# binary and ordinal variables
  center = '
    groupmean = stanmath hispanic;		# center at latent group means
    grandmean = stanmath.mean hispanic.mean;',  # center at grand means
  model = 'probsolve ~ stanmath hispanic stanmath.mean
    hispanic.mean intschool | intercept;',	# combined model
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000,                			# analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod3)                                     # print output

distribution_plot(mod3)                          # plot observed and imputed distributions
residuals_plot(mod3)                             # plot residuals


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
# FIGURE 7.3: DISTRIBUTIONS ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(
  mod3,
  observed_color = "grey60",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 18,
  line_width     = 0.5
)

fig7_3 <- (
  (dp$probsolve.residual / dp$probsolve.residual.qq) |
  (dp$`probsolve[school]` / dp$`probsolve[school].qq`)) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  book_theme &
  caps_axes &
  labs(title = NULL)

save_fig(fig7_3, "Figure 7.3", width = 11, height = 8.5)

#------------------------------------------------------------------------------#
# FIGURE 7.4: HETEROGENEITY PLOT ----
#------------------------------------------------------------------------------#

rp <- residuals_plot(
  mod3,
  point_color  = "grey40",
  curve_color  = "black",
  font_size    = 18,
  line_width   = 0.6,
  label_family = "Minion Pro"
)

fig7_4 <- rp$probsolve.cluster_variance +
  book_theme &
  caps_axes &
  labs(title = NULL, x = "Cluster (Ordered by Residual SD)")

save_fig(fig7_4, "Figure 7.4", width = 11, height = 8.5)

