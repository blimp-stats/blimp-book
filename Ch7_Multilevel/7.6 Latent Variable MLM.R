# MULTILEVEL LATENT VARIABLE MODEL WITH RANDOM SLOPES

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
  data = diary,                                  # R data frame
  clusterid = 'person',                          # cluster-level identifier
  model = '{ posaff pain } ~ intercept',
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod0)                                     # print output

#------------------------------------------------------------------------------#
# MULTILEVEL LATENT VARIABLE SPECIFICATION ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = diary,                                  # R data frame
  clusterid = 'person',                          # cluster-level identifier
  latent = 'person = b0c b1c',                   # define latent variables
  center = '
    groupmean = pain;                            # center at latent group means
    grandmean = pain.mean;',                     # center at grand means
  model = '
    level2:                                      # model block label
    b0c ~ intercept pain.mean;                   # level-2 random intercept
    b1c ~ intercept;                             # level-2 random slope
    b0c ~~ b1c;                                  # random effect correlation
    level1:                                      # model block label
    posaff ~ intercept@b0c pain@b1c;',           # level-2 model
  parameters = '
    var_total = posaff.totalvar + b0c.totalvar;
    rsq_l2coeff = b0c.coefvar / var_total;
    rsq_l1coeff = posaff.coefvar / var_total;
    rsq_l2resid = b0c.residvar / var_total;
    rsq_l1resid = posaff.residvar / var_total;',
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000                                   # analysis iterations
)

output(mod1)                                     # print output

posterior_plot(mod1,'posaff')                    # plot parameter distributions

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = diary,                                  # R data frame
  clusterid = 'person',                          # cluster-level identifier
  latent = 'person = b0c b1c',                   # define latent variables
  center = '
    groupmean = pain;                            # center at latent group means
    grandmean = pain.mean;',                     # center at grand means
  model = '
    level2:                                      # model block label
    b0c ~ intercept pain.mean;                   # level-2 random intercept
    b1c ~ intercept;                             # level-2 random slope
    b0c ~~ b1c;                                  # random effect correlation
    level1:                                      # model block label
    posaff ~ intercept@b0c pain@b1c;',           # level-2 model
  parameters = '
    var_total = posaff.totalvar + b0c.totalvar;
    rsq_l2coeff = b0c.coefvar / var_total;
    rsq_l1coeff = posaff.coefvar / var_total;
    rsq_l2resid = b0c.residvar / var_total;
    rsq_l1resid = posaff.residvar / var_total;',
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20                                     # save 20 imputed data sets
)


output(mod2)                                     # print output

distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot binned residuals

# save distribution plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/7.6 Distribution Plot.pdf", width = 8.5, height = 11)
plots <- distribution_plot(mod2)                 # plot observed and imputed distributions
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

# save residual plots to pdf
pdf("/Users/craig/Documents/GitHub/blimp-book/run_logs/7.6 Residuals Plot.pdf", width = 8.5, height = 11)
plots <- residuals_plot(mod2)                    # plot binned residuals
for (p in plots) print(p)                        # print plots to pdf
dev.off()                                        # close pdf file

#------------------------------------------------------------------------------#
# MULTILEVEL LATENT VARIABLE MODEL WITH A CROSS-LEVEL INTERACTION ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = diary,                                  # R data frame
  clusterid = 'person',                          # cluster-level identifier
  ordinal = 'female',                            # binary and ordinal variables
  latent = 'person = b0c b1c',                   # define latent variables
  center = '
    groupmean = pain;                            # center at latent group means
    grandmean = pain.mean;',                     # center at grand means
  model = '
    level2:                                      # model block label
    b0c ~ intercept pain.mean female;            # level-2 random intercept
    b1c ~ intercept female;                      # level-2 random slope
    b0c ~~ b1c;                                  # random effect correlation
    level1:                                      # model block label
    posaff ~ intercept@b0c pain@b1c;
',                                               # level-2 model
  simple = 'pain | female',
  parameters = '
    var_total = posaff.totalvar + b0c.totalvar;
    rsq_l2coeff = b0c.coefvar / var_total;
    rsq_l1coeff = posaff.coefvar / var_total;
    rsq_l2resid = b0c.residvar / var_total;
    rsq_l1resid = posaff.residvar / var_total;',
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000                                   # analysis iterations
)

output(mod3)                                     # print output

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
# FIGURE 7.9: HETEROGENEITY PLOT ----
#------------------------------------------------------------------------------#

rp <- residuals_plot(
  mod2,
  point_color  = "grey40",
  curve_color  = "black",
  font_size    = 18,
  line_width   = 0.6,
  label_family = "Minion Pro"
)

fig7_9 <- rp$posaff.cluster_variance +
  book_theme &
  caps_axes &
  labs(title = NULL, x = "Person (Ordered by Residual SD)")

save_fig(fig7_9, "Figure 7.9", width = 11, height = 8.5)
