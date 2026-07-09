# SINGLE-MEDIATOR MODEL WITH COVARIATES

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/discrimination.csv'

# create data frame from github data
discrimination <- read.csv(data_url)

#------------------------------------------------------------------------------#
# SINGLE-MEDIATOR MODEL ----
#------------------------------------------------------------------------------#

# basic model
mod1 <- rblimp(
  data = discrimination,  # R data frame
  ordinal = 'female', # binary and ordinal variables
  model = '
    victim ~ discrim@a1 age female; 		# label slope
    internalize ~ victim@b1 discrim age female; DEBUG: compact_output',  # label slope
  parameters = 'indirect = a1 * b1',				     # compute indirect effect
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod1)                                     # print output
posterior_plot(mod1)                             # plot parameter distributions
posterior_plot(mod1, 'indirect')                 # plot distribution of indirect effect

# add standardized indirect effect
mod2 <- rblimp(
  data = discrimination,  # R data frame
  ordinal = 'female', # binary and ordinal variables
  model = '
    apath:                                       # model block label
    victim ~ discrim@a1 age female; 		         # label slope
    bpath:                                       # model block label
    internalize ~ victim@b1 discrim age female;  # label slope
    predictors:                                  # model block label
    discrim ~ age female;',                      # predictor model
  parameters = '
    indirect = a1 * b1;				                   # compute indirect effect
    sd_ratio = sqrt(discrim.totalvar / internalize.totalvar);  # sd ratio 
    indirect_std = indirect * sd_ratio;', 		   # standardized indirect
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod2)                                     # print output
posterior_plot(mod2)                 # plot parameter distributions
posterior_plot(mod2, 'indirect') # plot distribution of indirect effect
posterior_plot(mod2, 'indirect_std') # plot distribution of indirect effect

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = discrimination,  # R data frame
  ordinal = 'female', # binary and ordinal variables
  model = '
    apath:                                       # model block label
    victim ~ discrim@a1 age female; 		         # label slope
    bpath:                                       # model block label
    internalize ~ victim@b1 discrim age female;  # label slope
    predictors:                                  # model block label
    discrim ~ age female;', # predictor model
  parameters = '
    indirect = a1 * b1;				                   # compute indirect effect
    sd_ratio = sqrt(discrim.totalvar / internalize.totalvar);  # sd ratio 
    indirect_std = indirect * sd_ratio;', 		   # standardized indirect
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,           # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

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
# FIGURE 5.2: POSTERIOR PLOT ----
#------------------------------------------------------------------------------#

panel_format <- list(
  scale_color_manual(values = "black"),
  labs(title = NULL, subtitle = NULL),
  book_theme,
  scale_fill_manual(values = NA),
  theme(legend.position = "none")
)

fig5_2a <- posterior_plot(mod3, 'indirect', line_width = 0.5) + panel_format +
  labs(x = "Indirect Effect") +
  scale_x_continuous(breaks = seq(0, 0.4, 0.1)) +
  coord_cartesian(xlim = c(0, 0.40))

fig5_2b <- posterior_plot(mod3, 'indirect_std', line_width = 0.5) + panel_format +
  labs(x = "Standardized Indirect Effect") +
  scale_x_continuous(breaks = seq(0, 0.15, 0.05)) +
  coord_cartesian(xlim = c(0, 0.15))

fig5_2 <- (fig5_2a / fig5_2b) +
  plot_annotation(tag_levels = "A") &
  book_theme &
  labs(title = NULL) &
  theme(legend.position = "none")

save_fig(fig5_2, "Figure 5.2", width = 8.5, height = 11)


