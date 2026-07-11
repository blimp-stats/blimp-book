# MULTIPLE MEDIATORS: PARALLEL MEDIATION

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
# PARALLEL MEDIATION MODEL ----
#------------------------------------------------------------------------------#

# unstandardized indirect effects
mod1 <- rblimp(
  data = discrimination,             		# R data frame
  ordinal = 'female',          			# binary and ordinal variables
  model = '  						# label slope parameters
    victim ~ discrim@a1v female age; 		# a path
    familism ~ discrim@a1f female age; 	# a path
    victim ~~ familism;				# correlated residual
    internalize ~ victim@b1 familism@b2 discrim age female;',  # b path
  parameters = '
    indirect_vic = a1v * b1; 			# compute indirect effect
    indirect_fam = a1f * b2;',			# compute indirect effect
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000                			# analysis iterations
)

output(mod1)               		      # print output
posterior_plot(mod1, 'indirect_vic')       	# plot indirect effect
posterior_plot(mod1, 'indirect_fam')       	# plot indirect effect

# add standardized indirect effects
mod2 <- rblimp(
  data = discrimination,             		# R data frame
  ordinal = 'female',          			# binary and ordinal variables
  model = '  						# label slope parameters
    mediation: 						# model block label
    victim ~ discrim@a1v female age; 		# a path
    familism ~ discrim@a1f female age; 		# a path
    victim ~~ familism;				# correlated residual
    internalize ~ victim@b1 familism@b2 discrim age female;  # b path
    predictors: 						# model block label
    discrim ~ age female; DEBUG: compact_output',   				# predictor model
  parameters = '
    indirect_vic = a1v * b1;				# compute indirect effect
    indirect_fam = a1f * b2;				# compute indirect effect
    sd_ratio = sqrt(discrim.totalvar / internalize.totalvar);  # sd ratio
    ind_std_vic = indirect_vic * sd_ratio;	# standardized indirect
    ind_std_fam = indirect_fam * sd_ratio;',	# standardized indirect
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000                			# analysis iterations
)

output(mod2)               		      # print output
posterior_plot(mod2, 'indirect_vic')       	# plot indirect effect
posterior_plot(mod2, 'indirect_fam')       	# plot indirect effect
posterior_plot(mod2, 'ind_std_vic')       	# plot indirect effect
posterior_plot(mod2, 'ind_std_fam')       	# plot indirect effect

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = discrimination,             		# R data frame
  ordinal = 'female',          			# binary and ordinal variables
  model = '  						# label slope parameters
    victim ~ discrim@a1v female age; 		# a path
    familism ~ discrim@a1f female age; 	# a path
    victim ~~ familism;				# correlated residual
    internalize ~ victim@b1 familism@b2 discrim age female;',  # b path
  parameters = '
    indirect_vic = a1v * b1; 			# compute indirect effect
    indirect_fam = a1f * b2;',			# compute indirect effect
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000,                			# analysis iterations
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

fig5_7a <- posterior_plot(mod1, 'indirect_vic', line_width = 0.5) + panel_format +
  labs(x = "Indirect Effect (Victimization)") +
  scale_x_continuous(breaks = seq(0, 0.3, 0.1)) +
  coord_cartesian(xlim = c(0, 0.30))

fig5_7b <- posterior_plot(mod1, 'indirect_fam', line_width = 0.5) + panel_format +
  labs(x = "Indirect Effect (Familism)") +
  scale_x_continuous(breaks = seq(.1, 0.40, 0.1)) +
  coord_cartesian(xlim = c(.1, 0.4))

fig5_7 <- (fig5_7a / fig5_7b) +
  plot_annotation(tag_levels = "A") &
  book_theme &
  labs(title = NULL) &
  theme(legend.position = "none")

save_fig(fig5_7, "Figure 5.7", width = 8.5, height = 11)


