# MEDIATION MODEL WITH MODERATED PATHWAYS

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
# MODERATOR OF A PATH ----
#------------------------------------------------------------------------------#

# conditional mediated effects computed using parameters command
mod1 <- rblimp(
  data = discrimination,
  transform = 'age16 = age - 16',
  model = '
    apath:
    victim ~ discrim@a1 age16 discrim*age16@a3 female;
    bpath:
    internalize ~ victim@b1 discrim age16 female; DEBUG: compact_output;',
  simple = 'discrim | age16 @ quartile',
  parameters = '
    index_mod = a3*b1;
    indirect14 = (a1 - 2*a3)*b1;
    indirect15 = (a1 - 1*a3)*b1;
    indirect16 = (a1 + 0*a3)*b1;
    indirect17 = (a1 + 1*a3)*b1',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(mod1)

# plot conditional effects
simple_plot(victim ~ discrim | age16, mod1)

# plot distribution of indirect effects
posterior_plot(mod1, 'indirect14')
posterior_plot(mod1, 'indirect15')
posterior_plot(mod1, 'indirect16')
posterior_plot(mod1, 'indirect17')

# conditional mediated effects computed using simple command
mod2 <- rblimp(
  data = discrimination,
  transform = 'age16 = age - 16',
  model = '
    apath:
    victim ~ discrim@a1 age16 discrim*age16@a3 female;
    bpath:
    internalize ~ victim@b1 discrim age16 female; DEBUG: compact_output;',
    simple = '(a1 + a3*age16)*b1 | age16 @ quartile',
  parameters = 'index_mod = a3*b1',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(mod2)

# plot conditional effects
# simple_plot(victim ~ discrim | age16, mod2)

#------------------------------------------------------------------------------#
# MODERATOR OF B PATH ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = discrimination,             		# R data frame
  ordinal = 'female',          			# binary and ordinal variables
  transform = 'age16 = age – 16',  		# centered age variable
  model = '  						# label slope parameters
  victim ~ discrim@a1 age16 female;  		# a path
  internalize ~ victim@b1 discrim age16 victim*age16@b4 female; DEBUG: compact_output',  # bpath 
  simple = 'victim | age16 @ quartile', 	# conditional effects
  parameters = '
    index_mod = a1*b4; 				# index of mediated moderation
    indirect14 = a1*(b1 - 2*b4); 		# conditional indirect effect
    indirect15 = a1*(b1 - 1*b4); 		# conditional indirect effect
    indirect16 = a1*(b1 + 0*b4); 		# conditional indirect effect
    indirect17 = a1*(b1 + 1*b4);', 		# conditional indirect effect
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000                			# analysis iterations
)

output(mod3)               		      # print output
posterior_plot(mod3, 'indirect14') 		# plot conditional indirect
posterior_plot(mod3, 'indirect15') 		# plot conditional indirect
posterior_plot(mod3, 'indirect16') 		# plot conditional indirect
posterior_plot(mod3, 'indirect17') 		# plot conditional indirect

#------------------------------------------------------------------------------#
# MODERATOR OF A AND B PATH ----
#------------------------------------------------------------------------------#

mod4 <- rblimp(
  data = discrimination,             		# R data frame
  ordinal = 'female',          			# binary and ordinal variables
  transform = 'age16 = age – 16',  		# centered age variable
  model = '    					# label slope parameters
    victim ~ discrim@a1 age16 discrim*age16@a3 female;  # a path
    internalize ~ victim@b1 discrim age16 victim*age16@b4 female; DEBUG: compact_output', # b path 
  parameters = '
    indirect14 = (a1 - 2*a3)*(b1 - 2*b4); 	# conditional indirect effect
    indirect15 = (a1 - 1*a3)*(b1 - 1*b4);	# conditional indirect effect
    indirect16 = (a1 + 0*a3)*(b1 + 0*b4);	# conditional indirect effect
    indirect17 = (a1 + 1*a3)*(b1 + 1*b4);',	# conditional indirect effect
  seed = 90291,               			# random number seed
  burn = 10000,               			# warm-up iterations
  iter = 10000                			# analysis iterations
)

output(mod4)               		      # print output
posterior_plot(mod4, 'indirect14') 		# plot conditional indirect
posterior_plot(mod4, 'indirect15') 		# plot conditional indirect
posterior_plot(mod4, 'indirect16') 		# plot conditional indirect
posterior_plot(mod4, 'indirect17') 		# plot conditional indirect



#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# modx <- rblimp(
# 
#   nimps = 20)                                    # save 20 imputed data sets
# 
# distribution_plot(modx)                          # plot observed and imputed distributions
# residuals_plot(modx)                             # plot residuals

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
# FIGURE 5.4: INDIRECT PLOT ----
#------------------------------------------------------------------------------#

panel_format <- list(
  scale_color_manual(values = "black"),
  labs(title = NULL, subtitle = NULL),
  book_theme,
  scale_fill_manual(values = NA),
  theme(legend.position = "none"),
  scale_y_continuous(breaks = NULL)
)

fig5_4a <- posterior_plot(mod1, 'indirect14',  line_width = 0.6) + panel_format + 
  labs(x = "Indirect Effect at Age 14)") +
  scale_x_continuous(breaks = seq(-.10, 0.6, 0.1)) +
  coord_cartesian(xlim = c(-.10, 0.60))
fig5_4b <- posterior_plot(mod1, 'indirect15',  line_width = 0.6) + panel_format + 
  labs(x = "Indirect Effect at Age 15") +
  scale_x_continuous(breaks = seq(-.10, 0.6, 0.1)) +
  coord_cartesian(xlim = c(-.10, 0.60))
fig5_4c <- posterior_plot(mod1, 'indirect16', line_width = 0.6) + panel_format +
  labs(x = "Indirect Effect at Age 16") +
  scale_x_continuous(breaks = seq(-.10, 0.6, 0.1)) +
  coord_cartesian(xlim = c(-.10, 0.60))
fig5_4d <- posterior_plot(mod1, 'indirect17', line_width = 0.6) + panel_format +
  labs(x = "Indirect Effect at Age 17") +
  scale_x_continuous(breaks = seq(-.10, 0.6, 0.1)) +
  coord_cartesian(xlim = c(-.10, 0.60))
fig5_4e <- posterior_plot(mod1, 'index_mod', line_width = 0.6) + panel_format + 
  labs(x = "Index of Moderated Mediation") +
  scale_x_continuous(breaks = round(seq(-.25, 0.05, 0.05),2)) +
  coord_cartesian(xlim = c(-.25, 0.05))

fig5_4 <- (fig5_4a / fig5_4b / fig5_4c / fig5_4d / fig5_4e) +
  plot_annotation(tag_levels = "A") &
  book_theme &
  labs(title = NULL) &
  theme(legend.position = "none")

save_fig(fig5_4, "Figure 5.4", width = 8.6, height = 11)

#------------------------------------------------------------------------------#
# FIGURE 5.5: INDIRECT PLOT ----
#------------------------------------------------------------------------------#

panel_format <- list(
  scale_color_manual(values = "black"),
  labs(title = NULL, subtitle = NULL),
  book_theme,
  scale_fill_manual(values = NA),
  theme(legend.position = "none"),
  scale_y_continuous(breaks = NULL)
)

fig5_5a <- posterior_plot(mod3, 'indirect14',  line_width = 0.5) + panel_format + 
  labs(x = "Indirect Effect at Age 14)") +
  scale_x_continuous(breaks = seq(-.10, 0.5, 0.1)) +
  coord_cartesian(xlim = c(-.10, 0.50))
fig5_5b <- posterior_plot(mod3, 'indirect15',  line_width = 0.5) + panel_format + 
  labs(x = "Indirect Effect at Age 15") +
  scale_x_continuous(breaks = seq(-.10, 0.5, 0.1)) +
  coord_cartesian(xlim = c(-.10, 0.50))
fig5_5c <- posterior_plot(mod3, 'indirect16', line_width = 0.5) + panel_format +
  labs(x = "Indirect Effect at Age 16") +
  scale_x_continuous(breaks = seq(-.10, 0.5, 0.1)) +
  coord_cartesian(xlim = c(-.10, 0.50))
fig5_5d <- posterior_plot(mod3, 'indirect17', line_width = 0.5) + panel_format +
  labs(x = "Indirect Effect at Age 17") +
  scale_x_continuous(breaks = seq(-.10, 0.5, 0.1)) +
  coord_cartesian(xlim = c(-.10, 0.50))
fig5_5e <- posterior_plot(mod3, 'index_mod', line_width = 0.5) + panel_format + 
  labs(x = "Index of Moderated Mediation") +
  scale_x_continuous(breaks = round(seq(-.15, 0.05, 0.05),2)) +
  coord_cartesian(xlim = c(-.15, 0.05))

fig5_5 <- (fig5_5a / fig5_5b / fig5_5c / fig5_5d / fig5_5e) +
  plot_annotation(tag_levels = "A") &
  book_theme &
  labs(title = NULL) &
  theme(legend.position = "none")

save_fig(fig5_5, "Figure 5.5", width = 8.5, height = 11)