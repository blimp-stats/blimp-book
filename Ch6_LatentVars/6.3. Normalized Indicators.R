# LATENT VARIABLE MEDIATION MODEL W NONNORMAL INDICATORS

# plotting functions
# source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source("/Users/craig/Documents/Claude/Projects/Blimp Book/rblimp_cleaned_functions.R")

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflamm <- read.csv(data_url)

#------------------------------------------------------------------------------#
# TRANSFORMED INDICATORS ----
#------------------------------------------------------------------------------#

# yeo-johnson normalized indicators
mod1 <- rblimp(
  data = inflammation,             		               # R data frame
  ordinal = 'female els',    			               # binary and ordinal variables
  latent = 'inflammation',	       	             # define latent variable  
  center = 'age',                                # center predictors
  model = '
    structural: 					                       # model block label
    inflammation ~ els female age;            # latent variable regression
    inflammation@1;        			                 # fix latent variance at 1
    yjt(dpdd - 6) ~ inflammation els female age;  # regression model with yjt transform
	  measurement:          			                 # model block label
    inflammation –> yjt(crp)@load1 yjt(il6) tnf yjt(ifn);', # measurement model with yjt transform
  seed = 90291,               		               # random number seed
  burn = 10000,               		               # warm-up iterations
  iter = 10000                		               # analysis iterations
)

output(mod1)                                     # print output
round(standardized(mod1),3)                               # print standardized estimates in one table
posterior_plot(mod1)                             # plot parameter distributions

# log transformed indicators
mod2 <- rblimp(
  data = inflammation,             		               # R data frame
  ordinal = 'female els',    			               # binary and ordinal variables
  latent = 'inflammation',	       	             # define latent variable  
  center = 'age',                                # center predictors
  model = '
    structural: 					                       # model block label
    inflammation ~ els female age;            # latent variable regression
    inflammation@1;        			                 # fix latent variance at 1
    ln(dpdd) ~ inflammation els female age;   # regression model with log transform
	  measurement:          			                 # model block label
	  ln(crp + 5) ~ inflammation;                  # measurement model with log transform
	  ln(il6 + 5) ~ inflammation;                  # measurement model with log transform
	  tnf ~ inflammation;                          # measurement model with normal indicator
	  ln(ifn + 5) ~ inflammation;',                # measurement model with log transform
  seed = 90291,               		               # random number seed
  burn = 10000,               		               # warm-up iterations
  iter = 10000,                		               # analysis iterations
)

output(mod2)                                     # print output
standardized(mod2)                               # print standardized estimates in one table
posterior_plot(mod2)                             # plot parameter distributions

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# yeo-johnson normalized indicators
mod3 <- rblimp(
  data = inflammation,             		               # R data frame
  ordinal = 'female els',    			               # binary and ordinal variables
  latent = 'inflammation',	       	             # define latent variable  
  center = 'age',                                # center predictors
  model = '
    structural: 					                       # model block label
    inflammation ~ els female age;            # latent variable regression
    inflammation@1;        			                 # fix latent variance at 1
    yjt(dpdd - 6) ~ inflammation els female age;   # regression model with yjt transform
	  measurement:          			                 # model block label
    inflammation –> yjt(crp)@load1 yjt(il6) tnf yjt(ifn);', # measurement model with yjt transform
  seed = 90291,               		               # random number seed
  burn = 10000,               		               # warm-up iterations
  iter = 10000,                		               # analysis iterations
  nimps = 20                                     # save 20 imputed data sets
)

output(mod3)                                     # print output
distribution_plot(mod3)                          # plot observed and imputed distributions
residuals_plot(mod3)                             # plot residuals

# log transformed indicators
mod4 <- rblimp(
  data = inflammation,             		               # R data frame
  ordinal = 'female els',    			               # binary and ordinal variables
  latent = 'inflammation',	       	             # define latent variable  
  center = 'age',                                # center predictors
  model = '
    structural: 					                       # model block label
    inflammation ~ els@a1 female age;            # latent variable regression
    inflammation@1;        			                 # fix latent variance at 1
    dpdd ~ inflammation@b1 els female age;       # regression model
	  measurement:          			                 # model block label
	  ln(crp + 5) ~ inflammation;                  # measurement model with log transform
	  ln(il6 + 5) ~ inflammation;                  # measurement model with log transform
	  tnf ~ inflammation;                          # measurement model with normal indicator
	  ln(ifn + 5) ~ inflammation;',                # measurement model with log transform
  parameters = 'indirect = a1 * b1',             # indirect effect 
  seed = 90291,               		               # random number seed
  burn = 10000,               		               # warm-up iterations
  iter = 10000,                		               # analysis iterations
  nimps = 20                                    # save 20 imputed data sets
)

output(mod4)                                     # print output
distribution_plot(mod4)                          # plot observed and imputed distributions
residuals_plot(mod4)                             # plot residuals
   
#------------------------------------------------------------------------------#
# BOOK FIGURE THEME ----
#------------------------------------------------------------------------------#

library(patchwork)
library(ggplot2)

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
         units = "in", dpi = dpi, device = ragg::agg_png)
  message("Wrote:\n  ", pdf_path, "\n  ", png_path)   # confirms exact paths
  invisible(c(pdf_path, png_path))
}

#------------------------------------------------------------------------------#
# FIGURE 6.5: YJT RESIDUAL DISTRIBUTIONS ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(mod3, observed_color = "grey60", imputed_color = "grey40",
                        density_color = "black", font_size = 18, line_width = 0.5)
rp <- residuals_plot(mod3, point_color = "grey40", curve_color = "black",
                     point_size = 0.35, point_alpha = 0.25,
                     font_size = 18, line_width = 0.6, label_family = "Minion Pro")

fig6_5 <- (dp$crp.residual | (rp$crp.inflammation + labs(y = "Std. Residual"))) /
  (dp$il6.residual | (rp$il6.inflammation + labs(y = "Std. Residual"))) /
  (dp$tnf.residual | (rp$tnf.inflammation + labs(y = "Std. Residual"))) /
  (dp$ifn.residual | (rp$ifn.inflammation + labs(y = "Std. Residual"))) +
  plot_annotation(tag_levels = "A") &
  book_theme &
  caps_axes &
  labs(title = NULL) &
  theme(plot.tag.location = "margin")

save_fig(fig6_5, "Figure 6.5", width = 8.5, height = 11)