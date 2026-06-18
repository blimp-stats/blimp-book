# LOGISTIC REGRESSION FOR BINARY OUTCOMES

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/alcohol.csv'

# create data frame from github data
alcohol <- read.csv(data_url)

#------------------------------------------------------------------------------#
# BINARY LOGISTIC MODEL ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = alcohol,                                # R data frame
  ordinal = 'college male',                      # binary and ordinal variables
  nominal = 'drinker',                           # nominal variables (auto dummy coded)
  center = 'agetryalc age',                      # center predictors
  model = 'drinker ~ agetryalc college age male', # regression model
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod1)                                     # print output
posterior_plot(mod1, 'drinker')                  # plot parameter distributions

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = alcohol,                                # R data frame
  ordinal = 'college male',                      # binary and ordinal variables
  nominal = 'drinker',                           # nominal variables (auto dummy coded)
  center = 'agetryalc age',                      # center predictors
  model = 'drinker ~ agetryalc college age male', # regression model
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod2)                                     # print output

distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot binned residuals

#------------------------------------------------------------------------------#
# MARGINAL PREDICTED PROBABILITIES ----
#------------------------------------------------------------------------------#

# average marginal predicted probabilities by college x male
library(mitml)

implist <- as.mitml(mod2)                        # mitml-compatible list of data frames
probs <- with(implist,                           # fit lm to each data set
              lm(drinker.1.probability ~ 0 +
                   I((male == 0) * (college == 0)) +
                   I((male == 0) * (college == 1)) +
                   I((male == 1) * (college == 0)) +
                   I((male == 1) * (college == 1))))

testEstimates(probs)                             # pool estimates and standard errors

#------------------------------------------------------------------------------#
# LOGISTIC MODELS WITH CURVILINEAR AGE EFFECTS ----
#------------------------------------------------------------------------------#

# quadratic age effect
mod3 <- rblimp(
  data = alcohol,                                # R data frame
  ordinal = 'college male',                      # binary and ordinal variables
  nominal = 'drinker',                           # nominal variables (auto dummy coded)
  center = 'agetryalc age',                      # center predictors
  model = 'drinker ~ agetryalc college age age^2 male', # regression with quadratic age
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

residuals_plot(mod3)                             # plot binned residuals

# cubic age effect
mod4 <- rblimp(
  data = alcohol,                                # R data frame
  ordinal = 'college male',                      # binary and ordinal variables
  nominal = 'drinker',                           # binary outcome (auto dummy coded)
  center = 'agetryalc age',                      # center predictors
  model = 'drinker ~ agetryalc college age age^2 age^3 male', # regression with cubic age
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

residuals_plot(mod4)                             # plot binned residuals

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
# FIGURE 3.5: DISTRIBUTIONS ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(
  mod2,
  observed_color = "grey60",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 18,
  line_width     = 0.5
)

fig3_5 <- dp$agetryalc +
  plot_layout(guides = "collect") +
  plot_annotation() &
  book_theme &
  caps_axes &
  labs(title = NULL)

save_fig(fig3_5, "Figure 3.5", width = 11, height = 8.5)

#------------------------------------------------------------------------------#
# FIGURE 3.6: PREDICTED PROBABILITIES ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(
  mod2,
  observed_color = "grey60",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 18,
  line_width     = 0.5
)

rp <- residuals_plot(
  mod2,
  point_color  = "grey40",
  curve_color  = "black",
  font_size    = 18,
  line_width   = 0.6,
  label_family = "Minion Pro",
)

fig3_6 <- dp$drinker.1.probability / rp$drinker.college.probability +
  plot_annotation(tag_levels = "A") &
  book_theme &
  caps_axes &
  labs(title = NULL) &
  scale_color_manual(values = rep("black", 7))

save_fig(fig3_6, "Figure 3.6", width = 8.5, height = 11)

#------------------------------------------------------------------------------#
# FIGURE 3.7: RESIDUAL VS. PREDICTED + RESIDUAL VS. PREDICTORS ----
#------------------------------------------------------------------------------#

rp <- residuals_plot(
  mod2,
  point_color  = "grey40",
  curve_color  = "black",
  font_size    = 18,
  line_width   = 0.6,
  label_family = "Minion Pro"
)

fig3_7 <- rp$drinker.1.binned / rp$drinker.1.agetryalc / rp$drinker.1.age +
  plot_annotation(tag_levels = "A") &
  book_theme &
  caps_axes &
  labs(title = NULL)

save_fig(fig3_7, "Figure 3.7", width = 8.5, height = 11)
