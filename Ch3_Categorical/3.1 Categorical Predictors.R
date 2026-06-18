# LINEAR REGRESSION WITH CATEGORICAL PREDICTORS

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/smoking.csv'

# create data frame from github data
smoking <- read.csv(data_url)

#------------------------------------------------------------------------------#
# LINEAR REGRESSION MODEL ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = smoking,                                # R data frame
  ordinal = 'parsmoke',                          # binary and ordinal variables
  nominal = 'educ',                              # nominal variables (auto dummy coded)
  center = 'age',                                # center predictors
  model = 'cigsperday ~ parsmoke educ age',      # regression model
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod1)                                     # print output
posterior_plot(mod1, 'cigsperday')               # plot parameter distributions

#------------------------------------------------------------------------------#
# GROUP MEANS AND CONTRASTS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = smoking,                                # R data frame
  ordinal = 'parsmoke',                          # binary and ordinal variables
  nominal = 'educ',                              # nominal variables (auto dummy coded)
  center = 'age',                                # center predictors
  model = 'cigsperday ~ intercept@b0 parsmoke@b1 educ.2@b2 educ.3@b3 age', # label dummy codes
  parameters = '
    mean_educ1 = b0 + (.452 * b1);               # group 1 mean
    mean_educ2 = mean_educ1 + b2;                # group 2 mean
    mean_educ3 = mean_educ1 + b3;                # group 3 mean
    educ2_vs_educ3 = mean_educ3 – mean_educ2;',  # group 3 vs. 2 contrast
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod2)                                     # print output

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = smoking,                                # R data frame
  ordinal = 'parsmoke',                          # binary and ordinal variables
  nominal = 'educ',                              # nominal variables (auto dummy coded)
  center = 'age',                                # center predictors
  model = 'cigsperday ~ parsmoke educ age',      # regression model
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod3)                                     # print output

distribution_plot(mod3)                          # plot observed and imputed distributions
residuals_plot(mod3)                             # plot residuals

#------------------------------------------------------------------------------#
# LINEAR REGRESSION WITH A LATENT RESPONSE VARIABLE PREDICTOR ----
#------------------------------------------------------------------------------#

mod4 <- rblimp(
  data = smoking,                                # R data frame
  ordinal = 'parsmoke',                          # binary and ordinal variables
  nominal = 'educ',                              # nominal variables (auto dummy coded)
  center = 'age',                                # center predictors
  model = '
    cigsperday ~ parsmoke.latent educ age;       # focal model w latent predictor
    parsmoke ~ educ age',                        # predictor model
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod4)                                     # print output
posterior_plot(mod4, 'cigsperday')               # plot parameter distributions

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
# FIGURE 3.1 ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(
  mod3,
  observed_color = "grey60",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 18,
  line_width     = 0.5
)

fig3_1A <- dp$parsmoke
fig3_1B <- dp$parsmoke.latent +
  geom_vline(xintercept = 0, linewidth = 0.5)

fig3_1 <- (fig3_1A / fig3_1B) +
  plot_annotation(tag_levels = "A") &
  book_theme &
  caps_axes &
  labs(title = NULL) &
  theme(legend.position = "none")

save_fig(fig3_1, "Figure 3.1", width = 8.5, height = 11)

#------------------------------------------------------------------------------#
# FIGURE 3.3: DISTRIBUTIONS ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(
  mod3,
  observed_color = "grey60",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 18,
  line_width     = 0.5
)

fig3_3 <- dp$cigsperday +
  plot_layout(guides = "collect") +
  plot_annotation() &
  book_theme &
  caps_axes &
  labs(title = NULL)

save_fig(fig3_3, "Figure 3.3", width = 11, height = 8.5)
