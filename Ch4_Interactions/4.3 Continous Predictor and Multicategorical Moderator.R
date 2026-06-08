# INTERACTION INVOLVING A CONTINUOUS PREDICTOR AND A MULTICATEGORICAL MODERATOR

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/reading.csv'

# create data frame from github data
reading <- read.csv(data_url)

#------------------------------------------------------------------------------#
# MODERATED REGRESSION ----
#------------------------------------------------------------------------------#

# compact specification
mod1 <- rblimp(
  data = reading,                                # R data frame
  ordinal = 'esl',                               # binary and ordinal variables
  nominal = 'lproblev',                         # nominal variables (auto dummy coded)
  center = 'read1',                              # center predictors
  model = 'read9 ~ read1 lproblev read1*lproblev esl', # product term
  simple = 'read1 | lproblev',                  # conditional effects by group
  waldtest = 'read9 ~ read1 lproblev esl',      # wald test of interaction
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod1)                                     # print output
posterior_plot(mod1, 'read9')                    # plot parameter distributions
simple_plot(read9 ~ read1 | lproblev, mod1)     # plot conditional effects

# explicit specification
mod2 <- rblimp(
  data = reading,                                # R data frame
  ordinal = 'esl',                               # binary and ordinal variables
  nominal = 'lproblev',                         # nominal variables (auto dummy coded)
  center = 'read1',                              # center predictors
  model = '
    read9 ~ read1 lproblev.1 lproblev.2 
      read1*lproblev.1@b4 read1*lproblev.2@b5 esl', # product term
  simple = 'read1 | lproblev.1',                # conditional effects by group
  waldtest = 'b4 - b5 = 0',                      # wald test of interaction
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod2)                                     # print output
posterior_plot(mod2, 'read9')                    # plot parameter distributions
simple_plot(read9 ~ read1 | lproblev, mod2)     # plot conditional effects

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = reading,                                # R data frame
  ordinal = 'esl',                               # binary and ordinal variables
  nominal = 'lproblev',                         # nominal variables (auto dummy coded)
  center = 'read1',                              # center predictors
  model = 'read9 ~ read1 lproblev read1*lproblev esl', # product term
  simple = 'read1 | lproblev',                  # conditional effects by group
  waldtest = 'read9 ~ read1 lproblev esl',      # wald test of interaction
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod3)                                     # print output
distribution_plot(mod3)                          # plot observed and imputed distributions
residuals_plot(mod3)                             # plot residuals

#------------------------------------------------------------------------------#
# BOOK FIGURE THEME ----
#------------------------------------------------------------------------------#

library(patchwork)

book_theme <- ggplot2::theme(
  text              = ggplot2::element_text(family = "Minion Pro", size = 18),
  axis.text         = ggplot2::element_text(color = "black", size = 18),
  axis.line         = ggplot2::element_line(color = "black", linewidth = 0.5, lineend = "square"),
  axis.ticks        = ggplot2::element_line(color = "black", linewidth = 0.5),
  axis.ticks.length = grid::unit(4, "pt"),
  legend.text       = ggplot2::element_text(size = 18),
  legend.title      = ggplot2::element_text(size = 18),
  plot.tag          = ggplot2::element_text(face = "bold", size = 22),
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

#------------------------------------------------------------------------------#
# FIGURE 4.4 ----
#------------------------------------------------------------------------------#

fig4_4 <- simple_plot(read9 ~ read1 | lproblev, mod1)

# linetype by group on the line layers only
for (i in which(vapply(fig4_4$layers,
                       function(l) inherits(l$geom, "GeomLine"), logical(1)))) {
  q <- fig4_4$layers[[i]]$mapping[["colour"]]
  if (is.null(q)) q <- fig4_4$mapping[["colour"]]
  fig4_4$layers[[i]]$mapping[["linetype"]] <- q
}

# drop the three CI ribbons
fig4_4$layers <- fig4_4$layers[
  !vapply(fig4_4$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1))
]

fig4_4 <- fig4_4 +
  ggplot2::scale_linetype_manual(
    values = c("solid", "dashed", "dotted"),
    name   = "Learning problems",
    labels = c("Low", "High", "Medium")   # <- ORDER IS NOT low/med/high; see note
  ) +
  ggplot2::scale_colour_manual(values = rep("black", 3)) +
  ggplot2::guides(colour = "none", fill = "none") +
  ggplot2::labs(title = NULL, subtitle = NULL) +
  book_theme +
  caps_axes +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.line        = ggplot2::element_line(colour = "black", linewidth = 0.5),
    legend.position  = "bottom"
  )

ggplot2::ggsave(
  filename = "~/desktop/Figure 4.4.pdf",
  plot     = fig4_4,
  width    = 11,
  height   = 8.5,
  units    = "in",
  device   = cairo_pdf
)