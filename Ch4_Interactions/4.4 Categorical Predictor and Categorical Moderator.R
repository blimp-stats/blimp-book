# INTERACTION INVOLVING A CATEGORICAL PREDICTOR AND A CATEGORICAL MODERATOR

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
# MODERATED REGRESSION ----
#------------------------------------------------------------------------------#

# basic specification
mod1 <- rblimp(
  data = inflamm,                                # R data frame
  ordinal = 'female els',                        # binary and ordinal variables
  center = 'age',                                # center predictors
  model = 'inflam ~ intercept els female female*els age',  # product term
  simple = 'els | female',                       # conditional effects by group
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod1)                                     # print output
posterior_plot(mod1, 'inflam')                   # plot parameter distributions
simple_plot(inflam ~ els | female, mod1)         # plot conditional effects

# compute group means
mod2 <- rblimp(
  data = inflamm,                                # R data frame
  ordinal = 'female els',                        # binary and ordinal variables
  center = 'age',                                # center predictors
  model = 'inflam ~ intercept@b0 els@b1 female@b2 female*els@b3 age',  # product term
  simple = 'els | female',                       # conditional effects by group
  parameters = '                               
    mean_sex0els0 = b0;                              # group mean (fem = 0, els = 0)
    mean_sex0els1 = b0 + b1;                         # group mean (fem = 0, els = 1)
    mean_sex1els0 = b0 + b2;                         # group mean (fem = 1, els = 0)
    mean_sex1els1 = b0 + b1 + b2 + b3;',             # group mean (fem = 1, els = 1)
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod2)                                     # print output
posterior_plot(mod2, 'inflam')                   # plot parameter distributions
simple_plot(inflam ~ els | female, mod2)         # plot conditional effects

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = inflamm,                                # R data frame
  ordinal = 'female els',                        # binary and ordinal variables
  center = 'age',                                # center predictors
  model = 'inflam ~ intercept@b0 els@b1 female@b2  female*els@b3 age',  # product term
  simple = 'els | female',                       # conditional effects by group
  parameters = '                               
    mean_f0e0 = b0;                              # group mean (fem = 0, els = 0)
    mean_f0e1 = b0 + b1;                         # group mean (fem = 0, els = 1)
    mean_f1e0 = b0 + b2;                         # group mean (fem = 1, els = 0)
    mean_f1e1 = b0 + b1 + b2 + b3;',             # group mean (fem = 1, els = 1)
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
# FIGURE 4.5 ----
#------------------------------------------------------------------------------#

# make the plots
fig4_5 <- simple_plot(inflam ~ els | female, mod1)

# linetype by moderator level on the line layers only
for (i in which(vapply(fig4_5$layers,
                       function(l) inherits(l$geom, "GeomLine"), logical(1)))) {
  q <- fig4_5$layers[[i]]$mapping[["colour"]]
  if (is.null(q)) q <- fig4_5$mapping[["colour"]]
  fig4_5$layers[[i]]$mapping[["linetype"]] <- q
}

# make the two CI ribbons a bit more visible
for (i in which(vapply(fig4_5$layers,
                       function(l) inherits(l$geom, "GeomRibbon"), logical(1)))) {
  fig4_5$layers[[i]]$aes_params$alpha <- 0.35
}

fig4_5 <- fig4_5 +
  ggplot2::scale_colour_manual(values = c("black", "black"), guide = "none") +
  ggplot2::scale_fill_manual(values = c("grey70", "grey30"), guide = "none") +  # @0 light, @1 dark; no swatch in legend
  ggplot2::scale_linetype_manual(
    values = c("solid", "dashed"),           # @0 solid, @1 dashed
    name   = "Sex",
    labels = c("Males", "Females")   # set to your moderator's coding
  ) +
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
  filename = "~/desktop/Figure 4.5.pdf",
  plot     = fig4_5,
  width    = 11,
  height   = 8.5,
  units    = "in",
  device   = cairo_pdf
)
