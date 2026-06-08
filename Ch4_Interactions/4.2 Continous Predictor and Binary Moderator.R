# INTERACTION INVOLVING A CONTINUOUS PREDICTOR AND A BINARY MODERATOR

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

# add the product term
mod1 <- rblimp(
  data = reading,                                # R data frame
  ordinal = 'esl lprobhi',                      # binary and ordinal variables
  center = 'read1',                              # center predictors
  model = 'read9 ~ read1 lprobhi read1*lprobhi esl',  # product term
  simple = 'read1 | lprobhi',                   # conditional effects by group
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod1)                                     # print output
posterior_plot(mod1, 'read9')                    # plot parameter distributions
simple_plot(read9 ~ read1 | lprobhi, mod1)      # plot conditional effects

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = reading,                                # R data frame
  ordinal = 'esl lprobhi',                      # binary and ordinal variables
  center = 'read1',                              # center predictors
  model = 'read9 ~ read1 lprobhi read1*lprobhi esl', # product term
  simple = 'read1 | lprobhi',                   # conditional effects by group
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod2)                                     # print output

distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot residuals

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
# FIGURE 4.3 ----
#------------------------------------------------------------------------------#

# make the plots
fig4_3 <- simple_plot(read9 ~ read1 | lprobhi, mod1)


# linetype by moderator level on the line layers only
for (i in which(vapply(fig4_3$layers,
                       function(l) inherits(l$geom, "GeomLine"), logical(1)))) {
  q <- fig4_3$layers[[i]]$mapping[["colour"]]
  if (is.null(q)) q <- fig4_3$mapping[["colour"]]
  fig4_3$layers[[i]]$mapping[["linetype"]] <- q
}

# make the two CI ribbons a bit more visible
for (i in which(vapply(fig4_3$layers,
                       function(l) inherits(l$geom, "GeomRibbon"), logical(1)))) {
  fig4_3$layers[[i]]$aes_params$alpha <- 0.35
}

fig4_3 <- fig4_3 +
  ggplot2::scale_colour_manual(values = c("black", "black"), guide = "none") +
  ggplot2::scale_fill_manual(values = c("grey70", "grey30"), guide = "none") +  # @0 light, @1 dark; no swatch in legend
  ggplot2::scale_linetype_manual(
    values = c("solid", "dashed"),           # @0 solid, @1 dashed
    name   = "Learning problems",
    labels = c("Not elevated", "Elevated")   # set to your moderator's coding
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
  filename = "~/desktop/Figure 4.3.pdf",
  plot     = fig4_3,
  width    = 11,
  height   = 8.5,
  units    = "in",
  device   = cairo_pdf
)
