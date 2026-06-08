# INTERACTION INVOLVING A CONTINUOUS PREDICTOR AND A CONTINUOUS MODERATOR

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
  ordinal = 'esl',                               # binary and ordinal variables
  center = 'read1 lrnprob1',                     # center predictors
  model = 'read9 ~ read1 lrnprob1 read1*lrnprob1 esl', # product term
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod1)                                     # print output
posterior_plot(mod1, 'read9')                    # plot parameter distributions

#------------------------------------------------------------------------------#
# PROBING INTERACTION ----
#------------------------------------------------------------------------------#

# simple slopes at sd units
mod2 <- rblimp(
  data = reading,                                # R data frame
  ordinal = 'esl',                               # binary and ordinal variables
  center = 'read1 lrnprob1',                     # center predictors
  model = 'read9 ~ read1 lrnprob1 read1*lrnprob1 esl', # product term
  simple = 'read1 | lrnprob1',                   # conditional effects of read1 at SD units
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod2)                                     # print output
simple_plot(read9 ~ read1 | lrnprob1, mod2)      # plot conditional effects
jn_plot(read9 ~ read1 | lrnprob1, mod2)          # plot johnson-neyman regions

# simple slopes at quantiles
mod3 <- rblimp(
  data = reading,                                # R data frame
  ordinal = 'esl',                               # binary and ordinal variables
  center = 'read1 lrnprob1',                     # center predictors
  model = 'read9 ~ read1 lrnprob1 read1*lrnprob1 esl', # product term
  simple = 'read1 | lrnprob1 @ quantile',        # conditional effects at 16/50/84% quantiles
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod3)                                     # print output
simple_plot(read9 ~ read1 | lrnprob1, mod3)      # plot conditional effects
jn_plot(read9 ~ read1 | lrnprob1, mod3)          # plot johnson-neyman regions

# simple slopes at quartiles
mod4 <- rblimp(
  data = reading,                                # R data frame
  ordinal = 'esl',                               # binary and ordinal variables
  center = 'read1 lrnprob1',                     # center predictors
  model = 'read9 ~ read1 lrnprob1 read1*lrnprob1 esl', # product term
  simple = 'read1 | lrnprob1 @ quartile',        # conditional effects at quartiles
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod4)                                     # print output
simple_plot(read9 ~ read1 | lrnprob1, mod4)      # plot conditional effects

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod5 <- rblimp(
  data = reading,                                # R data frame
  ordinal = 'esl',                               # binary and ordinal variables
  center = 'read1 lrnprob1',                     # center predictors
  model = 'read9 ~ read1 lrnprob1 read1*lrnprob1 esl', # product term
  simple = 'read1 | lrnprob1',                   # conditional effects of read1 at SD units
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 imputed data sets

output(mod5)                                     # print output

distribution_plot(mod5)                          # plot observed and imputed distributions
residuals_plot(mod5)                             # plot residuals

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
# FIGURE 4.2 ----
#------------------------------------------------------------------------------#

# make the plots
fig4_2A <- simple_plot(read9 ~ read1 | lrnprob1, mod2)
fig4_2B <- jn_plot(read9 ~ read1 | lrnprob1, mod2)

# grey the non-significant region (NAMED values, so it maps correctly even when
# only one significance level is present in range)
fig4_2B <- fig4_2B +
  ggplot2::scale_fill_manual(values = c(`TRUE` = NA, `FALSE` = "grey80")) +  # TRUE=sig (no fill); FALSE=non-sig (grey)
  ggplot2::labs(subtitle = "Shaded area represents 0 within 95% interval\nBound: -24")

# linetype by moderator level (line layers only), black lines, drop CI ribbons
for (i in which(vapply(fig4_2A$layers,
                       function(l) inherits(l$geom, "GeomLine"), logical(1)))) {
  q <- fig4_2A$layers[[i]]$mapping[["colour"]]
  if (is.null(q)) q <- fig4_2A$mapping[["colour"]]
  fig4_2A$layers[[i]]$mapping[["linetype"]] <- q
}
fig4_2A$layers <- fig4_2A$layers[
  !vapply(fig4_2A$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1))
]
fig4_2A <- fig4_2A +
  ggplot2::scale_linetype_manual(
    values = c("dashed", "solid", "dotted"),               # middle = solid
    name   = "First-grade learning problems",
    labels = c("-1 SD", "Mean", "+1 SD")
  ) +
  ggplot2::scale_colour_manual(values = rep("black", 3)) +
  ggplot2::guides(colour = "none", fill = "none") +
  ggplot2::labs(subtitle = NULL)                           # drop "Centered variables: ..."

# force white panels + no grid on every subplot
fig4_2 <- (fig4_2A / fig4_2B) +
  plot_annotation(tag_levels = "A") &
  book_theme &
  caps_axes &                                              # <- apply uppercase axis names
  ggplot2::labs(title = NULL) &
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.line        = ggplot2::element_line(colour = "black", linewidth = 0.5)
  )

ggplot2::ggsave(
  filename = "~/desktop/Figure 4.2.pdf",
  plot     = fig4_2,
  width    = 8.5,
  height   = 11,
  units    = "in",
  device   = cairo_pdf
)