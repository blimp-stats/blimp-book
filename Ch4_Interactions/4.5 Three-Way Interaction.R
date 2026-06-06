# CURVILINEAR REGRESSION

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
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/workbehavior.csv'

# create data frame from github data
workbeh <- read.csv(data_url)

#------------------------------------------------------------------------------#
# MODERATED REGRESSION MODEL ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
    data = workbeh,                              # R data frame
    center = 'orgcon consci emostab',            # center predictors
    model = '
      workbeh ~ orgcon consci emostab
       orgcon*consci orgcon*emostab consci*emostab
       orgcon*consci*emostab;',                  # 2- and 3-way products
    simple = 'orgcon | consci and emostab',      # conditional effects
    seed = 90291,                                # random number seed
    burn = 10000,                                # warm-up iterations
    iter = 10000)                                # analysis iterations

output(mod1)                                     # print output
posterior_plot(mod1, 'workbeh')                  # plot parameter distributions
simple_plot(workbeh ~ orgcon | consci + emostab, mod1)  # plot conditional effects

#------------------------------------------------------------------------------#
# PROBE 3-WAY INTERACTION ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = workbeh,                                # R data frame
  center = 'orgcon consci emostab',              # center predictors
  model = '
    workbeh ~ orgcon consci emostab
      orgcon*consci@b4 orgcon*emostab consci*emostab
      orgcon*consci*emostab@b7;                  # focal model
    emostab ~ orgcon consci; DEBUG: compact_output',   		             # predictor model
  parameters = '
    sd_emostab = sqrt(emostab.totalvar);	       # moderator std. dev.
    int_hi_emostab = b4 + b7*sd_emostab;	       # simple interaction
    int_lo_emostab = b4 - b7*sd_emostab;',       # simple interaction
  simple = 'orgcon | consci and emostab',        # conditional effects
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

output(mod2)                                     # print output
simple_plot(workbeh ~ orgcon | consci + emostab, mod1)  # plot conditional effects

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod3 <- rblimp(
  data = workbeh,                                # R data frame
  center = 'orgcon consci emostab',              # center predictors
  model = '
      workbeh ~ orgcon consci emostab
       orgcon*consci orgcon*emostab consci*emostab
       orgcon*consci*emostab',                   # 3-way product
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000,                                  # analysis iterations
  nimps = 20)                                    # save 20 data sets

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
# FIGURE 4.7: DISTRIBUTIONS ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(
  mod3,
  observed_color = "grey60",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 18,
  line_width     = 0.5
)

fig4_7 <- (dp$orgcon / dp$workbeh) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  book_theme &
  caps_axes &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 4.7.pdf",
  plot     = fig4_7,
  width    = 8.5,
  height   = 11,
  units    = "in",
  device   = cairo_pdf
)

#------------------------------------------------------------------------------#
# FIGURE 4.8: SIMPLE INTERACTIONS ----
#------------------------------------------------------------------------------#

emostab_lab <- ggplot2::as_labeller(function(x) sub("^([a-z0-9]+)", "\\U\\1", x, perl = TRUE))

fig4_8 <- simple_plot(workbeh ~ orgcon | consci + emostab, mod1) +
  lemon::facet_rep_grid(emostab ~ ., repeat.tick.labels = FALSE, labeller = emostab_lab)

# --- linetype by consci on the line layers; drop the ribbons ---
for (i in which(vapply(fig4_8$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))) {
  q <- fig4_8$layers[[i]]$mapping[["colour"]]
  if (is.null(q)) q <- fig4_8$mapping[["colour"]]
  fig4_8$layers[[i]]$mapping[["linetype"]] <- q
}
fig4_8$layers <- fig4_8$layers[
  !vapply(fig4_8$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1))
]

# --- B&W styling (single faceted plot, so '+') ---
fig4_8 <- fig4_8 +
  ggplot2::scale_linetype_manual(
    values = c("dashed", "solid", "dotted"),       # consci: -1 SD / Mean / +1 SD
    name   = "CONSCI",
    labels = c("-1 SD", "Mean", "+1 SD")
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
    panel.border     = ggplot2::element_blank(),                                  # no box
    axis.line        = ggplot2::element_line(colour = "black", linewidth = 0.5),  # L on every panel
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "plain"),
    legend.position  = "bottom"
  )

ggplot2::ggsave(
  filename = "~/desktop/Figure 4.8.pdf",
  plot     = fig4_8,
  width    = 8.5,
  height   = 11,
  units    = "in",
  device   = cairo_pdf
)
