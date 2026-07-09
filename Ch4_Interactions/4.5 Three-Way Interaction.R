# REGRESSION WITH THREE-WAY INTERACTION 

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
    emostab ~ orgcon consci;',   		             # predictor model
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
library(ggplot2)
library(ragg)
library(lemon)

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
  labs(title = NULL)

save_fig(fig4_7, "Figure 4.7", width = 8.5, height = 11)

#------------------------------------------------------------------------------#
# FIGURE 4.8: SIMPLE INTERACTIONS ----
#------------------------------------------------------------------------------#

emostab_lab <- as_labeller(function(x) sub("^([a-z0-9]+)", "\\U\\1", x, perl = TRUE))

fig4_8 <- simple_plot(workbeh ~ orgcon | consci + emostab, mod1) +
  facet_rep_grid(emostab ~ ., repeat.tick.labels = FALSE, labeller = emostab_lab)

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
  scale_linetype_manual(
    values = c("dashed", "solid", "dotted"),       # consci: -1 SD / Mean / +1 SD
    name   = "CONSCI",
    labels = c("-1 SD", "Mean", "+1 SD")
  ) +
  scale_colour_manual(values = rep("black", 3)) +
  guides(colour = "none", fill = "none") +
  labs(title = NULL, subtitle = NULL) +
  book_theme +
  caps_axes +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border     = element_blank(),                                  # no box
    axis.line        = element_line(colour = "black", linewidth = 0.5),  # L on every panel
    strip.background = element_blank(),
    strip.text = element_text(face = "plain"),
    legend.position  = "bottom"
  )

save_fig(fig4_8, "Figure 4.8", width = 8.5, height = 11)
