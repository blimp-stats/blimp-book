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
  scale_colour_manual(values = c("black", "black"), guide = "none") +
  scale_fill_manual(values = c("grey70", "grey30"), guide = "none") +  # @0 light, @1 dark; no swatch in legend
  scale_linetype_manual(
    values = c("solid", "dashed"),           # @0 solid, @1 dashed
    name   = "Learning problems",
    labels = c("Not elevated", "Elevated")   # set to your moderator's coding
  ) +
  labs(title = NULL, subtitle = NULL) +
  book_theme +
  caps_axes +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(colour = "black", linewidth = 0.5),
    legend.position  = "bottom"
  )

save_fig(fig4_3, "Figure 4.3", width = 11, height = 8.5)
