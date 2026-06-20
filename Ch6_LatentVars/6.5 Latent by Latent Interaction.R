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
# LATENT VARIABLE INTERACTION ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = workbeh,                                 # R data frame
  ordinal = 'workbeh1:workbeh6 orgcon1:orgcon6 consci1:consci6',  # binary and ordinal variables
  latent = 'orgconstr conscient wrkbehave',      # define latent variables
  model = '
    structural:                                   # model block label
    wrkbehave ~ orgconstr conscient orgconstr*conscient;  # latent product
    wrkbehave@1;                                  # fix residual variance to 1
    predictor:                                    # model block label
    orgconstr ~~ conscient;                      # latent correlation
    orgconstr@1; conscient@1;                    # fix variances to 1
    measurement:                                  # model block label
    wrkbehave -> workbeh1@wload1 workbeh2:workbeh6;  # measurement model with estimated loadings
    orgconstr -> orgcon1@oload1 orgcon2:orgcon6;  # measurement model with estimated loadings
    conscient -> consci1@cload1 consci2:consci6;', # measurement model with estimated loadings           
  simple = 'orgconstr | conscient',               # conditional effects
  seed = 90291,                                   # random number seed
  burn = 20000,                                   # warm-up iterations
  iter = 20000)                                   # analysis iterations

output(mod1)                                     # print output
posterior_plot(mod1, 'wrkbehave')                 # plot parameter distributions
round(standardized(mod1),3)                       # print standardized estimates in one table
simple_plot(wrkbehave ~ orgconstr | conscient, mod1) # plot conditional effects
jn_plot(wrkbehave ~ orgconstr | conscient, mod1) # plot conditional effects

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = workbeh,                                 # R data frame
  ordinal = 'workbeh1:workbeh6 orgcon1:orgcon6 consci1:consci6',  # binary and ordinal variables
  latent = 'orgconstr conscient wrkbehave',      # define latent variables
  model = '
    structural:                                   # model block label
    wrkbehave ~ orgconstr conscient orgconstr*conscient;  # latent product
    wrkbehave@1;                                  # fix residual variance to 1
    predictor:                                    # model block label
    orgconstr ~~ conscient;                      # latent correlation
    orgconstr@1; conscient@1;                    # fix variances to 1
    measurement:                                  # model block label
    wrkbehave -> workbeh1@wload1 workbeh2:workbeh6;  # measurement model with estimated loadings
    orgconstr -> orgcon1@oload1 orgcon2:orgcon6;  # measurement model with estimated loadings
    conscient -> consci1@cload1 consci2:consci6;; DEBUG: compact_output', # measurement model with estimated loadings           
  simple = 'orgconstr | conscient',               # conditional effects
  seed = 90291,                                   # random number seed
  burn = 20000,                                   # warm-up iterations
  nimps = 20                                      # save 20 imputed data sets
)

output(mod2)                                     # print output
distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot residuals

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
# FIGURE 6.7 ----
#------------------------------------------------------------------------------#

# make the plots
fig6_7A <- simple_plot(wrkbehave ~ orgconstr | conscient, mod1) 
fig6_7B <- jn_plot(wrkbehave ~ orgconstr | conscient, mod1)

# grey the non-significant region (NAMED values, so it maps correctly even when
# only one significance level is present in range)
fig6_7B <- fig6_7B +
  scale_fill_manual(values = c(`TRUE` = NA, `FALSE` = "grey80")) +  # TRUE=sig (no fill); FALSE=non-sig (grey)
  labs(subtitle = "Shaded area represents 0 within 95% interval\nBound: 1.5")

# linetype by moderator level (line layers only), black lines, drop CI ribbons
for (i in which(vapply(fig6_7A$layers,
                       function(l) inherits(l$geom, "GeomLine"), logical(1)))) {
  q <- fig6_7A$layers[[i]]$mapping[["colour"]]
  if (is.null(q)) q <- fig6_7A$mapping[["colour"]]
  fig6_7A$layers[[i]]$mapping[["linetype"]] <- q
}
fig6_7A$layers <- fig6_7A$layers[
  !vapply(fig6_7A$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1))
]
fig6_7A <- fig6_7A +
  scale_linetype_manual(
    values = c("dashed", "solid", "dotted"),               # middle = solid
    name   = "Latent Conscientiousness",
    labels = c("-1 SD", "Mean", "+1 SD")
  ) +
  scale_colour_manual(values = rep("black", 3)) +
  guides(colour = "none", fill = "none") +
  labs(subtitle = NULL)                           # drop "Centered variables: ..."

# force white panels + no grid on every subplot
fig6_7 <- (fig6_7A / fig6_7B) +
  plot_annotation(tag_levels = "A") &
  book_theme &
  caps_axes &                                              # <- apply uppercase axis names
  labs(title = NULL) &
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(colour = "black", linewidth = 0.5)
  )

save_fig(fig6_7, "Figure 6.7", width = 8.5, height = 11)
