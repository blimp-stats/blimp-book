# INTERACTION INVOLVING A CONTINUOUS MODERATOR

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
# LINEAR REGRESSION MODEL ----
#------------------------------------------------------------------------------#

# comment
mod0 <- rblimp(
  data = reading, 
  ordinal = 'esl',
  center = 'read1 lrnprob1',
  model = 'read9 ~ read1 lrnprob1 esl',  
  seed = 90291,                                              
  burn = 10000,                                              
  iter = 10000,
  nimps = 20)   

output(mod0)                                     # print output

#------------------------------------------------------------------------------#
# MODERATED REGRESSION ----
#------------------------------------------------------------------------------#

# comment
mod1 <- rblimp(
  data = reading, 
  ordinal = 'esl',
  center = 'read1 lrnprob1',
  model = 'read9 ~ read1 lrnprob1 read1*lrnprob1 esl',  
  seed = 90291,                                              
  burn = 10000,                                              
  iter = 10000)                                                

output(mod1)                                     # print output
posterior_plot(mod1, 'read9')               # plot parameter distributions


#------------------------------------------------------------------------------#
# PROBING INTERACTION ----
#------------------------------------------------------------------------------#

# comment
mod2 <- rblimp(
  data = reading, 
  ordinal = 'esl',
  center = 'read1 lrnprob1',
  model = 'read9 ~ read1 lrnprob1 read1*lrnprob1 esl; DEBUG: compact_output',  
  simple = 'read1 | lrnprob1',      
  seed = 90291,                                              
  burn = 10000,                                              
  iter = 10000)                                                

# print output
output(mod2)

# plot conditional effects and johnson-neyman regions of significance
simple_plot(read9 ~ read1 | lrnprob1, mod2)
jn_plot(read9 ~ read1 | lrnprob1, mod2)

# comment
mod3 <- rblimp(
  data = reading, 
  ordinal = 'esl',
  center = 'read1 lrnprob1',
  model = 'read9 ~ read1 lrnprob1 read1*lrnprob1 esl; DEBUG: compact_output',  
  simple = 'read1 | lrnprob1 @ quantile',      
  seed = 90291,                                              
  burn = 10000,                                              
  iter = 10000)                                                

# print output
output(mod3)
simple_plot(read9 ~ read1 | lrnprob1, mod3)

#------------------------------------------------------------------------------#
# BOOK FIGURE THEME ----
#------------------------------------------------------------------------------#

library(patchwork)

book_theme <- ggplot2::theme(
  text              = ggplot2::element_text(family = "Minion Pro", size = 18),
  axis.text         = ggplot2::element_text(color = "black", size = 18),
  axis.line         = ggplot2::element_line(color = "black", linewidth = 0.5),
  axis.ticks        = ggplot2::element_line(color = "black", linewidth = 0.5),
  axis.ticks.length = grid::unit(4, "pt"),
  legend.text       = ggplot2::element_text(size = 18),
  legend.title      = ggplot2::element_text(size = 18),
  plot.tag          = ggplot2::element_text(face = "bold", size = 22),
  legend.position   = "bottom"
)

#------------------------------------------------------------------------------#
# FIGURE 4.2 ----
#------------------------------------------------------------------------------#

# --- make the plots ---
fig4_2A <- simple_plot(read9 ~ read1 | lrnprob1, mod2)
fig4_2B <- jn_plot(read9 ~ read1 | lrnprob1, mod2)

# --- Panel B: grey the non-significant region; reword subtitle ---
fig4_2B <- fig4_2B +
  ggplot2::scale_fill_manual(values = c("grey80", NA)) +   # FALSE = non-sig = grey; TRUE = sig = none
  ggplot2::labs(subtitle = "Shaded area represents 0 within 95% interval\nBound: -24")

# --- Panel A: linetype by moderator level (line layers only), black lines, drop CI ribbons ---
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

# --- compose; FORCE white panels + no grid on every subplot (applied last) ---
fig4_2 <- (fig4_2A / fig4_2B) +
  plot_annotation(tag_levels = "A") &
  book_theme &
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