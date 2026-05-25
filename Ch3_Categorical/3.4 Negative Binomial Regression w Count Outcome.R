# NEGATIVE BINOMIAL REGRESSION FOR COUNT OUTCOMES

# plotting functions
# source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source("/Users/craig/Documents/Claude/Projects/Blimp Book/rblimp_cleaned_functions.R")

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(ggplot2)
library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflammation <- read.csv(data_url)

#------------------------------------------------------------------------------#
# NEGATIVE BINOMIAL REGRESSION MODEL ----
#------------------------------------------------------------------------------#

mod1 <- rblimp(
  data = inflammation,             		             # R data frame
  ordinal = 'els female',          		             # binary and ordinal variables 
  count = 'hdd',                                   # count outcome with negative binomial model
  center = 'inflam age',	                         # center predictors  
  model = 'hdd ~ inflam els female age',           # regression model
  seed = 90291,               		                 # random number seed
  burn = 10000,               		                 # warm-up iterations
  iter = 10000)                		                 # analysis iteration

output(mod1)               		                     # print output
posterior_plot(mod1)                               # plot parameter distributions

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

mod2 <- rblimp(
  data = inflammation,             		             # R data frame
  ordinal = 'els female',          		             # binary and ordinal variables 
  count = 'hdd',                                   # count outcome with negative binomial model
  center = 'inflam els female age',	               # center predictors  
  model = 'hdd ~ inflam els female age;',          # regression model
  seed = 90291,               		                 # random number seed
  burn = 10000,               		                 # warm-up iterations
  iter = 10000,                                    # analysis iteration
  nimps = 20)                		                   # save 20 imputations  

output(mod2)               		                     # print output

distribution_plot(mod2)                          # plot observed and imputed distributions
residuals_plot(mod2)                             # plot standardized Pearson residuals

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
# FIGURE 3.8: OVERLAY DISTRIBUTIONS ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(
  mod2,
  observed_color = "grey60",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 18,
  line_width     = 0.5
)

fig3_8 <- dp$hdd +
  plot_layout(guides = "collect") +
  plot_annotation() &
  book_theme &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 3.8.pdf",
  plot     = fig3_8,
  width    = 11,
  height   = 8.5,
  units    = "in",
  device   = cairo_pdf
)
