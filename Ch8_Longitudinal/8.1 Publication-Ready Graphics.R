# INTERACTION INVOLVING A CONTINUOUS PREDICTOR AND A CONTINUOUS MODERATOR

# plotting functions
# source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source("/Users/craig/Dropbox/Claude/Projects/Blimp Book/rblimp_cleaned_functions.R")

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(ggplot2)
library(rblimp)

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

mod1 <- rblimp(
  data = reading,                                # R data frame
  ordinal = 'esl',                               # binary and ordinal variables
  center = 'read1 lrnprob',                      # center predictors
  model = 'read9 ~ read1 lrnprob read1*lrnprob esl',  # product term
  simple = 'read1 | lrnprob',                    # conditional effects of read1 at SD units
  seed = 90291,                                  # random number seed
  burn = 10000,                                  # warm-up iterations
  iter = 10000)                                  # analysis iterations

simple_plot(read9 ~ read1 | lrnprob, mod1)       # plot conditional effects

#------------------------------------------------------------------------------#
# CONVERT TO PUBLICATION-READY GRAPHICS ----
#------------------------------------------------------------------------------#

library(ggplot2)

plot <- simple_plot(read9 ~ read1 | lrnprob, mod1)

# remove CI bands if desired
# plot$layers <- Filter(\(l) !inherits(l$geom, "GeomRibbon"), plot$layers)

plot <- plot +
  # maps the moderator values to line type
  aes(linetype = lrnprob) +    
  # change all regression lines to black and removes the color legend
  scale_color_manual(values = rep("black", 3), guide = "none") +
  # change CI bands to grey
  scale_fill_manual(
    values = c("grey80", "grey60", "grey80"),
    guide = "none"
  ) +
  # define the line types and provide legend labels
  scale_linetype_manual(
    values = c("dashed", "solid", "dotted"),
    name = "Learning Problems",
    labels = c("-1 SD", "Mean", "+1 SD")
  ) +
  # custom axis limits
  coord_cartesian(xlim = c(-40, 40), ylim = c(60, 120)) +
  # axis labels
  labs(
    title = NULL,
    subtitle = NULL,
    x = "First-Grade Reading (Centered)",
    y = "Ninth-Grade Reading"
  ) +
  # replace gray background with black-and-white theme and change the font
  theme_classic(base_family = "Times New Roman")

# view plot
print(plot)

# save plot as PDF
ggsave('~/desktop/simpleplot.pdf', plot = plot, width = 8.5, height = 11,
       units = "in", device = cairo_pdf)
