# LINEAR REGRESSION

# plotting functions
# source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(ggplot2)
library(patchwork)
library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')
# set_blimp('/applications/blimp/blimp')

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/inflammation.csv'

# create data frame from github data
inflammation <- read.csv(data_url)

#------------------------------------------------------------------------------#
# LINEAR REGRESSION MODEL ----
#------------------------------------------------------------------------------#

# linear regression
mod1 <- rblimp(
  data = inflammation,             		             # R data frame
  ordinal = 'els female',          		             # binary and ordinal variables         
  center = 'inflam els female age',	               # center predictors  
	model = 'dpdd ~ inflam els female age;',         # regression model
  seed = 90291,               		                 # random number seed
  burn = 10000,               		                 # warm-up iterations
  iter = 10000)                		                 # analysis iteration
      
output(mod1)               		                     # print output

#------------------------------------------------------------------------------#
# CUSTOM WALD TEST ----
#------------------------------------------------------------------------------#

# method 1: label parameters
mod2 <- rblimp(
  data = inflammation,             		             # R data frame
  ordinal = 'els female',          		             # binary and ordinal variables         
  center = 'inflam els female age',	               # center predictors  
  model = 'dpdd ~ inflam@b1 els@b2 female age;',   # labeled slopes
  seed = 90291,               		                 # random number seed
  waldtest = 'b1 = 0; b2 = 0',                     # wald test
  burn = 10000,               		                 # warm-up iterations
  iter = 10000)                		                 # analysis iteration

output(mod2)               		                     # print output

# method 2: label parameters
mod3 <- rblimp(
  data = inflammation,             		             # R data frame
  ordinal = 'els female',          		             # binary and ordinal variables         
  center = 'inflam els female age',	               # center predictors  
  model = 'dpdd ~ inflam els female age',          # regression model
  seed = 90291,               		                 # random number seed
  waldtest = 'dpdd ~ female age',                  # wald test as nested model
  burn = 10000,               		                 # warm-up iterations
  iter = 10000)                		                 # analysis iteration

output(mod3)               		                     # print output

# multiple wald tests
mod4 <- rblimp(
  data = inflammation,             		                    # R data frame
  ordinal = 'els female',          		                    # binary and ordinal variables         
  center = 'inflam els female age',	                      # center predictors  
  model = 'dpdd ~ inflam@b1 els@b2 female@b3 age@b4;',    # labeled slopes
  waldtest = c('b1:b2 = 0', 'b3:b4 = 0'),                 # vector of tests
  seed = 90291,               		                        # random number seed
  burn = 10000,               		                        # warm-up iterations
  iter = 10000)                		                        # analysis iteration
             
output(mod4)               		                            # print output
            		          

#------------------------------------------------------------------------------#
# GRAPHICAL DIAGNOSTICS WITH MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# save multiply imputed data sets
mod5 <- rblimp(
  data = inflammation,             		       # R data frame
  ordinal = 'els female',          		       # binary and ordinal variables         
  center = 'inflam els female age',	         # center predictors  
  model = 'dpdd ~ inflam els female age;',   # regression model
  seed = 90291,               		           # random number seed
  burn = 10000,               		           # warm-up iterations
  iter = 10000,                              # analysis iterations
  nimps = 20)                                # save 20 imputed data sets                		           

output(mod5)

# plot distributions
posterior_plot(mod5)
distribution_plot(mod5)
residuals_plot(mod5)

#------------------------------------------------------------------------------#
# BOOK FIGURE THEME ----
#------------------------------------------------------------------------------#

book_theme <- ggplot2::theme(
  text              = ggplot2::element_text(family = "Minion Pro"),
  axis.text         = ggplot2::element_text(color = "black"),
  axis.line         = ggplot2::element_line(color = "black", linewidth = 0.5),
  axis.ticks        = ggplot2::element_line(color = "black", linewidth = 0.5),
  axis.ticks.length = grid::unit(4, "pt"),
  plot.tag          = ggplot2::element_text(face = "bold", size = 22),
  legend.position   = "bottom"
)

#------------------------------------------------------------------------------#
# FIGURE 2.2: TRACE PLOT ----
#------------------------------------------------------------------------------#

# Fit single-chain model for trace plot diagnostic
mod_1chain <- rblimp(
  data    = inflammation,
  ordinal = 'els female',
  center  = 'inflam els female age',
  model   = 'dpdd ~ inflam els female age',
  seed    = 90291,
  chains  = 1,
  burn    = 10000,
  iter    = 10000
)
output(mod_1chain)

# Build trace plot
fig2_2 <- trace_plot(mod_1chain, 6) +
  ggplot2::xlim(0, 100) +
  ggplot2::scale_color_manual(values = "black") +
  ggplot2::ylab("Slope Parameter") +
  ggplot2::labs(title = NULL, subtitle = NULL) +
  ggplot2::theme_classic(base_size = 14, base_family = "Minion Pro") +
  book_theme +
  ggplot2::theme(
    text            = ggplot2::element_text(size = 20),
    axis.title      = ggplot2::element_text(size = 20),
    axis.text       = ggplot2::element_text(size = 20),
    legend.position = "none"
  )

ggplot2::ggsave(
  filename = "~/desktop/Figure 2.2.pdf",
  plot     = fig2_2,
  width    = 11,
  height   = 8.5,
  units    = "in",
  device   = cairo_pdf
)

#------------------------------------------------------------------------------#
# FIGURE 2.6: DISTRIBUTIONS ----
#------------------------------------------------------------------------------#

dp <- distribution_plot(
  mod5,
  observed_color = "grey60",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 16,
  line_width     = 0.5
)

fig2_6 <- (dp$dpdd / dp$inflam) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  book_theme &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 2.6.pdf",
  plot     = fig2_6,
  width    = 8.5,
  height   = 11,
  units    = "in",
  device   = cairo_pdf
)

#------------------------------------------------------------------------------#
# FIGURE 2.7: RESIDUAL DISTRIBUTION + STANDARDIZED RESIDUAL INDEX ----
#------------------------------------------------------------------------------#

# Residual distribution in B&W
dp <- distribution_plot(
  mod5,
  vars           = "dpdd.residual",
  observed_color = "grey40",
  imputed_color  = "grey40",
  density_color  = "black",
  font_size      = 20,
  line_width     = 0.6
)

# Residual diagnostics in B&W
rp <- residuals_plot(
  mod5,
  point_color  = "grey40",
  curve_color  = "black",
  font_size    = 20,
  line_width   = 0.6,
  label_family = "Minion Pro"
)

# 2-panel vertical stack: residual distribution above standardized residual index
fig2_7 <- dp$dpdd.residual / rp$dpdd.index +
  plot_annotation(tag_levels = "A") &
  book_theme &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 2.7.pdf",
  plot     = fig2_7,
  width    = 8.5,
  height   = 11,
  units    = "in",
  device   = cairo_pdf
)

#------------------------------------------------------------------------------#
# FIGURE 2.8: RESIDUAL VS. PREDICTED + RESIDUAL VS. PREDICTORS ----
#------------------------------------------------------------------------------#

# Reuses rp from above; rerun residuals_plot() here if running this block alone.

fig2_8 <- rp$dpdd.predicted / rp$dpdd.inflam / rp$dpdd.age +
  plot_annotation(tag_levels = "A") &
  book_theme &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 2.8.pdf",
  plot     = fig2_8,
  width    = 8.5,
  height   = 11,
  units    = "in",
  device   = cairo_pdf
)

#------------------------------------------------------------------------------#
# FIGURE 2.9: COOK'S D AND LEVERAGE ----
#------------------------------------------------------------------------------#

# Reuses rp from above; rerun residuals_plot() here if running this block alone.

fig2_9 <- rp$dpdd.leverage / rp$dpdd.cooks +
  plot_annotation(tag_levels = "A") &
  book_theme &
  ggplot2::labs(title = NULL)

ggplot2::ggsave(
  filename = "~/desktop/Figure 2.9.pdf",
  plot     = fig2_9,
  width    = 11,
  height   = 8.5,
  units    = "in",
  device   = cairo_pdf
)