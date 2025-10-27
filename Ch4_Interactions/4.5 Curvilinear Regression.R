# CURVILINEAR REGRESSION

# LOAD R PACKAGES ----

library(rblimp)
library(psych)
library(summarytools)

# READ DATA ----

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/math.csv'

# create data frame from github data
math <- read.csv(data_url)

# FIT MODEL ----

# curvilinear regression
model <- rblimp(
    data = math,
    ordinal = 'frlunch male',
    fixed = 'male mathpre',
    center = 'anxiety',
    model = 'mathpost ~ anxiety anxiety^2 frlunch mathpre male',
    seed = 12345,
    burn = 10000,
    iter = 10000)

output(model)

# ADD SOME DISPLAY TO PLOT CURVILINEAR EFFECT ----

library(ggplot2)

ggplot(model@average_imp, aes(x = anxiety, y = mathpost.predicted)) +
  geom_point(color = "grey60", size = 2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE),
              color = "blue", linewidth = 1.3, se = FALSE) +
  labs(x = "Anxiety",
       y = "Predicted Math Score",
       title = "Quadratic Fit: Predicted Math Achievement by Anxiety") +
  theme_minimal()

