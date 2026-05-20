## Script for making graph with all the people on it to identify
## people who acc have the polynomial fit, and people who dont

################################################################################

library(tidyverse)
library(glue)
library(glmmTMB)
library(performance)
library(ggeffects)
library(readxl)

################################################################################

# Data Loading
dd <- read.csv("Data_for_stats/model_dataset_backward.csv")

# Plotting

graph <- function(dd){
  dat_good <- filter(dd, sample %in% c("B17", "B3", "B2"))
  dat_bad  <- filter(dd, sample %in% c("B5", "B6", "B7", "B8", "B9", "B10",
                                       "B11", "B12", "B13", "B14", "B15", "B16"))
  ggplot() +
    geom_smooth(data = dat_bad, aes(x = p11, y = StdBodyX, group = sample),
                method = "lm", formula = y ~ poly(x, 2), se = FALSE,
                color = "grey75", linewidth = 0.4, alpha = 0.6) +
    geom_smooth(data = dat_good, aes(x = p11, y = StdBodyX, col = sample),
                method = "lm", formula = y ~ poly(x, 2), se = FALSE,
                linewidth = 1.5) +
    theme_bw() +
    labs(x = "p11", y = "StdBodyX") +
    theme(strip.text = element_text(size = 8))
    #scale_y_continuous(limits = c(0.5, 2))
}
gg <- graph(dd)
gg
ggsave("plots/Composite_exploration_backwards.png", gg, width = 6, height = 4)
