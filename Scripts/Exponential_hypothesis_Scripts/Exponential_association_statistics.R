## This is the statistical analysis of the learning effect for only forward ##

###############################################################################
# HYPOTHESIS
# To evaluate whether a learning effect was present during repeated platform 
# perturbations, we analyzed postural deviation across the first 10 pulses separately
# for forward and backward platform movements. Postural deviation was quantified as
# the absolute deviation from normal posture in standard deviations, averaged across
# subjects for each pulse. We hypothesized that the first perturbation would elicit
# the largest postural response, with deviation decreasing systematically over
# subsequent pulses as subjects adapted to the repeated stimulus. To test whether
# this attenuation was statistically significant, a linear mixed model was fitted
# with pulse number as a continuous predictor, direction (forward vs. backward) as
# a fixed factor, and their interaction to assess whether the rate of adaptation
# differed between directions. Subject was included as a random effect with a random
# intercept and slope over pulse to account for individual differences in baseline
# posture and learning rate.
###############################################################################

library(tidyverse)
library(glue)
library(glmmTMB)
library(performance)
library(ggeffects)
library(readxl)

###############################################################################
# DATA LOADING
###############################################################################

# Complete data frame
path <- "Data/"
all_data <- list()

for (i in 1:20) {
  sample_id        <- glue("B{i}")
  folder           <- Sys.glob(glue("{path}{sample_id}_*"))
  processed_folder <- file.path(folder, "processed")
  
  files <- list.files(processed_folder,
                      pattern = "(only_forward|only_backward).*_trialResults\\.xlsx$",
                      full.names = TRUE)
  
  for (f in files) {
    obj_name  <- sub("_trialResults\\.xlsx$", "", basename(f))
    direction <- ifelse(grepl("only_forward", basename(f)), "forward", "backward")
    
    dat            <- read_xlsx(f)
    dat            <- dat[1:10, ]
    dat$sample     <- sample_id
    dat$direction  <- direction
    dat$pulse      <- 1:10
    
    all_data[[length(all_data) + 1]] <- dat
  }
}

# Final ready dataset for analysis
dd <- bind_rows(all_data)

glimpse(dd)

###############################################################################
# DATA EXPLORATION (QUICK)
###############################################################################

ggplot(data = dd) + 
  geom_density(aes(x = StdBodyX, group = Direction, col = factor(Direction))) + 
  facet_wrap(~sample)
  
  # This suggests a underlying gamma distribution

###############################################################################
# MODEL FITTING
###############################################################################

mod.null <- glmmTMB(StdBodyX ~ 1, data = dd, family = Gamma(link = "log"))
mod.only.subject <- glmmTMB(StdBodyX ~ 1 + (1| sample), data = dd, family = Gamma(link = "log"))
mod.simple <- glmmTMB(StdBodyX ~ pulse + (1|sample), data = dd, family = Gamma(link = "log"))
mod.simple.additive <- glmmTMB(StdBodyX ~ pulse + direction + (1|sample), data = dd, family = Gamma(link = "log"))
mod.simple.interactive <- glmmTMB(StdBodyX ~ pulse * direction + (1|sample), data = dd, family = Gamma(link = "log"))


# The log link already models exponential decay, but log(pulse) as predictor
# tests whether the decay follows a power law (faster initial drop, slower tail)
# which is a common alternative to pure exponential
mod.exp.additive <- glmmTMB(StdBodyX ~ log(pulse) + direction + (1|sample),
                            data = dd, family = Gamma(link = "log"))

mod.exp.interactive <- glmmTMB(StdBodyX ~ log(pulse) * direction + (1|sample),
                               data = dd, family = Gamma(link = "log"))

# Random slope over log(pulse) per subject — allows individuals to differ
# in how fast they adapt, not just where they start
mod.exp.random.slope <- glmmTMB(StdBodyX ~ log(pulse) * direction + (1 + log(pulse) | sample),
                                data = dd, family = Gamma(link = "log"))

# Model comparison
anova(mod.null,
    mod.only.subject,
    mod.simple,
    mod.simple.additive,
    mod.simple.interactive,
    mod.exp.additive,
    mod.exp.interactive,
    mod.exp.random.slope, 
    test = "Chisq")


# Final model
model <- mod.exp.interactive

###############################################################################
# MODEL DIAGNOSITCS
###############################################################################

check_model(model)
check_predictions(model)
summary(model)

###############################################################################
# PREDICTION PLOT 
###############################################################################

s <- seq(min(dd$pulse), max(dd$pulse), length.out = 100)
new_data <- predict_response(model, terms = c("pulse [s]", "direction"))

direction_colors <- c("backward" = "#4E84C4", "forward" = "#C45E4E")
direction_labels <- c("backward" = "Backward", "forward" = "Forward")

ggplot() +
  geom_ribbon(
    data = new_data,
    aes(x = x, ymin = conf.low, ymax = conf.high, fill = group),
    alpha = 0.15
  ) +
  geom_line(
    data = new_data,
    aes(x = x, y = predicted, color = group),
    linewidth = 0.8
  ) +
  scale_color_manual(values = direction_colors, labels = direction_labels) +
  scale_fill_manual(values = direction_colors, labels = direction_labels) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x     = "Pulse number",
    y     = "Postural deviation (SD)",
    color = "Direction",
    fill  = "Direction"
  ) +
  theme_classic(base_size = 11, base_family = "serif") +
  theme(
    axis.line        = element_line(linewidth = 0.4, color = "grey30"),
    axis.ticks       = element_line(linewidth = 0.4, color = "grey30"),
    axis.text        = element_text(color = "grey20", size = 10),
    axis.title       = element_text(color = "grey10", size = 11),
    legend.position  = "top",
    legend.title     = element_text(size = 10),
    legend.text      = element_text(size = 10),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.margin      = margin(8, 12, 8, 8, "pt")
  )

# Save the plot
ggsave(
  "plots/exponential_forward_backward_adaptability.pdf",
  width = 88, height = 85, units = "mm",
)
