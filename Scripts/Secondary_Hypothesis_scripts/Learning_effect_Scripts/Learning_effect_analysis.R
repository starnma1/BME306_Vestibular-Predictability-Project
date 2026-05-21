### Script: Learning Curve Analysis ###
### Glossary:

# Data Loading
# Data Preparation
# Model Fitting
# Model Diagnostics
# Prediction Plotting

################################################################################
# HYPOTHESIS
# This script tests whether postural deviation decreases systematically across
# repeated experimental sessions. The core hypothesis is that the first session
# produces the highest median postural deviation (StdBodyX), as participants
# are naive to the perturbation paradigm. As sessions progress, participants
# develop an internal model of the expected disturbance, allowing the
# vestibular and motor systems to anticipate and counteract incoming pulses
# more effectively. This predicts a monotonically decreasing relationship
# between session number and median StdBodyX, consistent with a learning or
# habituation effect across experiments.
################################################################################

library(tidyverse)
library(glue)
library(glmmTMB)
library(performance)
library(ggeffects)
library(readxl)
library(ggeffects)
library(ggpubr)

################################################################################
# DATA LOADING + CLEAN
################################################################################

participant_info <- read_excel("Data/Participant_info.xlsx")

dat_full <- read.csv("Created_Datasets/learning_curve_data.csv")
dat_first_10 <- read.csv("Created_Datasets/learning_curve_data_only_first10.csv")

dat_full <- merge(dat_full, participant_info, by = "sample")
dat_first_10 <- merge(dat_first_10, participant_info, by = "sample")

table(dat_full$direction)

str(dat_full)
str(dat_first_10)

data_wrangling <- function(data){
  data$trial <- as.factor(data$trial)
  data$sample <- as.factor(data$sample)
  data$direction <- factor(data$direction, levels = c(0,1), labels = c("Backward", "Forward"))
  return(data)
}

dat_full <- data_wrangling(dat_full)
dat_first_10 <- data_wrangling(dat_first_10)

table(dat_full$direction)

################################################################################
# DATA EXPLORATION (QUICK)
################################################################################

ggarrange(
  ggplot(data = dat_full) + 
  geom_density(aes(x = median_body_x, group = trial, col = trial)) + 
  facet_wrap(~direction),
  ggplot(data = dat_first_10) + 
    geom_density(aes(x = median_body_x, group = trial, col = trial)) + 
    facet_wrap(~direction),
  common.legend = TRUE,
  legend = "bottom"
)

  # Suggests a gamma distribution, so i will use it...especially cause small sample size.

################################################################################
# MODEL FITTING
################################################################################

model_fitting <- function(dataset) {
  model.null                    <- glmmTMB(median_body_x ~ 1,
                                           data = dataset, family = Gamma(link = "log"))
  model.simple.intercept        <- glmmTMB(median_body_x ~ 1 + (1|sample),
                                           data = dataset, family = Gamma(link = "log"))
  model.simple.predictor        <- glmmTMB(median_body_x ~ trial + (1|sample),
                                           data = dataset, family = Gamma(link = "log"))
  model.simple.additive         <- glmmTMB(median_body_x ~ trial + direction + (1|sample),
                                           data = dataset, family = Gamma(link = "log"))
  model.simple.interactive      <- glmmTMB(median_body_x ~ trial * direction + (1|sample),
                                           data = dataset, family = Gamma(link = "log"))
  model.exp.predictor           <- glmmTMB(median_body_x ~ log(as.numeric(trial)) + (1|sample),
                                           data = dataset, family = Gamma(link = "log"))
  model.exp.additive            <- glmmTMB(median_body_x ~ log(as.numeric(trial)) + direction + (1|sample),
                                           data = dataset, family = Gamma(link = "log"))
  model.exp.interactive         <- glmmTMB(median_body_x ~ log(as.numeric(trial)) * direction + (1|sample),
                                           data = dataset, family = Gamma(link = "log"))
  model.exp.additive.height     <- glmmTMB(median_body_x ~ log(as.numeric(trial)) + direction + Height + (1|sample),
                                           data = dataset, family = Gamma(link = "log"))
  model.exp.interactive.height  <- glmmTMB(median_body_x ~ log(as.numeric(trial)) * direction + Height + (1|sample),
                                           data = dataset, family = Gamma(link = "log"))
  
  models <- list(
    null                   = model.null,
    simple.intercept       = model.simple.intercept,
    simple.predictor       = model.simple.predictor,
    simple.additive        = model.simple.additive,
    simple.interactive     = model.simple.interactive,
    exp.predictor          = model.exp.predictor,
    exp.additive           = model.exp.additive,
    exp.interactive        = model.exp.interactive,
    exp.additive.height    = model.exp.additive.height,
    exp.interactive.height = model.exp.interactive.height
  )

  print(anova(model.null, model.simple.intercept, model.simple.predictor,
              model.simple.additive, model.simple.interactive,
              model.exp.predictor, model.exp.additive, model.exp.interactive,
              model.exp.additive.height, model.exp.interactive.height))
  return(models)
}


# Fit all models and inspect likelyhood ratios
all_models_full <- model_fitting(dat_full)
all_models_first_10 <- model_fitting(dat_first_10)

# Manually select the best
best_model_full <- all_models_full$exp.interactive.height
best_model_first_10 <- all_models_first_10$exp.additive.height


################################################################################
# MODEL DIAGNOSTICS
################################################################################
check_model(best_model_full)
check_model(best_model_first_10)

summary(best_model_full)
summary(best_model_first_10)

  # Both models look good, no major issues with residuals or overdispersion.

################################################################################
# PREDICTION PLOTS
################################################################################


direction_colors <- c("Backward" = "#4E84C4", "Forward" = "#C45E4E")

new_data_full <- predict_response(best_model_full,
                                  terms = c("trial", "direction"),
                                  bias_correction = TRUE)

new_data_first_10 <- predict_response(best_model_first_10,
                                      terms = c("trial", "direction"),
                                      bias_correction = TRUE)

plot_prediction <- function(new_data, raw_data, title) {
  new_data$x <- as.numeric(as.character(new_data$x))
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
    geom_point(
      data = raw_data,
      aes(x = as.numeric(as.character(trial)), y = median_body_x, color = direction),
      alpha = 0.4, size = 1.5,
      position = position_jitter(width = 0.05, seed = 42)
    ) +
    scale_color_manual(values = direction_colors) +
    scale_fill_manual(values = direction_colors) +
    scale_x_continuous(breaks = 1:6) +
    labs(
      title  = title,
      x      = "Trial number",
      y      = "Median Body Displacement (SD)",
      color  = "Direction",
      fill   = "Direction"
    ) +
    theme_classic(base_size = 11, base_family = "serif") +
    theme(
      axis.line        = element_line(linewidth = 0.4, color = "grey30"),
      axis.ticks       = element_line(linewidth = 0.4, color = "grey30"),
      axis.text        = element_text(color = "grey20", size = 10),
      axis.title       = element_text(color = "grey10", size = 11),
      plot.title       = element_text(size = 11, face = "italic"),
      legend.position  = "top",
      legend.title     = element_text(size = 10),
      legend.text      = element_text(size = 10),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.margin      = margin(8, 12, 8, 8, "pt")
    )
}

p_full <- plot_prediction(new_data_full, dat_full,
                          title = "All pulses per session")
p_first_10 <- plot_prediction(new_data_first_10, dat_first_10,
                              title = "First 10 pulses per session")

ggarrange(p_full, p_first_10,
          common.legend = TRUE,
          legend = "bottom")

#ggsave(
#  "plots/learning_curve.pdf",
#  width = 180, height = 90, units = "mm"
#)
