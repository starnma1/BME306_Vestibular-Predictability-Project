### Script: Statistical Analysis ###
### Glossary:
  
  # Data Loading
  # Data Preperation
  # Model Fitting
  # Model Diagnostics
  # Prediction PLotting
  
################################################################################
# HYPOTHESIS
# This script tests whether the relationship between p11 (Markov chain transition
# probability) and StdBodyX (standardized body displacement, a proxy for balance
# disruption) is non-linear. The core hypothesis is that predictability at the
# extremes (p11 = 0: pure alternating, p11 = 1: pure single-direction) allows
# the vestibular system to anticipate and adapt to incoming pulses, resulting in
# smaller body displacement. At intermediate values (p11 = 0.5: maximum
# unpredictability), adaptation is impaired and displacement is largest. This
# predicts a upside down U-shaped curve with a peak at p11 = 0.5.
################################################################################

library(tidyverse)
library(glue)
library(glmmTMB)
library(performance)
library(ggeffects)
library(readxl)

################################################################################
# DATA LOADING
################################################################################

samples <- paste0("B", 1:20)

participant_info <- read_excel("Data/Participant_info.xlsx")

dat_all <- data.frame()
for (s in samples) {
  path <- glue("Data/{s}_*/processed/")
  files <- Sys.glob(file.path(path, "*_trialResults.xlsx"))
  for (f in files) {
    dat <- readxl::read_excel(f)
    dat$condition <- sub("_trialResults.xlsx", "", sub(glue(".*EC_{s}_"), "", basename(f)))
    dat$sample <- s
    dat_all <- bind_rows(dat_all, dat)
  }
}


# Finished and Prepped dataset
dat_all <- merge(dat_all, participant_info, by = "sample")

################################################################################
# DATA PREPARATION
################################################################################

dat_model <- dat_all %>%
  filter(Direction == 0) %>%
  mutate(p11 = case_when(
    grepl("only_forward", condition)  ~ 1,
    grepl("only_backward", condition) ~ 1,
    grepl("p11_0.75", condition)      ~ 0.75,
    grepl("p11_0.5", condition)       ~ 0.5,
    grepl("p11_0.25", condition)      ~ 0.25,
    grepl("p11_0$", condition)        ~ 0
  ))

################################################################################
# MODEL FITTING
################################################################################

null_fit   <-             glmmTMB(StdBodyX ~ 1,                                    data = dat_model, family = Gamma(link = "log"))
simple_fit <-             glmmTMB(StdBodyX ~ p11,                                  data = dat_model, family = Gamma(link = "log"))
linear_fit <-             glmmTMB(StdBodyX ~ p11 + (1 | sample),                   data = dat_model, family = Gamma(link = "log"))
linear_fit_additive <-    glmmTMB(StdBodyX ~ p11 + Height + (1 | sample),          data = dat_model, family = Gamma(link = "log"))
linear_fit_interactive <- glmmTMB(StdBodyX ~ p11 * Height + (1 | sample),          data = dat_model, family = Gamma(link = "log"))
poly_fit   <-             glmmTMB(StdBodyX ~ poly(p11, 2) + (1 | sample),          data = dat_model, family = Gamma(link = "log"))
poly_fit_additive   <-    glmmTMB(StdBodyX ~ poly(p11, 2) + Height + (1 | sample), data = dat_mode, family = Gamma(link = "log"))
poly_fit_interactive <-   glmmTMB(StdBodyX ~ poly(p11, 2) * Height + (1 | sample), data = dat_model, family = Gamma(link = "log"))
over_fit   <-             glmmTMB(StdBodyX ~ poly(p11, 4) + (1 | sample),          data = dat_model, family = Gamma(link = "log"))

## Using data but each samples median

median_data <- dat_model %>%
  group_by(sample, p11) %>%
  summarise(med = median(StdBodyX), Height = mean(Height))

median_null_fit   <-             glmmTMB(median ~ 1,                                    data = median_data, family = Gamma(link = "log"))
median_simple_fit <-             glmmTMB(median ~ p11,                                  data = median_data, family = Gamma(link = "log"))
median_linear_fit <-             glmmTMB(median ~ p11 + (1 | sample),                   data = median_data, family = Gamma(link = "log"))
median_linear_fit_additive <-    glmmTMB(median ~ p11 + Height + (1 | sample),          data = median_data, family = Gamma(link = "log"))
median_linear_fit_interactive <- glmmTMB(median ~ p11 * Height + (1 | sample),          data = median_data, family = Gamma(link = "log"))
median_poly_fit   <-             glmmTMB(median ~ poly(p11, 2) + (1 | sample),          data = median_data, family = Gamma(link = "log"))
median_poly_fit_additive   <-    glmmTMB(median ~ poly(p11, 2) + Height + (1 | sample), data = median_data, family = Gamma(link = "log"))
median_poly_fit_interactive <-   glmmTMB(median ~ poly(p11, 2) * Height + (1 | sample), data = median_data, family = Gamma(link = "log"))
median_over_fit   <-             glmmTMB(median ~ poly(p11, 4) + (1 | sample),          data = median_data, family = Gamma(link = "log"))


################################## ##############################################
# MODEL COMPARISON
################################################################################

anova(null_fit, simple_fit, linear_fit,
      linear_fit_additive, linear_fit_interactive,
      poly_fit, poly_fit_additive,poly_fit_interactive,
      over_fit, test = "Chisq")

compare_performance(null_fit, simple_fit, linear_fit,
                    linear_fit_additive, linear_fit_interactive,
                    poly_fit, poly_fit_additive,poly_fit_interactive,
                    over_fit)

# Now for just medians
anova(median_null_fit, median_simple_fit, median_linear_fit,
      median_linear_fit_additive, median_linear_fit_interactive,
      median_poly_fit, median_poly_fit_additive,median_poly_fit_interactive,
      median_over_fit, test = "Chisq")

summary(median_poly_fit_additive)

################################################################################
# MODEL DIAGNOSTICS (best supported model: linear_fit)
# We will not be continuing with the interactive model since it is not as medically
# sensible to assume the effect of predictability depends on the height of the 
# participant. 
################################################################################

summary(poly_fit)
check_model(poly_fit)
check_predictions(poly_fit)

################################################################################
# PREDICTION PLOT
################################################################################


prediction_plot <- function(raw_data, model, outcome_variable,
                            x_label = "Predictability",
                            y_label = "Outcome") {
  
  s <- seq(min(raw_data$p11), max(raw_data$p11), length.out = 200)
  new_dat <- predict_response(model, terms = "p11 [s]")
  
  ggplot() +
    geom_ribbon(
      data = new_dat,
      aes(x = x, ymin = conf.low, ymax = conf.high),
      fill = "#4E84C4", alpha = 0.15
    ) +
    geom_line(
      data = new_dat,
      aes(x = x, y = predicted),
      color = "#4E84C4", linewidth = 0.8
    ) +
    geom_point(
      data = raw_data,
      aes(x = p11, y = {{ outcome_variable }}),
      shape = 21,
      fill = "#4E84C4", color = "white",
      size = 2, stroke = 0.4,
      alpha = 0.7,
      position = position_jitter(width = 0.01, seed = 42)
    ) +
    labs(
      x = x_label,
      y = y_label
    ) +
    theme_classic(base_size = 11, base_family = "serif") +
    theme(
      axis.line        = element_line(linewidth = 0.4, color = "grey30"),
      axis.ticks       = element_line(linewidth = 0.4, color = "grey30"),
      axis.text        = element_text(color = "grey20", size = 10),
      axis.title       = element_text(color = "grey10", size = 11),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.margin      = margin(8, 12, 8, 8, "pt")
    )
}

# Call the function to view the plot
prediction_plot(
  median_data, poly_fit_additive, med,
  x_label = "Predictability Score (p11, p00)",
  y_label = "Median Body Displacement (StdBodyX)"
)

# Save the plot
ggsave(
  "plots/non_linearity_prediction.pdf",
  width = 88, height = 85, units = "mm",
)
