### Script: Statistical Analysis ###
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
#
# DATA & MODEL STRUCTURE
# Data are trial-level StdBodyX observations from samples B1 to B20, separated
# by pulse direction (forward: Direction == 1, backward: Direction == 0).
# A random intercept per sample (1 | sample) accounts for between-subject
# variability. Five models of increasing complexity were compared via likelihood
# ratio tests using anova():
#
#   null_fit:   intercept only, no effect of p11
#   simple_fit: fixed linear effect of p11, no random effects
#   linear_fit: fixed linear effect of p11 + random intercept per sample
#   poly_fit:   fixed quadratic effect of p11 + random intercept per sample
#   over_fit:   fixed 4th degree polynomial + random intercept per sample
#
# MODEL COMPARISON RESULTS
#                        Df   AIC   BIC  logLik deviance    Chisq Chi Df Pr(>Chisq)    
# null_fit                2 12136 12148 -6066.0    12132                               
# simple_fit              3 12134 12152 -6063.8    12128   4.5242      1   0.033419 *  
# linear_fit              4 11858 11883 -5925.2    11850 277.1857      1  < 2.2e-16 ***
# linear_fit_additive     5 11857 11888 -5923.5    11847   3.2706      1   0.070534 .  
# poly_fit                5 11860 11891 -5925.1    11850   0.0000      0   1.000000    
# linear_fit_interactive  6 11854 11891 -5920.7    11842   8.6712      1   0.003233 ** 
# poly_fit_additive       6 11859 11896 -5923.4    11847   0.0000      0   1.000000    
# over_fit                7 11858 11901 -5921.9    11844   3.1775      1   0.074658 .  
# poly_fit_interactive    8 11857 11907 -5920.6    11841   2.4377      1   0.118446    

# CONCLUSION
# The analysis does not support the hypothesis of a U-shaped relationship
# between p11 and balance disruption. The dominant source of variance is
# between-subject differences rather than the p11 manipulation. The best
# supported model is the linear mixed model (linear_fit), suggesting at most
# a weak linear trend of p11 on StdBodyX after accounting for subject-level
# baseline differences.
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

samples <- paste0("B", 1:17)

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
  filter(Direction == 1) %>%
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

null_fit   <-             glmmTMB(StdBodyX ~ 1,                                    data = dat_model)
simple_fit <-             glmmTMB(StdBodyX ~ p11,                                  data = dat_model)
linear_fit <-             glmmTMB(StdBodyX ~ p11 + (1 | sample),                   data = dat_model)
linear_fit_additive <-    glmmTMB(StdBodyX ~ p11 + Height + (1 | sample),          data = dat_model)
linear_fit_interactive <- glmmTMB(StdBodyX ~ p11 * Height + (1 | sample),          data = dat_model)
poly_fit   <-             glmmTMB(StdBodyX ~ poly(p11, 2) + (1 | sample),          data = dat_model)
poly_fit_additive   <-    glmmTMB(StdBodyX ~ poly(p11, 2) + Height + (1 | sample), data = dat_model)
poly_fit_interactive <-   glmmTMB(StdBodyX ~ poly(p11, 2) * Height + (1 | sample), data = dat_model)
over_fit   <-             glmmTMB(StdBodyX ~ poly(p11, 4) + (1 | sample),          data = dat_model)

################################## ##############################################
# MODEL COMPARISON
################################################################################

anova(null_fit, simple_fit, linear_fit,
      linear_fit_additive, linear_fit_interactive,
      poly_fit, poly_fit_additive,poly_fit_interactive,
      over_fit, test = "Chisq")
compare_performance(null_fit, simple_fit, linear_fit, poly_fit, over_fit)

################################################################################
# MODEL DIAGNOSTICS (best supported model: linear_fit)
# We will not be continuing with the interactive model since it is not as medically
# sensible to assume the effect of predictability depends on the height of the 
# participant. 
################################################################################

summary(linear_fit)
check_model(linear_fit)
check_predictions(linear_fit)
#plot(predict_response(linear_fit, terms = "p11"))

s <- seq(min(dat_model$p11), max(dat_model$p11), length.out = 100)

new_dat <- predict_response(linear_fit, terms = "p11 [0, 0.25, 0.5, 0.75, 1]")

ggplot() + 
  geom_point(data = dat_model, aes(x = p11, y = StdBodyX, colour = sample),
             position = position_jitter(width = .4)) + 
  geom_line(data = new_dat, aes(x = x, y = predicted),linewidth = 5) + 
  scale_y_continuous(limits = c(0, 10))


