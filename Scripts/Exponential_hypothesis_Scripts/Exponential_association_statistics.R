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
dd <- data.frame()

for (sample in 1:20) {
  path <- "Data/"
  sample           <- glue("B{sample}")
  folder           <- Sys.glob(glue("{path}{sample}_*"))
  
  
  processed_folder <- file.path(folder, "processed")
  files            <- list.files(processed_folder, 
                                 pattern = "(only_forward|only_backward).*_trialResults\\.xlsx$", 
                                 full.names = TRUE)
  for (f in files) {
    obj_name <- sub("_trialResults\\.xlsx$", "", basename(f))
    dat      <- read_xlsx(f)
    dat      <- dat[1:10,]
    
    # Append the dat into the overarching dd dataframe
    dd <- rbind(dd, dat)
  }
}  
  