## Script for saving the data from the participants in a single data set for analysis ##

################################################################################

library(tidyverse)
library(glue)
library(glmmTMB)
library(performance)
library(ggeffects)
library(readxl)

samples <- paste0("B", 1:17)
participant_info <- read_excel("../Data/Participant_info.xlsx")

################################################################################
# DATA LOADING
################################################################################

model_dataset <- function(samples, direction) {
  dat_all <- data.frame()
  for (s in samples) {
    path <- glue("../Data/{s}_*/processed/")
    files <- Sys.glob(file.path(path, "*_trialResults.xlsx"))
    for (f in files) {
      dat <- readxl::read_excel(f)
      dat$condition <- sub("_trialResults.xlsx", "", sub(glue(".*EC_{s}_"), "", basename(f)))
      dat$sample <- s
      dat_all <- bind_rows(dat_all, dat)
    }
  }
  dat_all <- dat_all %>%
    filter(Direction == direction) %>%
    mutate(p11 = case_when(
      grepl("only_forward", condition)  ~ 1,
      grepl("only_backward", condition) ~ 1,
      grepl("p11_0.75", condition)      ~ 0.75,
      grepl("p11_0.5", condition)       ~ 0.5,
      grepl("p11_0.25", condition)      ~ 0.25,
      grepl("p11_0$", condition)        ~ 0
    ))
  
  dat_all <- merge(dat_all, participant_info, by = "sample")
  return(dat_all)
}

model_dataset(samples, 1)



