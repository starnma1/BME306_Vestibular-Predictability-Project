# Script to make a dataset for learning effect analysis #

###############################################################################

library(tidyverse)
library(glue)
library(glmmTMB)
library(performance)
library(ggeffects)
library(readxl)

###############################################################################

# Read all the data and create new files labbeled 1-6 based on 
# when they were done, aka the timestamp.

for (participant in 1:20) {
  path   <- "Data/"
  sample <- paste0("B", participant)
  folder <- Sys.glob(glue("{path}{sample}_*"))
  
  learning_dir <- file.path(folder, "Learning_Data")
  if (!dir.exists(learning_dir)) dir.create(learning_dir)
  
  files <- list.files(folder, pattern = "_trialResults\\.xlsx$", full.names = TRUE)
  
  # Extract timestamps from filenames and sort chronologically
  timestamps <- files |>
    basename() |>
    str_extract("\\d{4}_\\d{2}_\\d{2}_\\d{6}") |>
    as.POSIXct(format = "%Y_%m_%d_%H%M%S")
  
  ordered_files <- files[order(timestamps)]
  
  # Copy and rename 1 to n
  for (i in seq_along(ordered_files)) {
    dest <- file.path(learning_dir, glue("{i}.xlsx"))
    file.copy(ordered_files[i], dest, overwrite = TRUE)
  }
  
  message(glue("B{participant}: {length(ordered_files)} files processed"))
}

###############################################################################

# Compile all those files into one dataset, using only the median from each trial

all_data <- list()

for (participant in 1:20) {
  path   <- "Data/"
  sample <- paste0("B", participant)
  folder <- Sys.glob(glue("{path}{sample}_*/Learning_Data"))
  files <- list.files(folder, pattern = "^[^~].*\\.xlsx$", full.names = TRUE)
  for (f in files){
    frame_name <- sub("\\.xlsx$", "", basename(f))
    prelim_read <- read_excel(f)
    print(sample)
    print(colnames(prelim_read))
    dat <- data.frame(
      sample = sample,
      trial = frame_name,
      median_body_x = median(prelim_read$StdBodyX) # Assuming the median is in the 5th row
      )
      all_data <- append(all_data, list(dat))
    }
  }


