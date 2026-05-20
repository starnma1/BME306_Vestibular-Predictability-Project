# Script to make a dataset for learning effect analysis #
# 
# To create the dataset just select all and run the script.
# It will read all the data, create new folders for each 
# participant with the learning data, and then compile all 
# the median body x values into one dataset for analysis.

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

compile_learning_data <- function(n_rows = NULL, exclude_participants = 11) {
  
  all_data <- list()
  
  for (participant in 1:20) {
    if (participant %in% exclude_participants) next
    path   <- "Data/"
    sample <- paste0("B", participant)
    folder <- Sys.glob(glue("{path}{sample}_*/Learning_Data"))
    files <- list.files(folder, pattern = "^[^~].*\\.xlsx$", full.names = TRUE)
    for (f in files){
      frame_name <- sub("\\.xlsx$", "", basename(f))
      prelim_read <- read_excel(f)
      if (!is.null(n_rows)) prelim_read <- prelim_read[1:min(n_rows, nrow(prelim_read)), ]
      print(sample)
      print(colnames(prelim_read))
      dat <- prelim_read %>%
        group_by(Direction) %>%
        summarise(median_body_x = median(StdBodyX))
      
      final_dat <- data.frame(
        sample = sample,
        trial = frame_name,
        median_body_x = dat$median_body_x,
        direction = dat$Direction
      )
      all_data <- c(all_data, list(final_dat))
    }
  }
  
  bind_rows(all_data)
}

# Data set complete
learning_curve_data <- compile_learning_data(n_rows = 10)


#write.csv(learning_curve_data, "Data_for_stats/learning_curve_data.csv", row.names = FALSE)
#write.csv(learning_curve_data, "Data_for_stats/learning_curve_data_only_first10.csv", row.names = FALSE)


