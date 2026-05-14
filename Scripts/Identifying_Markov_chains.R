### Script to determine the Markov chain used for each dataset ###

library(readxl)
library(glue)
library(tidyverse)

sample <- "B1"

# Set folder path
folder <- glue("./{sample}__2026_05_08/")

# Set candidate Markov chains
candidate_probs <- c(0, 0.25,0.5, 0.75, 1)

# Find all matching files
files <- list.files(folder, pattern = "_trialResults\\.xlsx$", full.names = TRUE)

classify_chain <- function(direction_col) {
  
  x    <- direction_col
  from <- x[-length(x)]
  to   <- x[-1]
  
  n11 <- sum(from == 1 & to == 1)
  n10 <- sum(from == 1 & to == 0)
  
  # True edge case: no 1s in sequence at all, p11 undefined
  if ((n11 + n10) == 0) return("only_backward")
  
  # Now safe to compute p11_hat, including the case where it is 0
  p11_hat <- n11 / (n11 + n10)
  nearest <- candidate_probs[which.min(abs(candidate_probs - p11_hat))]
  
  switch(as.character(nearest),
         "0"    = "p11_0",
         "0.25" = "p11_0.25",
         "0.5"  = "p11_0.50",
         "0.75" = "p11_0.75",
         "1"    = "only_forward"
  )
}

# Create processed folder if it doesn't exist
processed_folder <- file.path(folder, "processed")
dir.create(processed_folder, showWarnings = FALSE)

for (f in files) {
  obj_name    <- sub("_trialResults\\.xlsx$", "", basename(f))
  dat         <- read_xlsx(f)
  chain_label <- classify_chain(dat$Direction)
  
  new_filename <- paste0(obj_name, "_", chain_label, "_trialResults.xlsx")
  file.copy(from = f, to = file.path(processed_folder, new_filename))
  
  cat(sprintf("  %s  -->  %s\n", obj_name, new_filename))
}
