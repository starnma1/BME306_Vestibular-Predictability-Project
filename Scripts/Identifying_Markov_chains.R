
################################################################################

define_first_sample <- 1
define_final_sample <- 20

################################################################################

library(readxl)
library(glue)

candidate_probs <- c(0, 0.25, 0.5, 0.75, 1)

classify_chain <- function(direction_col) {
  x    <- direction_col
  from <- x[-length(x)]
  to   <- x[-1]
  
  n11 <- sum(from == 1 & to == 1)
  n10 <- sum(from == 1 & to == 0)
  
  if ((n11 + n10) == 0) return("only_backward")
  
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

for (t in define_first_sample:define_final_sample) {
  sample <- glue("B{t}")
  folder <- Sys.glob(glue("Data/{sample}_*"))
  
  # Skip if folder doesn't exist
  if (!dir.exists(folder)) {
    cat(sprintf("  %s -- folder not found, skipping\n", sample))
    next
  }
  
  files <- list.files(folder, pattern = "_trialResults\\.xlsx$", full.names = TRUE)
  
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
}
