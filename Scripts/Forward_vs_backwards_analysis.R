## Script for anaylzing and confirming the only forward and only backward movements ##

################################################################################

define_first_sample <- 1
define_final_sample <- 17

################################################################################

library(readxl)
library(glue)
library(ggplot2)
library(gridExtra)


path <- "Data/"

################################################################################
# This first block produces a pdf containing plots for everyparticipant and their
# only forwards and only backwards body adaptations. We expect a nice convergence
# at a stable value, as the body begins predicting the perturbation and thus 
# reduces its movement variability.
################################################################################
plots <- list()

# Load all the files based on the pattern only forward and only backwards, we will start with these
for (t in define_first_sample:define_final_sample) {
  sample           <- glue("B{t}")
  folder           <- Sys.glob(glue("{path}{sample}_*"))
  
  
  processed_folder <- file.path(folder, "processed")
  files            <- list.files(processed_folder, 
                                 pattern = "(only_forward|only_backward).*_trialResults\\.xlsx$", 
                                 full.names = TRUE)
  
  
  for (f in files) {
    obj_name <- sub("_trialResults\\.xlsx$", "", basename(f))
    dat      <- read_xlsx(f)
    
    p <- ggplot(dat, aes(x = StartTime, y = StdBodyX)) +
      geom_point(color = "steelblue") +
      geom_smooth(method = "glm", color = "red", method.args = list(family = gaussian(link = "log"))) +
      labs(title = obj_name, x = "Start Time", y = "StdBodyX") +
      theme_minimal() +
      theme(plot.title = element_text(size = 6),
            axis.text  = element_text(size = 5),
            axis.title = element_text(size = 5))
    
    plots[[obj_name]] <- p
  }
}

# Arrange in 2 column grid
n_cols <- 2
n_rows <- ceiling(length(plots) / n_cols)

ggsave(
  filename = "plots/StdBodyX_grid.pdf",
  plot     = marrangeGrob(grobs = plots, nrow = n_rows, ncol = n_cols),
  width    = 10,
  height   = n_rows * 3
)

cat(sprintf("  saved %d plots to plots/StdBodyX_grid.pdf\n", length(plots)))

################################################################################

# Now comparing forwards and backward movement means
forward_means  <- list()
backward_means <- list()

for (t in define_first_sample:define_final_sample) {
  sample <- glue("B{t}")
  folder <- Sys.glob(glue("{path}{sample}_*"))
  
  if (length(folder) == 0) next
  
  processed_folder <- file.path(folder, "processed")
  
  # Forward files
  fwd_files <- list.files(processed_folder,
                          pattern = "only_forward.*_trialResults\\.xlsx$",
                          full.names = TRUE)
  
  # Backward files
  bwd_files <- list.files(processed_folder,
                          pattern = "only_backward.*_trialResults\\.xlsx$",
                          full.names = TRUE)
  
  if (length(fwd_files) > 0) {
    fwd_dat               <- read_xlsx(fwd_files[1])
    forward_means[[sample]] <- mean(fwd_dat$StdBodyX, na.rm = TRUE)
  }
  
  if (length(bwd_files) > 0) {
    bwd_dat                <- read_xlsx(bwd_files[1])
    backward_means[[sample]] <- mean(bwd_dat$StdBodyX, na.rm = TRUE)
  }
}

# Build summary dataframe
all_samples <- union(names(forward_means), names(backward_means))

diff <- data.frame(
  participant   = all_samples,
  mean_forward  = sapply(all_samples, function(s) forward_means[[s]]  %||% NA),
  mean_backward = sapply(all_samples, function(s) backward_means[[s]] %||% NA)
)

diff$difference <- diff$mean_forward - diff$mean_backward

### Output ----

write.csv(diff, "Data_for_stats/diff_summary.csv", row.names = FALSE)

