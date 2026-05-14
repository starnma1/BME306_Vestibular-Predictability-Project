### Function for graphical exploration, just change the sample, and run ALL ###

################################################################################
library(tidyverse)
library(glue)
library(ggplot2)
library(ggeffects)
library(ggpubr)

################################################################################


sample <- "B13"
#setwd("Expansion/BME306/")

################################################################################

path <- glue("Data/{sample}_*/processed/")
files <- Sys.glob(file.path(path, "*_trialResults.xlsx"))

# Start of function, make sure its all run above
graphical_exploration <- function(sample) {
  path <- glue("Data/{sample}_*/processed/")
  files <- Sys.glob(file.path(path, "*_trialResults.xlsx"))
  
  dat_all <- data.frame()
  for (f in files) {
    dat <- readxl::read_excel(f)
    dat$condition <- sub("_trialResults.xlsx", "", sub(glue(".*EC_{sample}_"), "", basename(f)))
    dat_all <- bind_rows(dat_all, dat)
  }
  
  boxplot <- ggplot(dat_all) +
    geom_boxplot(aes(x = factor(Direction), y = StdBodyX, fill = condition)) +
    scale_y_continuous(limits = c(0, 5))
  
  mutate_p11 <- function(df) {
    df %>% mutate(p11 = case_when(
      grepl("only_forward", condition)  ~ 1,
      grepl("only_backward", condition) ~ 0,
      grepl("p11_0.75", condition)      ~ 0.75,
      grepl("p11_0.5", condition)       ~ 0.5,
      grepl("p11_0.25", condition)      ~ 0.25,
      grepl("p11_0$", condition)        ~ 0
    ))
  }
  
  dat_forward <- dat_all %>%
    filter(Direction == 1) %>%
    mutate_p11() %>%
    group_by(p11) %>%
    summarize(mean_StdBodyX = mean(StdBodyX, na.rm = TRUE), .groups = "drop")
  
  dat_backward <- dat_all %>%
    filter(Direction == 0) %>%
    mutate_p11() %>%
    group_by(p11) %>%
    summarize(mean_StdBodyX = mean(StdBodyX, na.rm = TRUE), .groups = "drop")
  
  # If you want to change plots change this function
  curve_plot <- function(dat, direction_label) {
    ggplot(dat, aes(x = p11, y = mean_StdBodyX)) +
      geom_point() +
      geom_smooth(method = "loess") +
      theme_minimal() +
      scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
      labs(x = "Predictability", y = "Mean StdBodyX",
           title = glue("{direction_label} pulses: Mean StdBodyX by Predictibilaty ({sample})"))
  }
  
  forward_curve  <- curve_plot(dat_forward,  "Forward")
  backward_curve <- curve_plot(dat_backward, "Backward")
  
  graphs <- ggarrange(backward_curve, forward_curve, nrow = 1)
  plot_path <- glue("plots/Individual_Plots/{sample}_individual_level_exploration.png")
  #ggsave(plot_path, graphs, width = 12, height = 6)
  return(graphs)
}

################################################################################

graphical_exploration(sample)

