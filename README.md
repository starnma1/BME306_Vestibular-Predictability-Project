# BME306 Project Overview

This repository contains all scripts, datasets, and outputs for the BME306 course report on vestibular adaptation to galvanic vestibular stimulation (GVS) delivered according to a Markov chain framework.

---

## Repository Structure

### Created_Datasets/
Contains the processed datasets used for modelling.

- `model_dataset_forward.csv` and `model_dataset_backward.csv` are the main per-direction datasets used in the forward vs. backward analysis.
- `learning_curve_data.csv` contains session-level data for the learning effect analysis.
- `learning_curve_data_only_first10.csv` is the restricted version used in the final learning effect model, keeping only the first 10 pulses per session.
- `diff_summary.csv` contains summary-level difference data used in exploratory work.

### Data/
Raw data from the experiment.

### Plots/
All figures generated during the analysis.

### Scripts/

**Top-level scripts** handle data preparation and exploration:
- `Identifying_Markov_chains.R` identifies and assigns Markov chain parameters to trials.
- `Individual_level_DataExploration.R` explores subject-level variation in postural responses.
- `Composite_exploration.R` covers broader exploratory summaries across the dataset.

**Primary_Hypothesis_Scripts/** contains everything related to the main analysis of p11 and postural deviation:
- `Model_Dataset.R` prepares the dataset for the primary model.
- `non_linearity_statistics.R` fits and evaluates the polynomial Gamma mixed model.
- `primary_hypothesis_and_conclusions.md` documents the hypothesis and conclusions for this analysis.

**Secondary_Hypothesis_scripts/** is split into two subfolders:
- `Learning_effect_Scripts/` contains the analysis of how postural deviation changes across repeated sessions.
- `Forward_vs_backward_hypothesis_scripts/` contains the direction-stratified learning curve analysis comparing forward and backward perturbations.
