## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Worker for data analysis routines
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     July 23, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

suppressWarnings(
  suppressMessages(
    library("tidyverse", quietly = T)
  )
)

if (interactive()){
  source("src/utils/config.R")
}
source("src/utils/utils.R")
source("src/data/data-loading.R")

# Loading data
if(!interactive()){
  verbose_message("--- Loading data...")
}
master <- load_data(
  path2EU = path2EU,
  source = "subset"
)
regions <- load_data(
  path2EU = path2EU,
  source = "regions"
)


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Module 1 ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  verbose_message("--- Performing data analysis for Module 1 ...")
}
source("src/analysis/module_1.R")

freq_probs_by_severity <- freq_probs_by_severity(master, regions)
prevalence_nontrivial_problems <- prevalence_nontrivial_problems(master, regions)
prevalence_nontrivial_problems_by_category <- prevalence_nontrivial_problems_by_category(master, regions)
cooccurence_network <- cooccurence_network(master, regions)


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Module 2 ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  verbose_message("--- Performing data analysis for Module 2.1 ...")
}
source("src/analysis/module_2_1.R")

access2information <- access2information(master, regions)
access2information_logit <- access2information_logit(master, regions, study_countries)
access2information_perceptions <- access2information_perceptions(master, regions)


if(!interactive()){
  verbose_message("--- Performing data analysis for Module 2.2 ...")
}
source("src/analysis/module_2_2.R")

access2representation <- access2representation(master, regions)
access2representation_logit <- access2representation_logit(master, regions, study_countries)
access2representation_perceptions <- access2representation_perceptions(master, regions)
contacted_advisers <- contacted_advisers(master, regions, study_countries)
access2representation_barriers <- access2representation_barriers(master, regions, study_countries)

  