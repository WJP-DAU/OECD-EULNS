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
jg_data <- load_data(
  path2EU = path2EU,
  source = "jg"
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
cooccurence_nproblems <- cooccurence_nproblems(master, regions)
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


if(!interactive()){
  verbose_message("--- Performing data analysis for Module 2.3 ...")
}
source("src/analysis/module_2_3.R")
access2DRM <- access2DRM(master, regions)
access2DRM_logit <- access2DRM_logit(master, regions, study_countries)
contacted_mechanisms <- contacted_mechanisms(master, regions, study_countries)
access_to_alternative_DRM <- access_to_alternative_DRM(master, regions)
access2DRM_barriers <- access2DRM_barriers(master, regions, study_countries)
  

if(!interactive()){
  verbose_message("--- Performing data analysis for Module 2.4 ...")
}
source("src/analysis/module_2_4.R")
timeliness_rp <- timeliness_rp(master, regions)
slowness_rp <- slowness_rp(master, regions)
fair_rp <- fair_rp(master, regions)
cost_rp <- cost_rp(master, regions)
expensive_rp <- expensive_rp(master, regions)


if(!interactive()){
  verbose_message("--- Performing data analysis for Module 2.5 ...")
}
source("src/analysis/module_2_5.R")
outcome_rp <- outcome_rp(master, regions)
satisfaction_rp <- satisfaction_rp(master, regions)


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Module 3 ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  verbose_message("--- Performing data analysis for Module 3 ...")
}
source("src/analysis/module_3.R")
justice_gap_keepdk <- justice_gap_keepdk(master, regions)
justice_gap_nodk   <- justice_gap_nodk(master, regions)
unmet_legal_needs  <- unmet_legal_needs(jg_data, study_countries)
justice_gap_logit  <- justice_gap_logit(master, regions, study_countries)
hardships <- hardships(master, regions)


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Module 4 ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  verbose_message("--- Performing data analysis for Module 4 ...")
}
source("src/analysis/module_4.R")
knowledge_rights <- knowledge_rights(master, regions)
perceived_causes <- perceived_causes(master, regions, study_countries)
expert_support <- expert_support(master, regions)
legal_empowerment <- legal_empowerment(master, regions)
  
  
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Module 5 ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  verbose_message("--- Performing data analysis for Module 5 ...")
}
source("src/analysis/module_5.R")
trust <- trust(master, regions)
  