## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Worker for data loading & wrangling routines
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     July 22, 2025
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
source("src/data/special-vars.R")

# Study variables (+ ALL LNS module)
study_vars <- c(
  "country_year_id", "country_name_ltn", "nuts_id",  # identifiers
  
  # Sociodemographics
  "gender", "age", "financial", "edu", "residence",  
  "age_group", "edu_level", "marital_status", 
  "nationality", "emp_status",
  
  # GPP variables
  "JSE_access2info", "JSE_access2assis", "JSE_mediation",
  "TRT_judges", "TRT_prosecutors", "TRT_pda",
  
  # A2J variables
  "prevalence1", "prevalence2",
  "vulnerability1", "vulnerability2", "vulnerability3",
  "access2info", "access2rep", "access2drm",
  "rp_time", "rp_cost", "rp_fair", "rp_outcome", "rp_quick", "rp_costdiff",
  "non_trivial_problem", "selected_problem_category", 
  "problem_status", "resolution_favor", "rp_satisfaction",
  "nproblems", "cooccurence_group",
  
  # Justice Gap variables
  "inside_justice_gap_nodk", "inside_justice_gap_keepdk"
)

# Loading data
if(!interactive()){
  verbose_message("--- Loading EU-GPP data...")
}
eugpp_subset <- load_data(
  path2EU = path2EU,
  source = "gpp"
) %>% 
  filter(
    country_name_ltn %in% study_countries
  )

# Processing data
if(!interactive()){
  verbose_message("--- Processing data...")
}
master <- eugpp_subset %>%
  left_join(
    add_special_demographics(eugpp_subset),
    by = "country_year_id"
  ) %>%
  left_join(
    add_a2j_vars(eugpp_subset),
    by = "country_year_id"
  ) %>%
  select(
    all_of(study_vars),
    starts_with(c("AJP_", "AJD_", "AJR_", "AJE_"))
  )

# Saving data
if(!interactive()){
  verbose_message("--- Saving master data...")
}
write_csv(master, "data/master.csv")

