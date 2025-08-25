## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Routines for the "Outcome of the Resolution Process " sub-module
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     July 28, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Outcomes Obtained by Users of Dispute Resolution Processes ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

outcome_rp <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "rp_outcome"
  )
  write_csv(results_tbl, "output/tabs/csv/2_5_1_outcome.csv")
  export_results_kable(
    results_tbl, 
    title = "Outcomes Obtained by Users of Dispute Resolution Processes",
    file  = "output/tabs/png/2_5_1_outcome.png"
  )
  
  return(results_tbl)
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Satisfaction ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

satisfaction_rp <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "rp_satisfaction",
    ctype = "custom_1"
  )
  write_csv(results_tbl, "output/tabs/csv/2_5_2_satisfaction.csv")
  export_results_kable(
    results_tbl, 
    title = "Satisfaction with the Resolution Outcome ",
    file  = "output/tabs/png/2_5_2_satisfaction.png",
    ctype = "custom_1"
  )
  
  return(results_tbl)
}

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Status of the Resolution Process: Overall ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

status_rp <- function(master, regions){
  data2plot <- master %>% 
    mutate(
      across(
        starts_with("AJR_"),
        \(x) case_when(
          x == 1 ~ "Ongoing",
          x == 2 ~ "Too early to say",
          x == 3 ~ "Done with, but problem persists",
          x == 4 ~ "Done with, problem fully resolved",
          x >= 98 ~ "DK/NA"
        )
      ),
      status = case_when(
        !is.na(AJR_state_noresol) ~ AJR_state_noresol,
        !is.na(AJR_state_resol) ~ AJR_state_resol
      )
    ) %>% group_by(
      country_name_ltn, nuts_id
    ) %>% 
    select(
      country_name_ltn, nuts_id, status, non_trivial_problem
    ) %>% 
    filter(
      !is.na(status) & non_trivial_problem == 1
    ) %>% 
    mutate(
      nuts_sample = n()
    ) %>% 
    group_by(
      country_name_ltn, nuts_id, status
    ) %>% 
    summarise(
      nuts_sample = first(nuts_sample),
      count = n(),
      .groups = "keep"
    ) %>% 
    left_join(
      regions %>% select(nuts_id, weight = regionpoppct),
      by = "nuts_id"
    ) %>%
    mutate(
      proportion_weighted = (count/nuts_sample)*weight
    ) %>% 
    group_by(country_name_ltn, status) %>%
    summarise(
      proportion_national = sum(proportion_weighted, na.rm = TRUE)*100,
      .groups = "keep"
    ) %>% 
    pivot_wider(
      id_cols = status,
      names_from = country_name_ltn,
      values_from = proportion_national
    ) %>% 
    mutate(
      grouping = "National Avg."
    ) %>% 
    rename(
      category = status
    )
}