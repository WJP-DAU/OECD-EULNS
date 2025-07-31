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