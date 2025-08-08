## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Routines for the "Assessment of the Resolution Process " sub-module
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     July 25, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Timeliness of the Resolution Process ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

timeliness_rp <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "rp_time"
  )
  write_csv(results_tbl, "output/tabs/csv/2_4_1_timeliness.csv")
  export_results_kable(
    results_tbl, 
    title = "Timeliness of the Resolution Process",
    file  = "output/tabs/png/2_4_1_timeliness.png"
  )
  
  return(results_tbl)
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Perceived Quickness of the Resolution Process ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

slowness_rp <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "rp_quick"
  )
  write_csv(results_tbl, "output/tabs/csv/2_4_2_quickness.csv")
  export_results_kable(
    results_tbl, 
    title = "Perceived Quickness of the Resolution Process",
    file  = "output/tabs/png/2_4_2_quickness.png"
  )
  
  return(results_tbl)
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Fairness of the Resolution Process ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fair_rp <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "rp_fair"
  )
  write_csv(results_tbl, "output/tabs/csv/2_4_3_fairness.csv")
  export_results_kable(
    results_tbl, 
    title = "Perceived Fairness of the Resolution Process",
    file  = "output/tabs/png/2_4_3_fairness.png"
  )
  
  return(results_tbl)
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  Costliness of the Resolution Process ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cost_rp <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "rp_cost"
  )
  write_csv(results_tbl, "output/tabs/csv/2_4_4_costliness.csv")
  export_results_kable(
    results_tbl, 
    title = "Costliness of the Resolution Process",
    file  = "output/tabs/png/2_4_4_costliness.png"
  )
  
  return(results_tbl)
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5.  Ease of Covering the Costs of the Resolution Process ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

expensive_rp <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "rp_expensive"
  )
  write_csv(results_tbl, "output/tabs/csv/2_4_5_expensive.csv")
  export_results_kable(
    results_tbl, 
    title = "Perception of Expensive Costs associated to the Resolution Process",
    file  = "output/tabs/png/2_4_5_expensive.png"
  )
  
  return(results_tbl)
}