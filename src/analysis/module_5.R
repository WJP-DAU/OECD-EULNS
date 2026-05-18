## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Routines for the "5.	Legal Capability and Empowerment" module
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     July 30, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Public Trust in Justice ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

trust <- function(master, regions){
  
  master <- master %>% 
    mutate(
      non_trivial_problem_broader = case_when(
        non_trivial_problem == 1 ~ "Yes",
        non_trivial_problem == 0 ~ "No",
        is.na(non_trivial_problem) ~ "No"
      )
    )
  
  justice_actors <- list(
    "judges"      = "TRT_judges",
    "prosecutors" = "TRT_prosecutors",
    "pda"         = "TRT_pda"
  )
  
  results_list <- imap(
    justice_actors,
    function(var_name, actor){
      
      results_tbl <- get_results_table(
        master, 
        regions, 
        target = var_name,
        ctype = "trust",
        transform_var = function(x) case_when(
          x %in% c(1,2) ~ 1,
          x %in% c(3,4,98) ~ 0
        )
      )
      write_csv(
        results_tbl, 
        glue::glue("output/tabs/csv/5_1_trust_{actor}.csv")
      )
      export_results_kable(
        results_tbl, 
        title = glue::glue("Public Trust: {actor}"),
        file  = glue::glue("output/tabs/png/5_1_trust_{actor}.png"),
        ctype = "trust"
      )
      
      return(results_tbl)
      
    }
  )
  
  return(results_list)
}