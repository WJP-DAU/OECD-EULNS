## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Routines for the "Access to Information and Advice" sub-module
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     July 24, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Access to Adequate Information and Advice ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

access2information <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "access2info"
  )
  write_csv(results_tbl, "output/tabs/csv/2_1_1_access_to_information.csv")
  export_results_kable(
    results_tbl, 
    title = "Access to Adequate Information and Advice",
    file  = "output/tabs/png/2_1_1_access_to_information.png"
  )
  
  return(results_tbl)
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Access to Adequate Information and Advice (Logit) ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

access2information_logit <- function(master, regions, study_countries){
  
  results_list <- lapply(
    study_countries,
    function(country){
      
      formula = paste(
        "access2info ~",
        paste(independent_vars[[country]], collapse = " + ")
      )
      
      data4logit = master %>% filter(country_name_ltn == country)
      results_tbl <- estimate_logit(
        data = data4logit, 
        f = formula
      )

      write_csv(results_tbl, "output/tabs/csv/2_1_2_access_to_information_logit.csv")
      
      export_results_kable(
        results_tbl, 
        title = "Access to Adequate Information and Advice",
        file  = "output/tabs/png/2_1_2_access_to_information_logit_{country}.png",
        ctype = "logit",
        country = country
      )
      
      return(results_tbl)
    }
  )

}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Access to Adequate Information and Advice (Perceptions) ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

access2information_perceptions <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "JSE_access2info",
    ctype = "short",
    transform_var = function(x) case_when(
      x %in% c(1,2) ~ 1,
      x %in% c(3,4,98) ~ 0
    )
  )
  write_csv(results_tbl, "output/tabs/csv/2_1_3_access_to_information_gpp.csv")
  export_results_kable(
    results_tbl, 
    title = "Perceptions of Access to Legal Information and Advice",
    file  = "output/tabs/png/2_1_3_access_to_information_gpp.png",
    ctype = "short"
  )
  
  return(results_tbl)
}
