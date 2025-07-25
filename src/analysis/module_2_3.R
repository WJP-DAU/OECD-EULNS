## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Routines for the "Access to Dispute Resolution Mechanisms" sub-module
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     July 25, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Access to Dispute Resolution Mechanisms ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

access2DRM <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "access2drm"
  )
  write_csv(results_tbl, "output/tabs/csv/2_3_1_access_to_DRM.csv")
  export_results_kable(
    results_tbl, 
    title = "Access to Dispute Resolution Mechanisms",
    file  = "output/tabs/png/2_3_1_access_to_DRM.png"
  )
  
  return(results_tbl)
}