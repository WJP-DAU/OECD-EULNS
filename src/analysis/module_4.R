## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Routines for the "Public Trust" module
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     July 30, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Knowledge of legal rights and responsibilities ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

knowledge_rights <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "AJE_legalrights",
    ctype = "short",
    transform_var = function(x) case_when(
      x %in% c(1,2) ~ 1,
      x %in% c(3,4,98) ~ 0
    )
  )
  write_csv(results_tbl, "output/tabs/csv/4_1_knowledge_rights.csv")
  export_results_kable(
    results_tbl, 
    title = "Knowledge of legal rights and responsibilities",
    file  = "output/tabs/png/4_1_knowledge_rights.png",
    ctype = "short"
  )
  
  return(results_tbl)
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Perceived causes of the problem ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

perceived_causes <- function(master, regions, study_countries){
  
  results_tbl <- master %>%
    select(
      country_name_ltn, nuts_id, 
      starts_with("AJE_description")
    ) %>% 
    pivot_longer(
      !c(country_name_ltn, nuts_id),
      values_to = "mentioned",
      names_to = "category"
    ) %>% 
    mutate(
      category = case_when(
        category == "AJE_description_1" ~ "Bad luck / Part of Life",
        category == "AJE_description_2" ~ "Bureaucratic",
        category == "AJE_description_3" ~ "Family or private matter",
        category == "AJE_description_4" ~ "Legal",
        category == "AJE_description_5" ~ "Political",
        category == "AJE_description_6" ~ "Social or community matter",
        category == "AJE_description_7" ~ "Economic",
        category == "AJE_description_8" ~ "None of these",
        category == "AJE_description_98" ~ "Don't know",
        category == "AJE_description_99" ~ "No answer"
      )
    ) %>% 
    mutate(
      mentioned = case_when(
        mentioned == 1 ~ 1,
        mentioned == 2 ~ 0,
      ),
      counter = if_else(
        is.na(mentioned), 0, 1
      )
    ) %>% 
    group_by(country_name_ltn, nuts_id, category) %>%
    summarise(
      total_sample = sum(counter, na.rm = TRUE),
      mentions = sum(mentioned, na.rm = TRUE),
      .groups = "keep"
    ) %>%
    left_join(
      regions %>% select(nuts_id, weight = regionpoppct),
      by = "nuts_id"
    ) %>%
    mutate(
      proportion_weighted = (mentions/total_sample)*weight
    ) %>%
    group_by(country_name_ltn, category) %>%
    summarise(
      proportion_national = sum(proportion_weighted, na.rm = TRUE)*100,
      .groups = "keep"
    )
  
  write_csv(results_tbl, "output/tabs/csv/4_2_perceived_causes.csv")
  
  lapply(
    study_countries,
    function(country){
      
      data4table <- results_tbl %>%
        filter(country_name_ltn==country) %>%
        arrange(desc(proportion_national))
      
      suppressMessages(
        kableExtra::kbl(
          data4table, 
          digits = 1, 
          caption = "Problem Description",
          col.names = c("Country", "Description", "Percentage (%)")
        ) %>%
          kableExtra::kable_classic(
            # full_width = F, 
            html_font = "Cambria"
          ) %>% 
          kableExtra::kable_styling(
            font_size = 18,
            latex_options = "striped"
          ) %>%
          kableExtra::save_kable(
            glue::glue("output/tabs/png/4_2_perceived_causes_{country}.png")
          )
      )
      
    }
  )
  
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Fully Satisfied Expert Support Needs ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

expert_support <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "AJE_advice",
    ctype = "short",
    transform_var = function(x) case_when(
      x %in% c(1,2) ~ 1,
      x %in% c(3,4,98) ~ 0
    )
  )
  write_csv(results_tbl, "output/tabs/csv/4_3_expert_support.csv")
  export_results_kable(
    results_tbl, 
    title = "Fully Satisfied Expert Support Needs",
    file  = "output/tabs/png/4_3_expert_support.png",
    ctype = "short"
  )
  
  return(results_tbl)
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  Legal Empowerment ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

legal_empowerment <- function(master, regions){
  
  vulnerabilities <- c("vulnerability1", "vulnerability2", "vulnerability3")
  names(vulnerabilities) <- c("identity", "housing", "contract")
  
  results_list <- imap(
    vulnerabilities,
    function(v, type){
      
      results_tbl <- get_results_table(
        master, 
        regions, 
        target = v,
        ctype = "short"
      )
      write_csv(
        results_tbl, 
        glue::glue("output/tabs/csv/4_4_legal_empowerment_{type}.csv")
      )
      export_results_kable(
        results_tbl, 
        title = glue::glue("Legal Empowerment: {type}"),
        file  = glue::glue("output/tabs/png/4_4_legal_empowerment_{type}.png"),
        ctype = "short"
      )
      
      return(results_tbl)
      
    }
  )
  
}
