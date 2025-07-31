## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Routines for the "Consequences of Unmet Legal Needs and Unresolved Justice" module
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     July 30, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Justice Gap ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

justice_gap_keepdk <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "inside_justice_gap_keepdk",
    ctype = "short"
  )
  write_csv(results_tbl, "output/tabs/csv/3_1_justice_gap_keepdk.csv")
  export_results_kable(
    results_tbl, 
    title = "Justice Gap (with DK)",
    file  = "output/tabs/png/3_1_justice_gap_keepdk.png",
    ctype = "short"
  )
  
  return(results_tbl)
}

justice_gap_nodk <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "inside_justice_gap_nodk",
    ctype = "short"
  )
  write_csv(results_tbl, "output/tabs/csv/3_1_justice_gap_nodk.csv")
  export_results_kable(
    results_tbl, 
    title = "Justice Gap (no DK)",
    file  = "output/tabs/png/3_1_justice_gap_nodk.png",
    ctype = "short"
  )
  
  return(results_tbl)
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Co-occurence of Unmet Legal Needs ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

unmet_legal_needs <- function(jg_data, study_countries){
  
  possible_needs <- c(
    "jg_inverse_access2info", "jg_inverse_access2rep",
    "jg_inverse_time", "jg_inverse_cost", "jg_inverse_fair", 
    "jg_inverse_outcome"
  )
  
  lapply(
    study_countries,
    function(country){
      
      if (country == "Italy"){ # Malta doesn't have the sample for this chart
      
        df <- jg_data %>%
          filter(
            !is.na(a2j_score) & str_detect(country_year_id, country) &
              inside_gap == 1
          ) %>% 
          select(
            all_of(possible_needs)
          ) %>%
          mutate(
            across(
              everything(),
              \(x) case_when(
                x == 1 ~ TRUE,
                x == 0 ~ FALSE,
                is.na(x) ~ FALSE
              )
            )
          )
        names(df) <- c(
          "Information", "Advice/Representation", "Timeliness",
          "Costliness", "Fairness", "Outcome"
        )
        n_rows <- nrow(df)
        
        upset <- ComplexUpset::upset(
          df, names(df), 
          name = "Unmet Legal Needs", 
          width_ratio = 0.1, 
          min_size    = 11,
          base_annotations = list(
            "Intersection size" = ComplexUpset::intersection_size(
              
              mapping = aes(
                fill  = "bar_color",
              ),
              
              text_colors=c(
                on_background = "#003E1F", 
                on_bar = "white"
              ),
              
              text_mapping = aes(
                # label = paste0(
                #   # !!ComplexUpset::upset_text_percentage(),
                #   !!ComplexUpset::get_size_mode("exclusive_intersection"),
                #   "\n(",
                #   format(
                #     round(
                #       ((!!ComplexUpset::get_size_mode("exclusive_intersection"))/nrow(df))*100, 1
                #     ),
                #     nsmall = 1
                #   ),
                #   "%)"
                # )
                label = paste0(
                  format(
                    round(
                      ((!!ComplexUpset::get_size_mode("exclusive_intersection"))/nrow(df))*100, 0
                    ),
                    nsmall = 0
                  ),
                  "%"
                )
              )
              
            ) + scale_fill_manual(
              values = c("bar_color" = "#003E1F"), 
              guide  = "none"
            )
          ),
          set_sizes = ComplexUpset::upset_set_size(
            geom = geom_bar(
              stat  = "count",
              fill  = "#003E1F"
            ) 
          ) + 
            geom_text(
              aes(
                label = paste0(
                  format(
                    round(
                      (after_stat(count)/n_rows)*100, 0
                    ),
                    nsmall = 0
                  ), "%"
                )
              ), 
              hjust = 1.1, 
              stat  = "count"
            ) +
            ylab("No. of respondents") + 
            expand_limits(y = 1000) +
            theme (
              axis.text.x  = element_blank(),
              axis.title.x = element_blank()
            )
        )
        
        ggsave(
          glue::glue("output/tabs/png/3_2_cooccurence_legal_needs_{country}.png"), 
          width = 12, 
          height = 6
        )
        
        return(df)
      }
      
    }
  )
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Justice Gap (Logit) ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

justice_gap_logit <- function(master, regions, study_countries){
  
  results_list <- lapply(
    study_countries,
    function(country){
      
      formula = paste(
        "inside_justice_gap_keepdk ~",
        paste(independent_vars[[country]], collapse = " + ")
      )
      
      results_tbl <- estimate_logit(
        data = master %>% filter(country_name_ltn == country), 
        f = formula
      )
      
      write_csv(results_tbl, "output/tabs/csv/3_3_justice_gap_logit.csv")
      
      export_results_kable(
        results_tbl, 
        title = "Justice Gap",
        file  = "output/tabs/png/3_3_justice_gap_logit_{country}.png",
        ctype = "logit",
        country = country
      )
      
      return(results_tbl)
    }
  )
  
}


