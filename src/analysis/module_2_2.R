## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Routines for the "Access to Assistance and Representation" sub-module
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     July 24, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Access to Appropriate Assistance and Representation ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

access2representation <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "access2rep"
  )
  write_csv(results_tbl, "output/tabs/csv/2_2_1_access_to_representation.csv")
  export_results_kable(
    results_tbl, 
    title = "Access to Appropriate Assistance and Representation",
    file  = "output/tabs/png/2_2_1_access_to_representation.png"
  )

  return(results_tbl)
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Access to Appropriate Assistance and Representation (Logit) ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

access2representation_logit <- function(master, regions, study_countries){
  
  results_list <- lapply(
    study_countries,
    function(country){
      
      formula = paste(
        "access2rep ~",
        paste(independent_vars[[country]], collapse = " + ")
      )
      
      results_tbl <- estimate_logit(
        data = master %>% filter(country_name_ltn == country), 
        f = formula
      )
      
      write_csv(results_tbl, "output/tabs/csv/2_2_2_access_to_representation_logit.csv")
      
      export_results_kable(
        results_tbl, 
        title = "Access to Appropriate Assistance and Representation",
        file  = "output/tabs/png/2_2_2_access_to_representation_logit_{country}.png",
        ctype = "logit",
        country = country
      )
      
      return(results_tbl)
    }
  )
  
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Access to affordable legal assistance and representation (Perceptions) ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

access2representation_perceptions <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "JSE_access2assis",
    ctype = "short",
    transform_var = function(x) case_when(
      x %in% c(1,2) ~ 1,
      x %in% c(3,4,98) ~ 0
    )
  )
  write_csv(results_tbl, "output/tabs/csv/2_2_3_access_to_representation_gpp.csv")
  export_results_kable(
    results_tbl, 
    title = "Perceptions of Access to Affordable Legal Assistance and Representation",
    file  = "output/tabs/png/2_2_3_access_to_representation_gpp.png",
    ctype = "short"
  )
  
  return(results_tbl)
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  Contacted Advisers ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

contacted_advisers <- function(master, regions, study_countries){
  
  possible_advisers <- c(
    "AJD_adviser_1", "AJD_adviser_2", "AJD_adviser_3" , "AJD_adviser_4" ,
    "AJD_adviser_5" , "AJD_adviser_6", "AJD_adviser_7", "AJD_adviser_8" 
  )
  
  lapply(
    study_countries,
    function(country){
      
      df <- master %>%
        filter(
          access2rep == 1 & country_name_ltn == country
        ) %>%
        select(
          all_of(possible_advisers)
        ) %>%
        mutate(
          # People who didn't needed an advisor
          none = case_when(
            is.na(AJD_adviser_1) ~ TRUE
          ),
          across(
            everything(),
            \(x) case_when(
              x == 1 ~ TRUE,
              x == 2 ~ FALSE,
              is.na(x) ~ FALSE # People who didn't needed an advisor
            )
          )
        )
      names(df) <- c(
        "Relative or Friend", "Lawyer or Prof. Advisor", "Gvt. Legal Aid",
        "Gvt. Body or Police", "Health/Welfare Prof.", "Trade Union or Employer", 
        "Rel. or Comm. Leader", "CSO", "None"
      )
      n_rows <- nrow(df)
      
      upset <- ComplexUpset::upset(
        df, names(df), 
        name = "Contacted Advisers", 
        width_ratio = 0.1, 
        min_size    = 10,
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
            fill  = "#003E1F",
            width = 0.75
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
        glue::glue("output/tabs/png/2_2_4_contacted_advisers_{country}.png"), 
        width = 12, 
        height = 6
      )
      
      return(df)
    }
  )
  
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5.  Mechanisms preventing access to assistance and representation ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

access2representation_barriers <- function(master, regions, study_countries){
  
  results_tbl <- master %>%
    select(
      country_name_ltn, nuts_id, 
      target = all_of("AJD_noadvice_reason"),
      filtering = all_of("access2rep")
    ) %>% 
    mutate(
      target = case_when(
        target == 1 ~ "Issue was not important",
        target == 2 ~ "Other side was right",
        target == 3 ~ "Not needed advice",
        target == 4 ~ "Financial cost",
        target == 5 ~ "Previous experiences",
        target == 6 ~ "Did not know who to contact",
        target == 7 ~ "Did not know they could get advice",
        target == 8 ~ "Fear",
        target == 9 ~ "Advisers were too far away",
        target == 10 ~ "Other",
        target %in% c(98,99) ~ "DK/NA"
      )
    ) %>% 
    filter(
      filtering == 0 & !is.na(target)
    ) %>% 
    group_by(country_name_ltn, nuts_id) %>%
    mutate(
      total_sample = n()
    ) %>% 
    group_by(country_name_ltn, nuts_id, target) %>%
    summarise(
      reason_count = n(),
      total_sample = first(total_sample),
      .groups = "keep"
    ) %>%
    left_join(
      regions %>% select(nuts_id, weight = regionpoppct),
      by = "nuts_id"
    ) %>%
    mutate(
      proportion_weighted = (reason_count/total_sample)*weight
    ) %>%
    group_by(country_name_ltn, target) %>%
    summarise(
      proportion_national = sum(proportion_weighted, na.rm = TRUE)*100,
      .groups = "keep"
    )
  
  write_csv(results_tbl, "output/tabs/csv/2_2_5_access2representation_barriers.csv")
  
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
          caption = "Mechanisms Preventing Access to Assistance and Representation",
          col.names = c("Country", "Reason", "Percentage (%)")
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
            glue::glue("output/tabs/png/2_2_5_access2representation_barriers_{country}.png")
          )
      )
      
    }
  )
  
}
