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


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Access to Dispute Resolution Mechanisms (Logit) ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

access2DRM_logit <- function(master, regions, study_countries){
  
  results_list <- lapply(
    study_countries,
    function(country){
      
      formula = paste(
        "access2drm ~",
        paste(independent_vars[[country]], collapse = " + ")
      )
      
      results_tbl <- estimate_logit(
        data = master %>% filter(country_name_ltn == country), 
        f = formula
      )
      
      write_csv(results_tbl, "output/tabs/csv/2_3_2_access_to_DRM_logit.csv")
      
      export_results_kable(
        results_tbl, 
        title = "Access to Dispute Resolution Mechanisms",
        file  = "output/tabs/png/2_3_2_access_to_DRM_logit_{country}.png",
        ctype = "logit",
        country = country
      )
      
      return(results_tbl)
    }
  )
  
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Contacted Mechanisms ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

contacted_mechanisms <- function(master, regions, study_countries){
  
  possible_mechanisms <- c(
    "AJR_court_bin", "AJR_police_bin", "AJR_office_bin", "AJR_relig_bin",
    "AJR_arbitration_bin", "AJR_appeal_bin", "AJR_other_bin" 
  )
  
  lapply(
    study_countries,
    function(country){
      
      df_full <- master %>%
        filter(
          access2drm == 1 & country_name_ltn == country
        ) %>%
        select(
          country_year_id, country_name_ltn, nuts_id,
          all_of(possible_mechanisms)
        ) %>%
        mutate(
          across(
            !c(country_year_id, country_name_ltn, nuts_id),
            \(x) case_when(
              x == 1 ~ TRUE,
              x %in% c(2,98,99) ~ FALSE
            )
          ),
          none = case_when(
            AJR_court_bin == FALSE & AJR_police_bin == FALSE & 
              AJR_office_bin == FALSE & AJR_relig_bin == FALSE & 
              AJR_arbitration_bin == FALSE & AJR_appeal_bin == FALSE & 
              AJR_other_bin == FALSE ~ TRUE,
            TRUE ~ FALSE
          )
        )
      df <- df_full %>% 
        select(-c(country_year_id, country_name_ltn, nuts_id))
      names(df) <- c(
        "Court or Tribunal", "Police", "Gvt. Office",
        "Rel. or Comm. Leader", "Third-party Mediator", 
        "Complaints or Appeal Office", "Other", "None"
      )
      n_rows <- nrow(df)
      
      if (country == "Italy"){
        upset <- ComplexUpset::upset(
          df, names(df), 
          name = "Contacted Institutions", 
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
          glue::glue("output/tabs/png/2_3_3_contacted_institutions_{country}.png"), 
          width = 12, 
          height = 6
        )
      }
      
      weighted_data <- df_full %>% 
        group_by(country_name_ltn, nuts_id) %>%
        filter(none == FALSE) %>% 
        select(-country_year_id) %>% 
        summarise(
          across(
            everything(),
            \(x) sum(x)
          ),
          nuts_size = n(),
          .groups = "keep"
        ) %>% 
        left_join(
          regions %>% select(nuts_id, wgt = regionpoppct),
          by = "nuts_id"
        ) %>%
        mutate(
          across(
            all_of(possible_mechanisms),
            \(x) (x/nuts_size)*wgt
          )
        ) %>% 
        group_by(country_name_ltn) %>% 
        summarise(
          across(
            starts_with("AJR_"),
            \(x) sum(x)*100
          )
        ) %>%
        pivot_longer(
          !country_name_ltn,
          names_to = "category",
          values_to = "value"
        ) %>% 
        mutate(
          category = case_when(
            category == "AJR_court_bin"       ~ "Court or Tribunal",
            category == "AJR_police_bin"      ~ "Police",
            category == "AJR_office_bin"      ~ "Gvt. Office",
            category == "AJR_relig_bin"       ~ "Rel. or Comm. Leader",
            category == "AJR_arbitration_bin" ~ "Third-party Mediator",
            category == "AJR_appeal_bin"      ~ "Complaints or Appeal Office",
            category == "AJR_other_bin"       ~ "Other"
          ),
          country_name_ltn = "national_avg"
        )
      names(weighted_data) <- c("grouping", "category", country)
      
      return(weighted_data)
      
    }
  ) %>%
    reduce(
      left_join,
      by = c("grouping", "category")
    )
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  Access to Alternative Dispute Resolution Mechanisms ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

access_to_alternative_DRM <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "JSE_mediation",
    ctype = "short",
    transform_var = function(x) case_when(
      x %in% c(1,2) ~ 1,
      x %in% c(3,4,98) ~ 0
    )
  )
  write_csv(results_tbl, "output/tabs/csv/2_3_4_access_to_alternative_DRM.csv")
  export_results_kable(
    results_tbl, 
    title = "Access to Alternative Dispute Resolution Mechanisms",
    file  = "output/tabs/png/2_3_4_access_to_alternative_DRM.png",
    ctype = "short"
  )
  
  return(results_tbl)
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5.  Mechanisms preventing access to Dispute Resolution Mechanisms ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

access2DRM_barriers <- function(master, regions, study_countries){
  
  results_tbl <- master %>%
    select(
      country_name_ltn, nuts_id, 
      target = all_of("AJR_noresol_reason"),
      filtering = all_of("access2drm")
    ) %>% 
    mutate(
      target = case_when(
        target == 1 ~ "Issue was not important",
        target == 2 ~ "Respondent was confidence to solve the issue alone",
        target == 3 ~ "Time or bureaucracy",
        target == 4 ~ "No evidence or strong case",
        target == 5 ~ "Fear of retaliation",
        target == 6 ~ "Did not know what to do or where to go",
        target == 7 ~ "Access problems",
        target == 8 ~ "Lack of trust in authorities",
        target == 9 ~ "Fear of consequences",
        target == 10 ~ "It was up to the other party",
        target == 11 ~ "Other",
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
  
  write_csv(results_tbl, "output/tabs/csv/2_3_5_access2DRM_barriers.csv")
  
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
          caption = "Mechanisms Preventing Access to a Dispute Resolution Mechanism",
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
            glue::glue("output/tabs/png/2_3_5_access2DRM_barriers_{country}.png")
          )
      )
      
    }
  )
  
  return(
    results_tbl %>% 
      pivot_wider(
        id_cols = target,
        names_from = country_name_ltn,
        values_from = proportion_national
      ) %>% 
      mutate(
        grouping = "National avg."
      ) %>% 
      rename(
        category = target
      )
  )
  
}

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 6.  Access to Dispute Resolution Mechanisms (Perceptions) ----
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
  write_csv(results_tbl, "output/tabs/csv/2_3_6_access2DRM_gpp.csv")
  export_results_kable(
    results_tbl, 
    title = "Perceptions of Access to Dispute Resolution Mechanisms",
    file  = "output/tabs/png/2_3_6_access2DRM_gpp.png",
    ctype = "short"
  )
  
  return(results_tbl)
}


## 7. No of Mechanisms contacted ----
ncontacts <- function(){
  possible_mechanisms <- c(
    "AJR_court_bin", "AJR_police_bin", "AJR_office_bin", "AJR_relig_bin",
    "AJR_arbitration_bin", "AJR_appeal_bin", "AJR_other_bin" 
  )
  data_subset <- master %>% 
    filter(
      access2drm == 1
    ) %>% 
    mutate(
      across(
        all_of(possible_mechanisms),
        \(x) case_when(
          x == 1 ~ 1,
          x %in% c(2,98,99) ~ 0
        )
      )
    ) %>% 
    select(
      country_year_id, country_name_ltn, nuts_id,
      all_of(possible_mechanisms)
    ) %>% 
    pivot_longer(
      !c(country_year_id, country_name_ltn, nuts_id),
      names_to = "category",
      values_to = "value"
    ) %>% 
    mutate(
      contacted_court = if_else(
        category == "AJR_court_bin" & value == 1, 1, 0,
        missing = NA_real_
      )
    ) %>% 
    group_by(country_year_id) %>% 
    summarise(
      country_name_ltn = first(country_name_ltn),
      nuts_id = first(nuts_id),
      contacted_mechanisms = sum(value),
      contacted_court = sum(contacted_court)
    ) %>% 
    filter(
      !is.na(contacted_mechanisms)
    )
  
  only_court <- data_subset %>% 
    mutate(
      contacted_only_court = if_else(
        contacted_mechanisms == 1 & contacted_court == 1, 1, 0
      )
    ) %>% 
    group_by(country_name_ltn, nuts_id) %>% 
    summarise(
      nuts_sample = n(),
      contacted_only_court = sum(contacted_only_court)
    ) %>% 
    mutate(
      proportion = contacted_only_court/nuts_sample
    ) %>% 
    left_join(
      regions %>% select(nuts_id, wgt = regionpoppct),
      by = "nuts_id"
    ) %>%
    mutate(
      weighted_proportion = proportion*wgt
    ) %>% 
    group_by(country_name_ltn) %>% 
    summarise(
      national_avg = sum(weighted_proportion)
    )
  
  three_or_more <- data_subset %>% 
    mutate(
      three_or_more = if_else(
        contacted_mechanisms >= 3, 1, 0
      )
    ) %>% 
    group_by(country_name_ltn, nuts_id) %>% 
    summarise(
      nuts_sample = n(),
      three_or_more = sum(three_or_more)
    ) %>% 
    mutate(
      proportion = three_or_more/nuts_sample
    ) %>% 
    left_join(
      regions %>% select(nuts_id, wgt = regionpoppct),
      by = "nuts_id"
    ) %>%
    mutate(
      weighted_proportion = proportion*wgt
    ) %>% 
    group_by(country_name_ltn) %>% 
    summarise(
      national_avg = sum(weighted_proportion)
    )
}