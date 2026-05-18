## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Routines for the final visualization indicators summary
## Author(s):         Santiago Pardo (spardo@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     May 17, 2026
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Helper routines ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

visual_tables <- data.frame(
  source_table = c(
    "1_2_prevalence_nontrivial_problems.csv",
    "1_3_prevalence_nontrivial_problems_by_category.csv",
    "2_1_1_access_to_information.csv",
    "2_1_3_access_to_information_gpp.csv",
    "2_2_1_access_to_representation.csv",
    "2_2_3_access_to_representation_gpp.csv",
    "2_2_5_access2representation_barriers.csv",
    "2_3_1_access_to_DRM.csv",
    "2_3_5_access2DRM_barriers.csv",
    "2_4_1_timeliness.csv",
    "2_4_2_quickness.csv",
    "2_4_3_fairness.csv",
    "2_4_4_costliness.csv",
    "2_4_5_expensive.csv",
    "2_5_1_outcome.csv",
    "2_5_2_satisfaction.csv",
    "3_1_justice_gap_keepdk.csv",
    "3_4_hardships.csv",
    "4_1_knowledge_rights.csv",
    "4_2_perceived_causes.csv",
    "4_3_expert_support.csv",
    "4_4_legal_empowerment_contract.csv",
    "4_4_legal_empowerment_housing.csv",
    "4_4_legal_empowerment_identity.csv",
    "5_1_trust_judges.csv",
    "5_1_trust_prosecutors.csv",
    "5_1_trust_pda.csv"
  ),
  module = c(
    "1", "1", "2.1", "2.1", "2.2", "2.2", "2.2", "2.3", "2.3",
    "2.4", "2.4", "2.4", "2.4", "2.4", "2.5", "2.5", "3", "3",
    "4", "4", "4", "4", "4", "4", "5", "5", "5"
  ),
  visualization = c(
    "Prevalence of Non-Trivial Problems",
    "Prevalence of Non-Trivial Problems by Category",
    "Access to Adequate Information and Advice",
    "Perceptions of Access to Legal Information and Advice",
    "Access to Adequate Assistance and Representation",
    "Perceptions of Access to Legal Assistance and Representation",
    "Mechanisms Preventing Access to Assistance and Representation",
    "Access to Dispute Resolution Mechanisms",
    "Mechanisms Preventing Access to Dispute Resolution Mechanisms",
    "Dispute Resolution Processes that Finalized in Less than One Year",
    "Perceived Quickness of the Dispute Resolution Process",
    "Perceived Fairness of the Dispute Resolution Process",
    "Costliness of Dispute Resolution Processes",
    "Dispute Resolution Processes Regarded as Expensive",
    "Outcomes Obtained by Users of Dispute Resolution Processes",
    "Satisfaction with the Resolution Outcome",
    "Justice Gap (with DK)",
    "Hardships Experienced by People in their Justice Pathway",
    "Knowledge of Rights",
    "Perceived Causes",
    "Expert Support",
    "Legal Empowerment: Written Work Agreement",
    "Legal Empowerment: Proof of Housing or Land Tenure",
    "Legal Empowerment: Official Proof of Identity",
    "Public Trust in Judges",
    "Public Trust in Prosecutors",
    "Public Trust in Public Defense Attorneys"
  ),
  stringsAsFactors = FALSE
)


relabel_visual_data <- function(df){
  
  results_tbl <- df %>%
    mutate(
      grouping = case_when(
        grouping == "National Avg." ~ "National",
        TRUE ~ grouping
      ),
      category = case_when(
        source_table == "3_4_hardships.csv" & category == "hardships_health" ~ "Health issues",
        source_table == "3_4_hardships.csv" & category == "hardships_emotional" ~ "Relationship breakdown",
        source_table == "3_4_hardships.csv" & category == "hardships_income" ~ "Economic issues",
        source_table == "3_4_hardships.csv" & category == "hardships_drugs" ~ "Alcohol/Drug problems",
        source_table == "4_4_legal_empowerment_contract.csv" & category == "National" ~ "Written work agreement",
        source_table == "4_4_legal_empowerment_housing.csv" & category == "National" ~ "Proof of housing or land tenure",
        source_table == "4_4_legal_empowerment_identity.csv" & category == "National" ~ "Official proof of identity",
        TRUE ~ category
      )
    )
  
  return(results_tbl)
  
}


get_binary_n <- function(data, target, transform_var = NULL){
  
  if (is.null(transform_var)){
    values <- data[[target]]
  } else {
    values <- transform_var(data[[target]])
  }
  
  return(
    sum(!is.na(values), na.rm = TRUE)
  )
  
}


get_visual_n <- function(master, source_table, country, grouping = NULL, category = NULL){
  
  data <- master %>%
    filter(
      country_name_ltn == country
    )
  
  if (source_table == "1_2_prevalence_nontrivial_problems.csv") return(sum(!is.na(data$prevalence2), na.rm = TRUE))
  if (source_table == "1_3_prevalence_nontrivial_problems_by_category.csv") return(nrow(data))
  if (source_table == "2_1_1_access_to_information.csv") return(sum(!is.na(data$access2info), na.rm = TRUE))
  
  if (source_table == "2_1_3_access_to_information_gpp.csv"){
    return(get_binary_n(data, "JSE_access2info", function(x) case_when(
      x %in% c(1,2) ~ 1,
      x %in% c(3,4,98) ~ 0
    )))
  }
  
  if (source_table == "2_2_1_access_to_representation.csv") return(sum(!is.na(data$access2rep), na.rm = TRUE))
  
  if (source_table == "2_2_3_access_to_representation_gpp.csv"){
    return(get_binary_n(data, "JSE_access2assis", function(x) case_when(
      x %in% c(1,2) ~ 1,
      x %in% c(3,4,98) ~ 0
    )))
  }
  
  if (source_table == "2_2_5_access2representation_barriers.csv"){
    return(data %>%
      mutate(
        target = case_when(
          AJD_noadvice_reason == 1 ~ "Issue was not important",
          AJD_noadvice_reason == 2 ~ "Other side was right",
          AJD_noadvice_reason == 3 ~ "Not needed advice",
          AJD_noadvice_reason == 4 ~ "Financial cost",
          AJD_noadvice_reason == 5 ~ "Previous experiences",
          AJD_noadvice_reason == 6 ~ "Did not know who to contact",
          AJD_noadvice_reason == 7 ~ "Did not know they could get advice",
          AJD_noadvice_reason == 8 ~ "Fear",
          AJD_noadvice_reason == 9 ~ "Advisers were too far away",
          AJD_noadvice_reason == 10 ~ "Other",
          AJD_noadvice_reason %in% c(98,99) ~ "Don't Know / No Answer"
        )
      ) %>%
      filter(access2rep == 0 & !is.na(target)) %>%
      nrow())
  }
  
  if (source_table == "2_3_1_access_to_DRM.csv") return(sum(!is.na(data$access2drm), na.rm = TRUE))
  
  if (source_table == "2_3_5_access2DRM_barriers.csv"){
    return(data %>%
      mutate(
        target = case_when(
          AJR_noresol_reason == 1 ~ "Access problems",
          AJR_noresol_reason == 2 ~ "Did not know what to do or where to go",
          AJR_noresol_reason == 3 ~ "Time or bureaucracy",
          AJR_noresol_reason == 4 ~ "Fear of retaliation",
          AJR_noresol_reason == 5 ~ "Lack of trust in authorities",
          AJR_noresol_reason %in% c(98,99) ~ "Don't know / No answer"
        )
      ) %>%
      filter(access2drm == 0 & !is.na(target)) %>%
      nrow())
  }
  
  if (source_table == "2_4_1_timeliness.csv") return(sum(!is.na(data$rp_time), na.rm = TRUE))
  if (source_table == "2_4_2_quickness.csv") return(sum(!is.na(data$rp_quick), na.rm = TRUE))
  if (source_table == "2_4_3_fairness.csv") return(sum(!is.na(data$rp_fair), na.rm = TRUE))
  if (source_table == "2_4_4_costliness.csv") return(sum(!is.na(data$rp_cost), na.rm = TRUE))
  if (source_table == "2_4_5_expensive.csv") return(sum(!is.na(data$rp_expensive), na.rm = TRUE))
  if (source_table == "2_5_1_outcome.csv") return(sum(!is.na(data$rp_outcome), na.rm = TRUE))
  if (source_table == "2_5_2_satisfaction.csv") return(sum(!is.na(data$rp_satisfaction), na.rm = TRUE))
  if (source_table == "3_1_justice_gap_keepdk.csv") return(sum(!is.na(data$inside_justice_gap_keepdk), na.rm = TRUE))
  
  if (source_table == "3_4_hardships.csv"){
    hardship_map <- c(
      "Health issues" = "hardships_health",
      "Relationship breakdown" = "hardships_emotional",
      "Economic issues" = "hardships_income",
      "Alcohol/Drug problems" = "hardships_drugs"
    )
    return(sum(!is.na(data[[hardship_map[[category]]]]), na.rm = TRUE))
  }
  
  if (source_table == "4_1_knowledge_rights.csv"){
    return(get_binary_n(data, "AJE_legalrights", function(x) case_when(
      x %in% c(1,2) ~ 1,
      x %in% c(3,4,98) ~ 0
    )))
  }
  
  if (source_table == "4_2_perceived_causes.csv"){
    cause_map <- c(
      "Bad luck / Part of life" = "AJE_description_1",
      "Bureaucratic" = "AJE_description_2",
      "Family or private matter" = "AJE_description_3",
      "Legal" = "AJE_description_4",
      "Political" = "AJE_description_5",
      "Social or community matter" = "AJE_description_6",
      "Economic" = "AJE_description_7",
      "None of these" = "AJE_description_8",
      "Don't know" = "AJE_description_98",
      "No answer" = "AJE_description_99"
    )
    return(sum(!is.na(data[[cause_map[[category]]]]), na.rm = TRUE))
  }
  
  if (source_table == "4_3_expert_support.csv"){
    return(get_binary_n(data, "AJE_advice", function(x) case_when(
      x %in% c(1,2) ~ 1,
      x %in% c(3,4,98) ~ 0
    )))
  }
  
  if (source_table == "4_4_legal_empowerment_contract.csv") return(sum(!is.na(data$vulnerability3), na.rm = TRUE))
  if (source_table == "4_4_legal_empowerment_housing.csv") return(sum(!is.na(data$vulnerability2), na.rm = TRUE))
  if (source_table == "4_4_legal_empowerment_identity.csv") return(sum(!is.na(data$vulnerability1), na.rm = TRUE))
  
  if (source_table == "5_1_trust_judges.csv"){
    return(get_binary_n(data, "TRT_judges", function(x) case_when(
      x %in% c(1,2) ~ 1,
      x %in% c(3,4,98) ~ 0
    )))
  }
  
  if (source_table == "5_1_trust_prosecutors.csv"){
    return(get_binary_n(data, "TRT_prosecutors", function(x) case_when(
      x %in% c(1,2) ~ 1,
      x %in% c(3,4,98) ~ 0
    )))
  }
  
  if (source_table == "5_1_trust_pda.csv"){
    return(get_binary_n(data, "TRT_pda", function(x) case_when(
      x %in% c(1,2) ~ 1,
      x %in% c(3,4,98) ~ 0
    )))
  }
  
  if (source_table == "status_rp (derived)"){
    status_data <- data %>%
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
      ) %>%
      filter(!is.na(status) & non_trivial_problem == 1)
    return(nrow(status_data))
  }
  
  if (source_table == "sankey_advice_representation (derived)"){
    return(data %>%
      filter(!is.na(sankey_advice_stage_1) & !is.na(sankey_advice_stage_2)) %>%
      nrow())
  }
  
  if (source_table == "sankey_drm (derived)"){
    if (grouping == "Access to a DRM (SDG 16.3.3)"){
      return(data %>% filter(!is.na(sankey_drm_stage_1) & !is.na(sankey_drm_stage_2)) %>% nrow())
    }
    return(data %>% filter(!is.na(sankey_drm_stage_1)) %>% nrow())
  }
  
  return(NA_real_)
  
}


add_visual_n <- function(df, master){
  
  results_tbl <- pmap_dfr(
    df,
    function(module, visualization, source_table, country, grouping, category, pct){
      tibble(
        module = module,
        visualization = visualization,
        source_table = source_table,
        country = country,
        grouping = grouping,
        category = category,
        pct = pct,
        n_obs = get_visual_n(master, source_table, country, grouping, category)
      )
    }
  )
  
  return(results_tbl)
  
}


pivot_visual_table <- function(source_table, module, visualization){
  
  countries <- unname(study_countries)
  
  data <- read_csv(
    file.path("output", "tabs", "csv", source_table),
    show_col_types = FALSE
  )
  
  if (all(c("grouping", "category", countries) %in% names(data))){
    
    results_tbl <- data %>%
      pivot_longer(
        all_of(countries),
        names_to = "country",
        values_to = "pct"
      ) %>%
      transmute(
        module = module,
        visualization = visualization,
        source_table = source_table,
        country,
        grouping,
        category,
        pct = as.numeric(pct)
      )
    
  } else if (all(c("category", countries) %in% names(data))){
    
    results_tbl <- data %>%
      pivot_longer(
        all_of(countries),
        names_to = "country",
        values_to = "pct"
      ) %>%
      transmute(
        module = module,
        visualization = visualization,
        source_table = source_table,
        country,
        grouping = "Category",
        category,
        pct = as.numeric(pct)
      )
    
  } else if (all(c("country_name_ltn", "category", "proportion_national") %in% names(data))){
    
    results_tbl <- data %>%
      transmute(
        module = module,
        visualization = visualization,
        source_table = source_table,
        country = country_name_ltn,
        grouping = "Category",
        category,
        pct = as.numeric(proportion_national)
      )
    
  } else if (all(c("country_name_ltn", "target", "proportion_national") %in% names(data))){
    
    results_tbl <- data %>%
      transmute(
        module = module,
        visualization = visualization,
        source_table = source_table,
        country = country_name_ltn,
        grouping = "Category",
        category = target,
        pct = as.numeric(proportion_national)
      )
    
  } else {
    
    stop(glue::glue("Unsupported table format in '{source_table}'"))
    
  }
  
  results_tbl <- relabel_visual_data(results_tbl)
  
  if ("grouping" %in% names(results_tbl)){
    results_tbl <- results_tbl %>%
      filter(
        grouping %in% c("National", "Category")
      )
  }
  
  return(results_tbl)
  
}


status_rp_visual <- function(master, regions){
  
  results_tbl <- master %>% 
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
    ) %>% 
    select(
      country_name_ltn, nuts_id, status, non_trivial_problem
    ) %>% 
    filter(
      !is.na(status) & non_trivial_problem == 1
    ) %>% 
    group_by(
      country_name_ltn, nuts_id
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
      pct = sum(proportion_weighted, na.rm = TRUE)*100,
      .groups = "drop"
    ) %>%
    transmute(
      module = "2.5",
      visualization = "Status of the Resolution Process",
      source_table = "status_rp (derived)",
      country = country_name_ltn,
      grouping = "National",
      category = status,
      pct
    )
  
  return(results_tbl)
  
}


advice_sankey_visual <- function(master, regions){
  
  stage_labels <- c(
    "sankey_advice_stage_1" = "Contacted an Advisor",
    "sankey_advice_stage_2" = "Type of Advisor / Reason for No Contact",
    "sankey_advice_stage_3" = "Access to Adequate Assistance and Representation"
  )
  
  results_tbl <- lapply(
    study_countries,
    function(country){
      
      sankey_base_data <- master %>%
        filter(
          country_name_ltn == country
        ) %>% 
        select(
          country_year_id, country_name_ltn, nuts_id,
          starts_with("sankey_advice_stage_")
        ) %>%
        filter(
          !is.na(sankey_advice_stage_1) & !is.na(sankey_advice_stage_2)
        ) %>% 
        group_by(nuts_id) %>%
        mutate(
          nuts_sample = n()
        )
      
      sankey_data_stage_1 <- sankey_base_data %>% 
        group_by(country_name_ltn, nuts_id, nuts_sample, sankey_advice_stage_1) %>% 
        summarise(
          count = n(),
          .groups = "keep"
        ) %>% 
        left_join(
          regions %>%
            select(nuts_id, wgt = regionpoppct),
          by = "nuts_id"
        ) %>% 
        mutate(
          value = (count/nuts_sample)*wgt
        ) %>% 
        group_by(country_name_ltn, sankey_advice_stage_1) %>% 
        summarise(
          pct = sum(value)*100,
          .groups = "drop"
        ) %>%
        transmute(
          module = "2.2",
          visualization = "Sankey: Advice and Representation",
          source_table = "sankey_advice_representation (derived)",
          country = country_name_ltn,
          grouping = stage_labels[["sankey_advice_stage_1"]],
          category = sankey_advice_stage_1,
          pct
        )
      
      sankey_data_stage_2 <- sankey_base_data %>% 
        group_by(country_name_ltn, nuts_id, nuts_sample, sankey_advice_stage_2) %>% 
        summarise(
          count = n(),
          .groups = "keep"
        ) %>% 
        left_join(
          regions %>%
            select(nuts_id, wgt = regionpoppct),
          by = "nuts_id"
        ) %>% 
        mutate(
          value = (count/nuts_sample)*wgt
        ) %>% 
        group_by(country_name_ltn, sankey_advice_stage_2) %>% 
        summarise(
          pct = sum(value)*100,
          .groups = "drop"
        ) %>%
        transmute(
          module = "2.2",
          visualization = "Sankey: Advice and Representation",
          source_table = "sankey_advice_representation (derived)",
          country = country_name_ltn,
          grouping = stage_labels[["sankey_advice_stage_2"]],
          category = sankey_advice_stage_2,
          pct
        )
      
      sankey_data_stage_3 <- sankey_base_data %>% 
        group_by(country_name_ltn, nuts_id, nuts_sample, sankey_advice_stage_2) %>% 
        summarise(
          count = n(),
          .groups = "keep"
        ) %>% 
        left_join(
          regions %>%
            select(nuts_id, wgt = regionpoppct),
          by = "nuts_id"
        ) %>% 
        mutate(
          value = (count/nuts_sample)*wgt
        ) %>% 
        group_by(country_name_ltn, sankey_advice_stage_2) %>% 
        summarise(
          national_avg = sum(value),
          .groups = "drop"
        ) %>%
        mutate(
          sankey_advice_stage_3 = case_when(
            sankey_advice_stage_2 %in% c(
              "Appropriate Advisor",
              "Did not need an advisor"
            ) ~ "Had access",
            sankey_advice_stage_2 %in% c(
              "Non-Appropriate Advisor",
              "Needed but did not have access to an advisor",
              "Unknown Reason"
            ) ~ "Had no access"
          )
        ) %>% 
        group_by(country_name_ltn, sankey_advice_stage_3) %>% 
        summarise(
          pct = sum(national_avg)*100,
          .groups = "drop"
        ) %>%
        transmute(
          module = "2.2",
          visualization = "Sankey: Advice and Representation",
          source_table = "sankey_advice_representation (derived)",
          country = country_name_ltn,
          grouping = stage_labels[["sankey_advice_stage_3"]],
          category = sankey_advice_stage_3,
          pct
        )
      
      bind_rows(
        sankey_data_stage_1,
        sankey_data_stage_2,
        sankey_data_stage_3
      )
      
    }
  ) %>%
    reduce(
      bind_rows
    )
  
  return(results_tbl)
  
}


drm_sankey_visual <- function(master, regions){
  
  stage_labels <- c(
    "sankey_drm_stage_0" = "Contacted a DRM",
    "sankey_drm_stage_1" = "Reason for No Contact",
    "sankey_drm_stage_2" = "Access to a DRM (SDG 16.3.3)"
  )
  
  results_tbl <- lapply(
    study_countries,
    function(country){
      
      sankey_base_data <- master %>%
        filter(
          country_name_ltn == country
        ) %>% 
        select(
          country_year_id, country_name_ltn, nuts_id,
          starts_with("sankey_drm_stage_")
        ) %>%
        filter(
          !is.na(sankey_drm_stage_1)
        )
      
      sankey_data_stage_1 <- sankey_base_data %>% 
        filter(
          sankey_drm_stage_1 != "Turned to a DRM"
        ) %>% 
        group_by(nuts_id) %>% 
        mutate(
          nuts_sample = n()
        ) %>% 
        group_by(country_name_ltn, nuts_id, nuts_sample, sankey_drm_stage_1) %>% 
        summarise(
          count = n(),
          .groups = "keep"
        ) %>% 
        left_join(
          regions %>%
            select(nuts_id, wgt = regionpoppct),
          by = "nuts_id"
        ) %>% 
        mutate(
          value = (count/nuts_sample)*wgt
        ) %>% 
        group_by(country_name_ltn, sankey_drm_stage_1) %>% 
        summarise(
          pct = sum(value)*100,
          .groups = "drop"
        ) %>%
        transmute(
          module = "2.3",
          visualization = "Sankey: Dispute Resolution Mechanisms",
          source_table = "sankey_drm (derived)",
          country = country_name_ltn,
          grouping = stage_labels[["sankey_drm_stage_1"]],
          category = sankey_drm_stage_1,
          pct
        )
      
      sankey_data_stage_2 <- sankey_base_data %>% 
        filter(
          !is.na(sankey_drm_stage_2)
        ) %>%
        group_by(nuts_id) %>% 
        mutate(
          nuts_sample = n()
        ) %>%
        group_by(country_name_ltn, nuts_id, nuts_sample, sankey_drm_stage_2) %>% 
        summarise(
          count = n(),
          .groups = "keep"
        ) %>% 
        left_join(
          regions %>%
            select(nuts_id, wgt = regionpoppct),
          by = "nuts_id"
        ) %>% 
        mutate(
          value = (count/nuts_sample)*wgt
        ) %>% 
        group_by(country_name_ltn, sankey_drm_stage_2) %>% 
        summarise(
          pct = sum(value)*100,
          .groups = "drop"
        ) %>%
        transmute(
          module = "2.3",
          visualization = "Sankey: Dispute Resolution Mechanisms",
          source_table = "sankey_drm (derived)",
          country = country_name_ltn,
          grouping = stage_labels[["sankey_drm_stage_2"]],
          category = sankey_drm_stage_2,
          pct
        )
      
      sankey_data_stage_0 <- sankey_base_data %>% 
        group_by(nuts_id) %>% 
        mutate(
          nuts_sample = n()
        ) %>% 
        group_by(country_name_ltn, nuts_id, nuts_sample, sankey_drm_stage_1) %>% 
        summarise(
          count = n(),
          .groups = "keep"
        ) %>% 
        left_join(
          regions %>%
            select(nuts_id, wgt = regionpoppct),
          by = "nuts_id"
        ) %>% 
        mutate(
          value = (count/nuts_sample)*wgt
        ) %>% 
        group_by(country_name_ltn, sankey_drm_stage_1) %>% 
        summarise(
          national_avg = sum(value),
          .groups = "drop"
        ) %>% 
        mutate(
          sankey_drm_stage_0 = if_else(
            sankey_drm_stage_1 == "Turned to a DRM",
            "Contacted a DRM",
            "Did not contact a DRM"
          )
        ) %>%
        group_by(country_name_ltn, sankey_drm_stage_0) %>% 
        summarise(
          pct = sum(national_avg)*100,
          .groups = "drop"
        ) %>%
        transmute(
          module = "2.3",
          visualization = "Sankey: Dispute Resolution Mechanisms",
          source_table = "sankey_drm (derived)",
          country = country_name_ltn,
          grouping = stage_labels[["sankey_drm_stage_0"]],
          category = sankey_drm_stage_0,
          pct
        )
      
      bind_rows(
        sankey_data_stage_0,
        sankey_data_stage_1,
        sankey_data_stage_2
      )
      
    }
  ) %>%
    reduce(
      bind_rows
    )
  
  return(results_tbl)
  
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Main Indicators ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

main_indicators <- function(master, regions){
  
  tabs_data <- lapply(
    seq_len(nrow(visual_tables)),
    function(i){
      pivot_visual_table(
        source_table = visual_tables$source_table[i],
        module = visual_tables$module[i],
        visualization = visual_tables$visualization[i]
      )
    }
  ) %>% 
    reduce(
      bind_rows
    ) %>%
    add_visual_n(master)
  
  derived_data <- bind_rows(
    status_rp_visual(master, regions),
    advice_sankey_visual(master, regions),
    drm_sankey_visual(master, regions)
  ) %>%
    add_visual_n(master)
  
  results_tbl <- bind_rows(
    tabs_data,
    derived_data
  ) %>%
    arrange(
      module, visualization, country, grouping, category
    )
  
  writexl::write_xlsx(
    list(
      "visual_indicators" = results_tbl
    ),
    "output/tabs/main_indicators.xlsx"
  )
  
  return(results_tbl)
  
}
