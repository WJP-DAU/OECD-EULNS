## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            EU average routines for main indicators
## Author(s):         Santiago Pardo (spardo@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     June 2026
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

build_eu_master <- function(path2EU, regions){
  study_vars <- c(
    "country_year_id", "country_name_ltn", "nuts_id",
    "gender", "age", "financial", "edu", "residence",
    "age_group", "edu_level", "marital_status",
    "nationality", "emp_status",
    "JSE_access2info", "JSE_access2assis", "JSE_mediation",
    "JSE_affordcosts", "JSE_fairoutcomes",
    "TRT_judges", "TRT_prosecutors", "TRT_pda",
    "prevalence1", "prevalence2",
    "vulnerability1", "vulnerability2", "vulnerability3",
    "access2info", "access2rep", "access2drm",
    "rp_time", "rp_cost", "rp_fair", "rp_outcome", "rp_quick", "rp_expensive",
    "non_trivial_problem", "selected_problem_category",
    "non_trivial_problem_broader",
    "problem_status", "resolution_favor", "rp_satisfaction",
    "nproblems", "cooccurence_group",
    "hardship", "hardships_health", "hardships_emotional", "hardships_income", "hardships_drugs",
    "inside_justice_gap_nodk", "inside_justice_gap_keepdk",
    "sankey_advice_stage_1", "sankey_advice_stage_2", "sankey_drm_stage_1",
    "sankey_drm_stage_2"
  )

  eu_countries <- sort(unique(regions$country))

  eugpp <- load_data(
    path2EU = path2EU,
    source = "gpp"
  ) %>%
    filter(country_name_ltn %in% eu_countries) %>%
    apply_a2j_quality_check()

  eugpp %>%
    left_join(
      add_special_demographics(eugpp),
      by = "country_year_id"
    ) %>%
    left_join(
      add_a2j_vars(eugpp),
      by = "country_year_id"
    ) %>%
    mutate(
      non_trivial_problem_broader = case_when(
        non_trivial_problem == 1 ~ "Yes",
        non_trivial_problem == 0 ~ "No",
        is.na(non_trivial_problem) ~ "No"
      )
    ) %>%
    select(
      any_of(study_vars),
      starts_with(c("AJP_", "AJD_", "AJR_", "AJE_"))
    )
}

country_cols <- function(results_tbl){
  setdiff(names(results_tbl), c("grouping", "category"))
}

as_country_indicators <- function(results_tbl, module, visualization, source_table){
  results_tbl %>%
    pivot_longer(
      all_of(country_cols(results_tbl)),
      names_to = "country",
      values_to = "pct"
    ) %>%
    mutate(
      module = module,
      visualization = visualization,
      source_table = source_table,
      pct = as.numeric(pct)
    ) %>%
    relabel_visual_data() %>%
    filter(grouping %in% c("National", "Category")) %>%
    select(module, visualization, source_table, country, grouping, category, pct)
}

binary_transform <- function(x){
  case_when(
    x %in% c(1, 2) ~ 1,
    x %in% c(3, 4, 98) ~ 0
  )
}

prevalence_category_country_indicators <- function(master, regions, module, visualization, source_table){
  legalProblems <- c(
    "F1", "F2", "J1", "J2", "J3", "E3", "C3", "A1", "A2",
    "A3", "G1", "G2", "G3", "E1", "E2", "D1", "D2", "D3",
    "D4", "D5", "D6", "C1", "C2", "C4", "B1", "B2", "B3",
    "B4", "I1", "L1", "L2", "K1", "K2", "K3", "H1", "H2",
    "H3", "J4"
  )
  legprob_bin <- paste0("AJP_", legalProblems, "_bin")
  legprob_sev <- paste0("AJP_", legalProblems, "_sev")

  quality_check <- master %>%
    select(country_year_id, all_of(legprob_bin), all_of(legprob_sev)) %>%
    pivot_longer(
      cols = all_of(legprob_bin),
      names_to = "problem",
      values_to = "answer"
    ) %>%
    mutate(problem = str_remove_all(problem, "AJP_|_bin")) %>%
    left_join(
      master %>%
        select(country_year_id, all_of(legprob_sev)) %>%
        pivot_longer(
          cols = all_of(legprob_sev),
          names_to = "problem",
          values_to = "severity"
        ) %>%
        mutate(problem = str_remove_all(problem, "AJP_|_sev")),
      by = c("country_year_id", "problem")
    ) %>%
    mutate(
      non_trivial = case_when(
        answer == 1 & severity >= 4 & severity < 98 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    group_by(country_year_id) %>%
    summarise(
      total_nontrivial = sum(non_trivial, na.rm = TRUE),
      quality_flag_nontrivial_25 = total_nontrivial > 25,
      .groups = "drop"
    ) %>%
    select(country_year_id, quality_flag_nontrivial_25)

  master_sev <- master %>%
    left_join(quality_check, by = "country_year_id") %>%
    mutate(
      across(
        all_of(legprob_sev),
        ~ ifelse(quality_flag_nontrivial_25, 0,
                 ifelse(.x > 3 & .x < 98, 1,
                        ifelse(.x < 4, 0, NA_real_)))
      )
    ) %>%
    mutate(
      problem_cat_consumer    = if_else(AJP_A1_sev == 1 | AJP_A2_sev == 1 | AJP_A3_sev == 1, 1, 0),
      problem_cat_land        = if_else(AJP_B1_sev == 1 | AJP_B2_sev == 1 | AJP_B3_sev == 1 | AJP_B4_sev == 1, 1, 0),
      problem_cat_housing     = if_else(AJP_C1_sev == 1 | AJP_C2_sev == 1 | AJP_C4_sev == 1, 1, 0),
      problem_cat_family      = if_else(AJP_D1_sev == 1 | AJP_D2_sev == 1 | AJP_D3_sev == 1 |
                                          AJP_D4_sev == 1 | AJP_D5_sev == 1 | AJP_D6_sev == 1, 1, 0),
      problem_cat_education   = if_else(AJP_E1_sev == 1 | AJP_E2_sev == 1, 1, 0),
      problem_cat_accidental  = if_else(AJP_F1_sev == 1 | AJP_F2_sev == 1, 1, 0),
      problem_cat_employment  = if_else(AJP_G1_sev == 1 | AJP_G2_sev == 1 | AJP_G3_sev == 1, 1, 0),
      problem_cat_public      = if_else(AJP_H1_sev == 1 | AJP_H2_sev == 1 | AJP_H3_sev == 1 | AJP_J4_sev == 1, 1, 0),
      problem_cat_law         = if_else(AJP_I1_sev == 1, 1, 0),
      problem_cat_id          = if_else(AJP_J1_sev == 1 | AJP_J2_sev == 1 | AJP_J3_sev == 1, 1, 0),
      problem_cat_money       = if_else(AJP_K1_sev == 1 | AJP_K2_sev == 1 | AJP_K3_sev == 1 |
                                          AJP_L1_sev == 1 | AJP_L2_sev == 1, 1, 0),
      problem_cat_community   = if_else(AJP_E3_sev == 1 | AJP_C3_sev == 1, 1, 0)
    )

  master_sev %>%
    left_join(regions, by = c("country_name_ltn" = "country", "nuts_id")) %>%
    group_by(country_name_ltn, nuts_id) %>%
    summarise(
      total_sample = n(),
      across(starts_with("problem_cat_"), ~ sum(.x, na.rm = TRUE) / total_sample),
      pop_weight = mean(regionpoppct, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(starts_with("problem_cat_"), ~ .x * pop_weight)) %>%
    group_by(country_name_ltn) %>%
    summarise(
      across(starts_with("problem_cat_"), ~ sum(.x, na.rm = TRUE) * 100),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = starts_with("problem_cat_"),
      names_to = "category",
      values_to = "pct"
    ) %>%
    mutate(
      category = str_remove(category, "problem_cat_"),
      category = case_when(
        category == "accidental" ~ "Accidental",
        category == "community"  ~ "Community",
        category == "consumer"   ~ "Consumer",
        category == "education"  ~ "Education",
        category == "employment" ~ "Employment",
        category == "family"     ~ "Family",
        category == "housing"    ~ "Housing",
        category == "id"         ~ "Citinzenship & ID",
        category == "land"       ~ "Land",
        category == "law"        ~ "Law Enforcement",
        category == "money"      ~ "Money & Debt",
        category == "public"     ~ "Public Services"
      ),
      module = module,
      visualization = visualization,
      source_table = source_table,
      country = country_name_ltn,
      grouping = "Category"
    ) %>%
    select(module, visualization, source_table, country, grouping, category, pct)
}

barrier_country_indicators <- function(master, regions, module, visualization, source_table, target_var, filter_var, target_map){
  master %>%
    select(country_name_ltn, nuts_id, target = all_of(target_var), filtering = all_of(filter_var)) %>%
    mutate(target = recode(as.character(target), !!!target_map, .default = NA_character_)) %>%
    filter(filtering == 0 & !is.na(target)) %>%
    group_by(country_name_ltn, nuts_id) %>%
    mutate(total_sample = n()) %>%
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
    mutate(proportion_weighted = (reason_count / total_sample) * weight) %>%
    group_by(country_name_ltn, target) %>%
    summarise(
      pct = sum(proportion_weighted, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    transmute(
      module = module,
      visualization = visualization,
      source_table = source_table,
      country = country_name_ltn,
      grouping = "Category",
      category = target,
      pct
    )
}

perceived_causes_country_indicators <- function(master, regions, module, visualization, source_table){
  master %>%
    select(country_name_ltn, nuts_id, starts_with("AJE_description")) %>%
    pivot_longer(
      !c(country_name_ltn, nuts_id),
      values_to = "mentioned",
      names_to = "category"
    ) %>%
    mutate(
      category = case_when(
        category == "AJE_description_1" ~ "Bad luck / Part of life",
        category == "AJE_description_2" ~ "Bureaucratic",
        category == "AJE_description_3" ~ "Family or private matter",
        category == "AJE_description_4" ~ "Legal",
        category == "AJE_description_5" ~ "Political",
        category == "AJE_description_6" ~ "Social or community matter",
        category == "AJE_description_7" ~ "Economic",
        category == "AJE_description_8" ~ "None of these",
        category == "AJE_description_98" ~ "Don't know",
        category == "AJE_description_99" ~ "No answer"
      ),
      mentioned = case_when(
        mentioned == 1 ~ 1,
        mentioned == 2 ~ 0
      ),
      counter = if_else(is.na(mentioned), 0, 1)
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
    mutate(proportion_weighted = (mentions / total_sample) * weight) %>%
    group_by(country_name_ltn, category) %>%
    summarise(
      pct = sum(proportion_weighted, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    transmute(
      module = module,
      visualization = visualization,
      source_table = source_table,
      country = country_name_ltn,
      grouping = "Category",
      category,
      pct
    )
}

hardships_country_indicators <- function(master, regions, module, visualization, source_table){
  target_vars <- c(
    "hardships_health", "hardships_emotional",
    "hardships_income", "hardships_drugs"
  )

  map_dfr(
    target_vars,
    function(var){
      get_results_table(master, regions, target = var) %>%
        filter(grouping == "National") %>%
        mutate(category = var) %>%
        as_country_indicators(module, visualization, source_table)
    }
  )
}

legal_empowerment_country_indicators <- function(master, regions, module, visualization, source_table, target, category_label){
  get_results_table(
    master,
    regions,
    target = target,
    ctype = "short"
  ) %>%
    filter(grouping == "National") %>%
    mutate(category = category_label) %>%
    as_country_indicators(module, visualization, source_table)
}

results_table_country_indicators <- function(master, regions, module, visualization, source_table, target, ctype = "full", transform_var = NULL){
  get_results_table(
    master,
    regions,
    target = target,
    ctype = ctype,
    transform_var = transform_var
  ) %>%
    as_country_indicators(module, visualization, source_table)
}

status_rp_country_indicators <- function(master, regions){
  status_rp_visual(master, regions)
}

advice_sankey_country_indicators <- function(master, regions){
  stage_labels <- c(
    "sankey_advice_stage_1" = "Contacted an Advisor",
    "sankey_advice_stage_2" = "Type of Advisor / Reason for No Contact",
    "sankey_advice_stage_3" = "Access to Adequate Assistance and Representation"
  )

  countries <- sort(unique(master$country_name_ltn))

  map_dfr(
    countries,
    function(country){
      sankey_base_data <- master %>%
        filter(country_name_ltn == country) %>%
        select(country_year_id, country_name_ltn, nuts_id, starts_with("sankey_advice_stage_")) %>%
        filter(!is.na(sankey_advice_stage_1) & !is.na(sankey_advice_stage_2)) %>%
        group_by(nuts_id) %>%
        mutate(nuts_sample = n())

      if (nrow(sankey_base_data) == 0) return(tibble())

      stage_1 <- sankey_base_data %>%
        group_by(country_name_ltn, nuts_id, nuts_sample, sankey_advice_stage_1) %>%
        summarise(count = n(), .groups = "keep") %>%
        left_join(regions %>% select(nuts_id, wgt = regionpoppct), by = "nuts_id") %>%
        mutate(value = (count / nuts_sample) * wgt) %>%
        group_by(country_name_ltn, sankey_advice_stage_1) %>%
        summarise(pct = sum(value, na.rm = TRUE) * 100, .groups = "drop") %>%
        transmute(
          module = "2.2",
          visualization = "Sankey: Advice and Representation",
          source_table = "sankey_advice_representation (derived)",
          country = country_name_ltn,
          grouping = stage_labels[["sankey_advice_stage_1"]],
          category = sankey_advice_stage_1,
          pct
        )

      stage_2 <- sankey_base_data %>%
        group_by(country_name_ltn, nuts_id, nuts_sample, sankey_advice_stage_2) %>%
        summarise(count = n(), .groups = "keep") %>%
        left_join(regions %>% select(nuts_id, wgt = regionpoppct), by = "nuts_id") %>%
        mutate(value = (count / nuts_sample) * wgt) %>%
        group_by(country_name_ltn, sankey_advice_stage_2) %>%
        summarise(pct = sum(value, na.rm = TRUE) * 100, .groups = "drop") %>%
        transmute(
          module = "2.2",
          visualization = "Sankey: Advice and Representation",
          source_table = "sankey_advice_representation (derived)",
          country = country_name_ltn,
          grouping = stage_labels[["sankey_advice_stage_2"]],
          category = sankey_advice_stage_2,
          pct
        )

      access2rep_stage_2 <- c(
        "Appropriate Advisor",
        "Non-Appropriate Advisor",
        "Needed but did not have access to an advisor"
      )

      stage_3 <- sankey_base_data %>%
        ungroup() %>%
        filter(sankey_advice_stage_2 %in% access2rep_stage_2) %>%
        select(-nuts_sample) %>%
        group_by(nuts_id) %>%
        mutate(nuts_sample = n()) %>%
        group_by(country_name_ltn, nuts_id, nuts_sample, sankey_advice_stage_2) %>%
        summarise(count = n(), .groups = "keep") %>%
        left_join(regions %>% select(nuts_id, wgt = regionpoppct), by = "nuts_id") %>%
        mutate(value = (count / nuts_sample) * wgt) %>%
        group_by(country_name_ltn, sankey_advice_stage_2) %>%
        summarise(national_avg = sum(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(
          sankey_advice_stage_3 = case_when(
            sankey_advice_stage_2 == "Appropriate Advisor" ~ "Had access",
            sankey_advice_stage_2 %in% c(
              "Non-Appropriate Advisor",
              "Needed but did not have access to an advisor"
            ) ~ "Had no access"
          )
        ) %>%
        group_by(country_name_ltn, sankey_advice_stage_3) %>%
        summarise(pct = sum(national_avg, na.rm = TRUE) * 100, .groups = "drop") %>%
        transmute(
          module = "2.2",
          visualization = "Sankey: Advice and Representation",
          source_table = "sankey_advice_representation (derived)",
          country = country_name_ltn,
          grouping = stage_labels[["sankey_advice_stage_3"]],
          category = sankey_advice_stage_3,
          pct
        )

      bind_rows(stage_1, stage_2, stage_3)
    }
  )
}

drm_sankey_country_indicators <- function(master, regions){
  stage_labels <- c(
    "sankey_drm_stage_0" = "Contacted a DRM",
    "sankey_drm_stage_1" = "Reason for No Contact",
    "sankey_drm_stage_2" = "Access to a DRM (SDG 16.3.3)"
  )

  countries <- sort(unique(master$country_name_ltn))

  map_dfr(
    countries,
    function(country){
      sankey_base_data <- master %>%
        filter(country_name_ltn == country) %>%
        select(country_year_id, country_name_ltn, nuts_id, starts_with("sankey_drm_stage_")) %>%
        filter(!is.na(sankey_drm_stage_1))

      if (nrow(sankey_base_data) == 0) return(tibble())

      stage_1 <- sankey_base_data %>%
        filter(sankey_drm_stage_1 != "Turned to a DRM") %>%
        group_by(nuts_id) %>%
        mutate(nuts_sample = n()) %>%
        group_by(country_name_ltn, nuts_id, nuts_sample, sankey_drm_stage_1) %>%
        summarise(count = n(), .groups = "keep") %>%
        left_join(regions %>% select(nuts_id, wgt = regionpoppct), by = "nuts_id") %>%
        mutate(value = (count / nuts_sample) * wgt) %>%
        group_by(country_name_ltn, sankey_drm_stage_1) %>%
        summarise(pct = sum(value, na.rm = TRUE) * 100, .groups = "drop") %>%
        transmute(
          module = "2.3",
          visualization = "Sankey: Dispute Resolution Mechanisms",
          source_table = "sankey_drm (derived)",
          country = country_name_ltn,
          grouping = stage_labels[["sankey_drm_stage_1"]],
          category = sankey_drm_stage_1,
          pct
        )

      stage_2 <- sankey_base_data %>%
        filter(!is.na(sankey_drm_stage_2)) %>%
        group_by(nuts_id) %>%
        mutate(nuts_sample = n()) %>%
        group_by(country_name_ltn, nuts_id, nuts_sample, sankey_drm_stage_2) %>%
        summarise(count = n(), .groups = "keep") %>%
        left_join(regions %>% select(nuts_id, wgt = regionpoppct), by = "nuts_id") %>%
        mutate(value = (count / nuts_sample) * wgt) %>%
        group_by(country_name_ltn, sankey_drm_stage_2) %>%
        summarise(pct = sum(value, na.rm = TRUE) * 100, .groups = "drop") %>%
        transmute(
          module = "2.3",
          visualization = "Sankey: Dispute Resolution Mechanisms",
          source_table = "sankey_drm (derived)",
          country = country_name_ltn,
          grouping = stage_labels[["sankey_drm_stage_2"]],
          category = sankey_drm_stage_2,
          pct
        )

      stage_0 <- sankey_base_data %>%
        group_by(nuts_id) %>%
        mutate(nuts_sample = n()) %>%
        group_by(country_name_ltn, nuts_id, nuts_sample, sankey_drm_stage_1) %>%
        summarise(count = n(), .groups = "keep") %>%
        left_join(regions %>% select(nuts_id, wgt = regionpoppct), by = "nuts_id") %>%
        mutate(value = (count / nuts_sample) * wgt) %>%
        group_by(country_name_ltn, sankey_drm_stage_1) %>%
        summarise(national_avg = sum(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(
          sankey_drm_stage_0 = if_else(
            sankey_drm_stage_1 == "Turned to a DRM",
            "Contacted a DRM",
            "Did not contact a DRM"
          )
        ) %>%
        group_by(country_name_ltn, sankey_drm_stage_0) %>%
        summarise(pct = sum(national_avg, na.rm = TRUE) * 100, .groups = "drop") %>%
        transmute(
          module = "2.3",
          visualization = "Sankey: Dispute Resolution Mechanisms",
          source_table = "sankey_drm (derived)",
          country = country_name_ltn,
          grouping = stage_labels[["sankey_drm_stage_0"]],
          category = sankey_drm_stage_0,
          pct
        )

      bind_rows(stage_0, stage_1, stage_2)
    }
  )
}

country_visual_indicators <- function(master, regions){
  bind_rows(
    results_table_country_indicators(master, regions, "1", "Prevalence of Non-Trivial Problems", "1_2_prevalence_nontrivial_problems.csv", "prevalence2"),
    prevalence_category_country_indicators(master, regions, "1", "Prevalence of Non-Trivial Problems by Category", "1_3_prevalence_nontrivial_problems_by_category.csv"),
    results_table_country_indicators(master, regions, "2.1", "Access to Adequate Information and Advice", "2_1_1_access_to_information.csv", "access2info"),
    results_table_country_indicators(master, regions, "2.1", "Perceptions of Access to Legal Information and Advice", "2_1_3_access_to_information_gpp.csv", "JSE_access2info", "short", binary_transform),
    results_table_country_indicators(master, regions, "2.2", "Access to Adequate Assistance and Representation", "2_2_1_access_to_representation.csv", "access2rep"),
    results_table_country_indicators(master, regions, "2.2", "Perceptions of Access to Legal Assistance and Representation", "2_2_3_access_to_representation_gpp.csv", "JSE_access2assis", "short", binary_transform),
    barrier_country_indicators(master, regions, "2.2", "Mechanisms Preventing Access to Assistance and Representation", "2_2_5_access2representation_barriers.csv", "AJD_noadvice_reason", "access2rep", c(
      "1" = "Issue was not important",
      "2" = "Other side was right",
      "3" = "Not needed advice",
      "4" = "Financial cost",
      "5" = "Previous experiences",
      "6" = "Did not know who to contact",
      "7" = "Did not know they could get advice",
      "8" = "Fear",
      "9" = "Advisers were too far away",
      "10" = "Other",
      "98" = "Don't Know / No Answer",
      "99" = "Don't Know / No Answer"
    )),
    results_table_country_indicators(master, regions, "2.3", "Access to Dispute Resolution Mechanisms", "2_3_1_access_to_DRM.csv", "access2drm"),
    barrier_country_indicators(master, regions, "2.3", "Mechanisms Preventing Access to Dispute Resolution Mechanisms", "2_3_5_access2DRM_barriers.csv", "AJR_noresol_reason", "access2drm", c(
      "1" = "Issue was not important",
      "2" = "Respondent was confidence to solve the issue alone",
      "3" = "Time or bureaucracy",
      "4" = "No evidence or strong case",
      "5" = "Fear of retaliation",
      "6" = "Did not know what to do or where to go",
      "7" = "Access problems",
      "8" = "Lack of trust in authorities",
      "9" = "Fear of consequences",
      "10" = "It was up to the other party",
      "11" = "Other",
      "98" = "DK/NA",
      "99" = "DK/NA"
    )),
    results_table_country_indicators(master, regions, "2.4", "Dispute Resolution Processes that Finalized in Less than One Year", "2_4_1_timeliness.csv", "rp_time"),
    results_table_country_indicators(master, regions, "2.4", "Perceived Quickness of the Dispute Resolution Process", "2_4_2_quickness.csv", "rp_quick"),
    results_table_country_indicators(master, regions, "2.4", "Perceived Fairness of the Dispute Resolution Process", "2_4_3_fairness.csv", "rp_fair"),
    results_table_country_indicators(master, regions, "2.4", "Costliness of Dispute Resolution Processes", "2_4_4_costliness.csv", "rp_cost"),
    results_table_country_indicators(master, regions, "2.4", "Dispute Resolution Processes Regarded as Expensive", "2_4_5_expensive.csv", "rp_expensive"),
    results_table_country_indicators(master, regions, "2.5", "Outcomes Obtained by Users of Dispute Resolution Processes", "2_5_1_outcome.csv", "rp_outcome"),
    results_table_country_indicators(master, regions, "2.5", "Satisfaction with the Resolution Outcome", "2_5_2_satisfaction.csv", "rp_satisfaction", "custom_1"),
    results_table_country_indicators(master, regions, "3", "Justice Gap (with DK)", "3_1_justice_gap_keepdk.csv", "inside_justice_gap_keepdk", "short"),
    hardships_country_indicators(master, regions, "3", "Hardships Experienced by People in their Justice Pathway", "3_4_hardships.csv"),
    results_table_country_indicators(master, regions, "4", "Knowledge of Rights", "4_1_knowledge_rights.csv", "AJE_legalrights", "short", binary_transform),
    perceived_causes_country_indicators(master, regions, "4", "Perceived Causes", "4_2_perceived_causes.csv"),
    results_table_country_indicators(master, regions, "4", "Expert Support", "4_3_expert_support.csv", "AJE_advice", "short", binary_transform),
    legal_empowerment_country_indicators(master, regions, "4", "Legal Empowerment: Written Work Agreement", "4_4_legal_empowerment_contract.csv", "vulnerability3", "Written work agreement"),
    legal_empowerment_country_indicators(master, regions, "4", "Legal Empowerment: Proof of Housing or Land Tenure", "4_4_legal_empowerment_housing.csv", "vulnerability2", "Proof of housing or land tenure"),
    legal_empowerment_country_indicators(master, regions, "4", "Legal Empowerment: Official Proof of Identity", "4_4_legal_empowerment_identity.csv", "vulnerability1", "Official proof of identity"),
    results_table_country_indicators(master, regions, "5", "Public Trust in Judges", "5_1_trust_judges.csv", "TRT_judges", "trust", binary_transform),
    results_table_country_indicators(master, regions, "5", "Public Trust in Prosecutors", "5_1_trust_prosecutors.csv", "TRT_prosecutors", "trust", binary_transform),
    results_table_country_indicators(master, regions, "5", "Public Trust in Public Defense Attorneys", "5_1_trust_pda.csv", "TRT_pda", "trust", binary_transform)
  )
}

eu_average_of_country_averages <- function(country_indicators){
  country_indicators %>%
    group_by(module, visualization, source_table, grouping, category) %>%
    summarise(
      eu_average = if (all(is.na(pct))) NA_real_ else mean(pct, na.rm = TRUE),
      n_countries = sum(!is.na(pct)),
      countries_included = paste(sort(country[!is.na(pct)]), collapse = "; "),
      min_country_average = if (all(is.na(pct))) NA_real_ else min(pct, na.rm = TRUE),
      max_country_average = if (all(is.na(pct))) NA_real_ else max(pct, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_countries > 0) %>%
    arrange(module, visualization, grouping, category)
}

eu_main_indicators <- function(path2EU, regions){
  eu_master <- build_eu_master(path2EU, regions)

  country_indicators <- bind_rows(
    country_visual_indicators(eu_master, regions),
    status_rp_country_indicators(eu_master, regions),
    advice_sankey_country_indicators(eu_master, regions),
    drm_sankey_country_indicators(eu_master, regions)
  ) %>%
    add_visual_n(eu_master) %>%
    arrange(module, visualization, source_table, country, grouping, category)

  eu_averages <- eu_average_of_country_averages(country_indicators)

  write_csv(country_indicators, "output/tabs/csv/eu_main_indicators_country_averages.csv")
  write_csv(eu_averages, "output/tabs/csv/eu_main_indicators.csv")

  writexl::write_xlsx(
    list(
      "eu_averages" = eu_averages,
      "country_averages" = country_indicators
    ),
    "output/tabs/eu_main_indicators.xlsx"
  )

  return(
    list(
      eu_averages = eu_averages,
      country_averages = country_indicators
    )
  )
}
