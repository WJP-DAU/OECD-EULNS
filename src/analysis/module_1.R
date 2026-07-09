## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Routines for the "Encountering Justice Problems in Italy" module
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     July 23, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Most frequent problems by level of severity ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

freq_probs_by_severity <- function(master, regions){

  # Build quality check flag (exclude people with >25 non-trivial problems)
  legalProblems_qc <- c(
    "A1", "A2", "A3", "B1", "B2", "B3", "B4", "C1", "C2", "C3", "C4",
    "D1", "D2", "D3", "D4", "D5", "D6", "E1", "E2", "E3", "F1", "F2",
    "G1", "G2", "G3", "H1", "H2", "H3", "I1", "J1", "J2", "J3", "J4",
    "K1", "K2", "K3", "L1", "L2"
  )
  legprob_bin_qc <- paste0("AJP_", legalProblems_qc, "_bin")
  legprob_sev_qc <- paste0("AJP_", legalProblems_qc, "_sev")

  quality_check <- master %>%
    select(country_year_id, all_of(legprob_bin_qc), all_of(legprob_sev_qc)) %>%
    pivot_longer(
      cols = all_of(legprob_bin_qc),
      names_to = "problem",
      values_to = "answer"
    ) %>%
    mutate(problem = str_remove_all(problem, "AJP_|_bin")) %>%
    left_join(
      master %>%
        select(country_year_id, all_of(legprob_sev_qc)) %>%
        pivot_longer(
          cols = all_of(legprob_sev_qc),
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

  total_sample_df <- master %>%
    group_by(country_name_ltn, nuts_id) %>%
    summarise(
      total_sample = n(),
      .groups = "keep"
    )

  base_df <- master %>%
    left_join(quality_check, by = "country_year_id") %>%
    select(
      country_year_id, country_name_ltn, nuts_id, quality_flag_nontrivial_25,
      ends_with(c("_bin", "_sev"))
    ) %>%
    select(
      -starts_with("AJR")
    ) %>%
    pivot_longer(
      !c(country_year_id, country_name_ltn, nuts_id, quality_flag_nontrivial_25),
      names_to = c("problem", "variable"),
      names_pattern = "AJP_(.*)_(.*)",
      values_to = "value"
    ) %>%
    pivot_wider(
      id_cols = c(country_year_id, country_name_ltn, nuts_id, quality_flag_nontrivial_25, problem),
      names_from = variable,
      values_from = value
    ) %>%
    mutate(
      # Apply quality check: set bin to NA for flagged people
      bin = case_when(
        quality_flag_nontrivial_25 ~ NA_real_,
        bin == 1 ~ 1,
        bin == 2 ~ 0
      ),
      severity_group = case_when(
        quality_flag_nontrivial_25 ~ NA_character_,
        sev %in% c(0,1,2,3)  ~ "Low",
        sev %in% c(4,5,6)    ~ "Medium",
        sev %in% c(7,8,9,10) ~ "High",
        sev %in% c(98,99)    ~ "Unknown",
        is.na(sev) ~ "Unknown"
      )
    ) %>%
    select(-quality_flag_nontrivial_25)
  
  levels <- c("National", "High", "Medium", "Low")
  names(levels) <- levels
  
  results_tbl <- lapply(
    levels,
    function(level){
      
      if (level == "National") {
        subset_df <- base_df
      } else {
        subset_df <- base_df %>%
          filter(severity_group == level)
      }
      
      results_by_region <- subset_df %>%
        group_by(country_name_ltn, nuts_id, problem) %>%
        summarise(
          frequency = sum(bin, na.rm = TRUE),
          .groups = "keep"
        ) %>%
        left_join(
          total_sample_df,
          by = c("country_name_ltn", "nuts_id")
        ) %>%
        mutate(
          proportion = frequency/total_sample
        )
      
      results_by_country <- get_wgt_national_avg(
        results_by_region,
        regions,
        target_col  = "proportion",
        group_col   = "problem"
      ) %>%
        group_by(country_name_ltn) %>%
        slice_max(
          order_by = proportion,
          n = 10,
          with_ties = FALSE
        ) %>%
        mutate(
          proportion = proportion*100
        )
      
      country_cols <- lapply(study_countries, function(cntry) {
        results_by_country %>% ungroup() %>%
          filter(country_name_ltn == cntry) %>%
          select(-country_name_ltn) %>%
          setNames(c(paste0("problem_", tolower(cntry)), paste0("proportion_", tolower(cntry))))
      })

      country_table <- bind_cols(
        as.data.frame(
          list(
            "level" = rep(level, 10),
            "rank"  = seq(1,10,1)
          )
        ),
        country_cols
      )
      
      return(country_table)

    }
  ) %>%
    reduce(bind_rows)
  
  write_csv(results_tbl, "output/tabs/csv/1_1_freq_probs_by_severity.csv")
  
  header_spec <- c(" " = 1, setNames(rep(2, length(study_countries)), study_countries))
  col_names_spec <- c("Rank", rep(c("Problem", "%"), length(study_countries)))

  suppressMessages(
    kableExtra::kbl(
      results_tbl %>% select(-level),
      digits = 1,
      align = "c",
      caption = "Most Frequent Problems By Severity Level",
      col.names = col_names_spec
    ) %>%
      kableExtra::add_header_above(header_spec) %>%
      kableExtra::pack_rows("National", 1, 10) %>%
      kableExtra::pack_rows("Severity: High", 11, 20) %>%
      kableExtra::pack_rows("Severity: Medium", 21, 30) %>%
      kableExtra::pack_rows("Severity: Low", 31, 40) %>%
      kableExtra::kable_classic(
        # full_width = F, 
        html_font = "Cambria"
      ) %>% 
      kableExtra::kable_styling(
        font_size = 18,
        latex_options = "striped"
      ) %>%
      safe_save_kable("output/tabs/png/1_1_freq_probs_by_severity.png")
  )
    
  return(results_tbl)
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Prevalence of non-trivial problems ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

prevalence_nontrivial_problems <- function(master, regions){
  
  results_tbl <- get_results_table(
    master, 
    regions, 
    target = "prevalence2",
    ctype = "short"
  )
  write_csv(results_tbl, "output/tabs/csv/1_2_prevalence_nontrivial_problems.csv")
  export_results_kable(
    results_tbl, 
    title = "Prevalence Of Non-Trivial Problems",
    file  = "output/tabs/png/1_2_prevalence_nontrivial_problems.png",
    ctype = "short"
  )
  
  return(results_tbl)
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Prevalence of non-trivial problems by category ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

prevalence_nontrivial_problems_by_category <- function(master, regions){
  legalProblems <- c(
    "F1", "F2", "J1", "J2", "J3", "E3", "C3", "A1", "A2",
    "A3", "G1", "G2", "G3", "E1", "E2", "D1", "D2", "D3",
    "D4", "D5", "D6", "C1", "C2", "C4", "B1", "B2", "B3",
    "B4", "I1", "L1", "L2", "K1", "K2", "K3", "H1", "H2",
    "H3", "J4"
  )
  legprob_bin <- paste0("AJP_", legalProblems, "_bin")
  legprob_sev <- paste0("AJP_", legalProblems, "_sev")

  # Build quality check flag (exclude people with >25 non-trivial problems)
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

  # convert severity to 0/1, applying quality check (0 for flagged people in categories)
  master_sev <- master %>%
    left_join(quality_check, by = "country_year_id") %>%
    mutate(
      across(
        all_of(legprob_sev),
        ~ ifelse(quality_flag_nontrivial_25, 0,
                 ifelse(. > 3 & . < 98, 1,
                        ifelse(. < 4, 0, NA_real_)))
      )
    ) %>%
    select(-quality_flag_nontrivial_25)
  
  master_sev <- master_sev %>%
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
  
  cat_by_region <- master_sev %>%
    left_join(regions, by = c("country_name_ltn" = "country", "nuts_id")) %>%
    group_by(country_name_ltn, nuts_id) %>%
    summarise(
      total_sample = n(),
      problem_cat_consumer    = sum(problem_cat_consumer, na.rm = TRUE)/total_sample,
      problem_cat_land        = sum(problem_cat_land, na.rm = TRUE)/total_sample,
      problem_cat_housing     = sum(problem_cat_housing, na.rm = TRUE)/total_sample,
      problem_cat_family      = sum(problem_cat_family, na.rm = TRUE)/total_sample,
      problem_cat_education   = sum(problem_cat_education, na.rm = TRUE)/total_sample,
      problem_cat_accidental  = sum(problem_cat_accidental, na.rm = TRUE)/total_sample,
      problem_cat_employment  = sum(problem_cat_employment, na.rm = TRUE)/total_sample,
      problem_cat_public      = sum(problem_cat_public, na.rm = TRUE)/total_sample,
      problem_cat_law         = sum(problem_cat_law, na.rm = TRUE)/total_sample,
      problem_cat_id          = sum(problem_cat_id, na.rm = TRUE)/total_sample,
      problem_cat_money       = sum(problem_cat_money, na.rm = TRUE)/total_sample,
      problem_cat_community   = sum(problem_cat_community, na.rm = TRUE)/total_sample,
      pop_weight              = mean(regionpoppct, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # multiply by pop weight
    mutate(
      across(starts_with("problem_cat_"), ~.x * pop_weight)
    )
  
  cat_by_country <- cat_by_region %>%
    group_by(country_name_ltn) %>%
    summarise(
      across(
        starts_with("problem_cat_"),
        \(x) sum(x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    filter(country_name_ltn %in% study_countries) %>%
    mutate(across(starts_with("problem_cat_"), ~ . * 100))
  
  legal_problem_summary <- cat_by_country %>%
    pivot_longer(
      cols = starts_with("problem_cat_"),
      names_to = "category",
      values_to = "value2plot"
    )
  
  results_tbl <- legal_problem_summary %>%
    pivot_wider(
      id_cols = category,
      names_from = country_name_ltn,
      values_from = value2plot
    ) %>%
    mutate(
      category = str_remove(
        category, "problem_cat_"
      ),
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
      )
    ) %>%
    arrange(category)
  
  write_csv(results_tbl, "output/tabs/csv/1_3_prevalence_nontrivial_problems_by_category.csv")
  
  suppressMessages(
    kableExtra::kbl(
      results_tbl,
      digits = 1,
      caption = "Prevalence Of Non-Trivial Problems By Category",
      col.names = c("Category", study_countries)
    ) %>%
      kableExtra::kable_classic(
        # full_width = F, 
        html_font = "Cambria"
      ) %>% 
      kableExtra::kable_styling(
        font_size = 18,
        latex_options = "striped"
      ) %>%
      safe_save_kable("output/tabs/png/1_3_prevalence_nontrivial_problems_by_category.png")
  )
  
  return(results_tbl)
}

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Co-occurence of non-trivial problems ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cooccurence_nproblems <- function(master, regions){
  
  results_tbl <- get_results_table(
    master %>% 
      mutate(
        nproblems = if_else(
          nproblems==0, NA_integer_, nproblems
        )
      ), 
    regions, 
    target = "nproblems",
    ctype = "short",
    prop = FALSE
  )
  write_csv(results_tbl, "output/tabs/csv/1_3_cooccurence_nproblems.csv")
  export_results_kable(
    results_tbl, 
    title = "Co-Occurence of Justice Problems (Number of Problems)",
    file  = "output/tabs/png/1_3_cooccurence_nproblems.png",
    ctype = "short"
  )
  
  return(results_tbl)
}


cooccurence_network <- function(master, regions){

  lapply(
    study_countries,
    function(country){

      master_country <- master %>% filter(country_name_ltn == country)

      legalProblems <- c(
        "F1", "F2", "J1", "J2", "J3", "E3", "C3", "A1", "A2",
        "A3", "G1", "G2", "G3", "E1", "E2", "D1", "D2", "D3",
        "D4", "D5", "D6", "C1", "C2", "C4", "B1", "B2", "B3",
        "B4", "I1", "L1", "L2", "K1", "K2", "K3", "H1", "H2",
        "H3", "J4"
      )
      legprob_bin <- paste0("AJP_", legalProblems, "_bin")
      legprob_sev <- paste0("AJP_", legalProblems, "_sev")

      # Build quality check flag (exclude people with >25 non-trivial problems)
      quality_check <- master_country %>%
        select(country_year_id, all_of(legprob_bin), all_of(legprob_sev)) %>%
        pivot_longer(
          cols = all_of(legprob_bin),
          names_to = "problem",
          values_to = "answer"
        ) %>%
        mutate(problem = str_remove_all(problem, "AJP_|_bin")) %>%
        left_join(
          master_country %>%
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

      # Binary data with quality check applied
      data <- master_country %>%
        left_join(quality_check, by = "country_year_id") %>%
        select(
          country_year_id, quality_flag_nontrivial_25,
          all_of(legprob_bin)
        ) %>%
        mutate(
          across(
            starts_with("AJP_"),
            \(x) case_when(
              quality_flag_nontrivial_25 ~ NA_real_,
              x == 1 ~ 1,
              x == 2 ~ 0
            )
          )
        ) %>%
        select(-quality_flag_nontrivial_25)
      
      # Map: Problems -> Categories
      macro_categories <- data.frame(
        problem = legprob_bin[order(names(setNames(legprob_bin, legprob_bin)))],
        category = c(
          "Consumer", "Consumer", "Consumer",
          "Land", "Land", "Land", "Land",
          "Housing", "Housing", "Community", "Housing",
          "Family", "Family", "Family", "Family", "Family", "Family",
          "Education", "Education", "Community",
          "Accidental", "Accidental",
          "Employment", "Employment", "Employment",
          "Public Services", "Public Services", "Public Services",
          "Law Enforcement",
          "Citizenship & ID", "Citizenship & ID", "Citizenship & ID", "Public Services",
          "Money & Debt", "Money & Debt", "Money & Debt", "Money & Debt", "Money & Debt"
        )
      )
      
      # Replace NA with 0 for frequency and co-occurrence calculations
      # (NA means person is excluded from quality check, so should not contribute)
      data_matrix <- data[, legprob_bin]
      data_matrix[is.na(data_matrix)] <- 0

      # Total frequencies
      frequencies <- colSums(data_matrix)

      # Co-occurence Matrix
      cooccur_matrix <- t(as.matrix(data_matrix)) %*%
        as.matrix(data_matrix)
      diag(cooccur_matrix) <- 0
      
      min_cooccurrence = 1
      edges <- which(cooccur_matrix >= min_cooccurrence, arr.ind = TRUE)
      edge_list <- data.frame(
        from = rownames(cooccur_matrix)[edges[, 1]],
        to = colnames(cooccur_matrix)[edges[, 2]],
        weight = cooccur_matrix[edges]
      )
      edge_list <- edge_list[edge_list$from < edge_list$to, ]  # Remove duplicate edges (keep only upper triangle)
      
      # Node list with attributes
      node_list <- data.frame(
        name = legprob_bin,
        frequency = frequencies,
        category = macro_categories$category[match(legprob_bin, macro_categories$problem)]
      )
      
      # Create igraph object
      g <- igraph::graph_from_data_frame(
        edge_list, 
        directed = FALSE, 
        vertices = node_list
      )
      
      # Add node attributes
      igraph::V(g)$frequency <- node_list$frequency
      igraph::V(g)$category <- node_list$category
      
      # Calculate centrality measures (for positioning highly connected nodes in center)
      igraph::V(g)$degree <- igraph::degree(g)
      igraph::V(g)$betweenness <- igraph::betweenness(g, normalized = TRUE)
      igraph::V(g)$closeness <- igraph::closeness(g, normalized = TRUE)
        
      # Network Chart
      layout <- ggraph::create_layout(g, layout = "fr")
      
      p <- ggraph::ggraph(layout) +
        ggraph::geom_edge_link(
          # aes(width = weight), 
          alpha = 0.6, 
          color = "gray60",
          width = 0.25
        ) +
        ggraph::geom_node_point(
          aes(size = frequency, 
              color = category),
          alpha = 0.8
        ) +
        ggraph::geom_node_text(
          aes(label = str_remove_all(name, "AJP|bin|_")),
          fontface = "bold",
          size = 2.5,
          # vjust = 1.5,
          hjust = 0.5
        ) +
        scale_size_continuous(
          name = "Frequency", 
          range = c(2, 15),
          guide = "none"
        ) +
        paletteer::scale_colour_paletteer_d(
          "PrettyCols::Autumn",
          name = ""
        ) +
        ggraph::scale_edge_width_continuous(
          name = "Co-occurrence", 
          range = c(0.5, 3)
        ) +
        ggraph::theme_graph() +
        theme(
          legend.position = "top",
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic")
        ) +
        labs(
          title = "Justice Problems Co-occurrence Network",
          subtitle = "Node size = frequency, Node color = category, Node position = co-occurrence"
        )
      
      ggsave(
        glue::glue("output/tabs/png/1_4_cooccurence_network_{country}.png"),
        width = 8,
        height = 6
      )

    }
  )
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5.  Lift Analysis - Co-occurrence strength between problem CATEGORIES ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

lift_analysis <- function(master, regions) {

  legalProblems <- c(
    "F1", "F2", "J1", "J2", "J3", "E3", "C3", "A1", "A2",
    "A3", "G1", "G2", "G3", "E1", "E2", "D1", "D2", "D3",
    "D4", "D5", "D6", "C1", "C2", "C4", "B1", "B2", "B3",
    "B4", "I1", "L1", "L2", "K1", "K2", "K3", "H1", "H2",
    "H3", "J4"
  )
  legprob_bin <- paste0("AJP_", legalProblems, "_bin")
  legprob_sev <- paste0("AJP_", legalProblems, "_sev")

  # Category names for output
  category_names <- c(
    "Accidental", "Citizenship & ID", "Community", "Consumer",
    "Education", "Employment", "Family", "Housing",
    "Land", "Law Enforcement", "Money & Debt", "Public Services"
  )

  # Store results for all countries
  all_top_pairs <- list()

  lapply(
    study_countries,
    function(country) {

      master_country <- master %>% filter(country_name_ltn == country)

      # Build quality check flag (exclude people with >25 non-trivial problems)
      quality_check <- master_country %>%
        select(country_year_id, all_of(legprob_bin), all_of(legprob_sev)) %>%
        pivot_longer(
          cols = all_of(legprob_bin),
          names_to = "problem",
          values_to = "answer"
        ) %>%
        mutate(problem = str_remove_all(problem, "AJP_|_bin")) %>%
        left_join(
          master_country %>%
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

      # Create category-level binary variables (1 if ANY problem in category, severity >= 4)
      master_sev <- master_country %>%
        left_join(quality_check, by = "country_year_id") %>%
        mutate(
          across(
            all_of(legprob_sev),
            ~ ifelse(quality_flag_nontrivial_25, 0,
                     ifelse(. > 3 & . < 98, 1,
                            ifelse(. < 4, 0, NA_real_)))
          )
        ) %>%
        select(-quality_flag_nontrivial_25)

      # Aggregate to category level
      master_cat <- master_sev %>%
        mutate(
          cat_accidental    = if_else(AJP_F1_sev == 1 | AJP_F2_sev == 1, 1, 0),
          cat_citizenship   = if_else(AJP_J1_sev == 1 | AJP_J2_sev == 1 | AJP_J3_sev == 1, 1, 0),
          cat_community     = if_else(AJP_E3_sev == 1 | AJP_C3_sev == 1, 1, 0),
          cat_consumer      = if_else(AJP_A1_sev == 1 | AJP_A2_sev == 1 | AJP_A3_sev == 1, 1, 0),
          cat_education     = if_else(AJP_E1_sev == 1 | AJP_E2_sev == 1, 1, 0),
          cat_employment    = if_else(AJP_G1_sev == 1 | AJP_G2_sev == 1 | AJP_G3_sev == 1, 1, 0),
          cat_family        = if_else(AJP_D1_sev == 1 | AJP_D2_sev == 1 | AJP_D3_sev == 1 |
                                        AJP_D4_sev == 1 | AJP_D5_sev == 1 | AJP_D6_sev == 1, 1, 0),
          cat_housing       = if_else(AJP_C1_sev == 1 | AJP_C2_sev == 1 | AJP_C4_sev == 1, 1, 0),
          cat_land          = if_else(AJP_B1_sev == 1 | AJP_B2_sev == 1 | AJP_B3_sev == 1 | AJP_B4_sev == 1, 1, 0),
          cat_law_enforc    = if_else(AJP_I1_sev == 1, 1, 0),
          cat_money_debt    = if_else(AJP_K1_sev == 1 | AJP_K2_sev == 1 | AJP_K3_sev == 1 |
                                        AJP_L1_sev == 1 | AJP_L2_sev == 1, 1, 0),
          cat_public_serv   = if_else(AJP_H1_sev == 1 | AJP_H2_sev == 1 | AJP_H3_sev == 1 | AJP_J4_sev == 1, 1, 0)
        ) %>%
        select(country_year_id, starts_with("cat_"))

      # Extract category columns for matrix calculations
      cat_cols <- c("cat_accidental", "cat_citizenship", "cat_community", "cat_consumer",
                    "cat_education", "cat_employment", "cat_family", "cat_housing",
                    "cat_land", "cat_law_enforc", "cat_money_debt", "cat_public_serv")

      data_matrix <- master_cat[, cat_cols]
      data_matrix[is.na(data_matrix)] <- 0

      # Total sample size
      n_total <- nrow(data_matrix)

      # Marginal probabilities P(category)
      frequencies <- colSums(data_matrix)
      prob_marginal <- frequencies / n_total

      # Co-occurrence matrix
      cooccur_matrix <- t(as.matrix(data_matrix)) %*% as.matrix(data_matrix)
      diag(cooccur_matrix) <- 0

      # Joint probability P(A ∩ B)
      prob_joint <- cooccur_matrix / n_total

      # Expected probability under independence P(A) * P(B)
      prob_expected <- outer(prob_marginal, prob_marginal)

      # Lift matrix
      lift_matrix <- prob_joint / prob_expected
      lift_matrix[is.nan(lift_matrix) | is.infinite(lift_matrix)] <- NA
      diag(lift_matrix) <- NA

      # Set clean names
      colnames(lift_matrix) <- category_names
      rownames(lift_matrix) <- category_names
      colnames(cooccur_matrix) <- category_names
      rownames(cooccur_matrix) <- category_names
      names(prob_marginal) <- category_names

      # Save lift matrix as CSV
      lift_df <- as.data.frame(lift_matrix)
      lift_df <- cbind(category = rownames(lift_df), lift_df)
      write_csv(lift_df, glue::glue("output/tabs/csv/1_5_lift_matrix_{country}.csv"))

      # Extract all pairs (upper triangle only)
      upper_idx <- which(upper.tri(lift_matrix), arr.ind = TRUE)

      top_pairs <- data.frame(
        country = country,
        category_A = category_names[upper_idx[, 1]],
        category_B = category_names[upper_idx[, 2]],
        lift = lift_matrix[upper_idx],
        support = prob_joint[upper_idx],
        cooccurrence = cooccur_matrix[upper_idx],
        P_A = prob_marginal[upper_idx[, 1]],
        P_B = prob_marginal[upper_idx[, 2]]
      ) %>%
        filter(!is.na(lift) & cooccurrence >= 5) %>%
        arrange(desc(lift))

      all_top_pairs[[country]] <<- top_pairs

      # Create heatmap visualization
      lift_long <- as.data.frame(lift_matrix) %>%
        mutate(category_A = rownames(lift_matrix)) %>%
        pivot_longer(
          cols = -category_A,
          names_to = "category_B",
          values_to = "lift"
        ) %>%
        filter(!is.na(lift)) %>%
        mutate(
          category_A = factor(category_A, levels = category_names),
          category_B = factor(category_B, levels = category_names)
        )

      p <- ggplot(lift_long, aes(x = category_A, y = category_B, fill = lift)) +
        geom_tile(color = "white", linewidth = 0.5) +
        geom_text(aes(label = round(lift, 2)), size = 3, color = "black") +
        scale_fill_gradient2(
          low = "#2166AC",
          mid = "white",
          high = "#B2182B",
          midpoint = 1,
          name = "Lift",
          limits = c(0, max(lift_long$lift, na.rm = TRUE)),
          oob = scales::squish
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 9),
          axis.title = element_blank(),
          legend.position = "right",
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          panel.grid = element_blank()
        ) +
        labs(
          title = glue::glue("Lift Analysis: Problem Category Co-occurrence - {country}"),
          subtitle = "Lift > 1: positive association | Lift = 1: independence | Lift < 1: negative association"
        ) +
        coord_fixed()

      ggsave(
        glue::glue("output/tabs/png/1_5_lift_heatmap_{country}.png"),
        plot = p,
        width = 10,
        height = 8,
        dpi = 150
      )

    }
  )

  # Combine all pairs and save
  results_tbl <- bind_rows(all_top_pairs) %>%
    select(country, category_A, category_B, lift, support, cooccurrence, P_A, P_B) %>%
    arrange(country, desc(lift))

  write_csv(results_tbl, "output/tabs/csv/1_5_lift_top_pairs.csv")

  # Create summary table
  suppressMessages(
    kableExtra::kbl(
      results_tbl %>%
        mutate(
          lift = round(lift, 2),
          support = round(support, 4),
          P_A = round(P_A, 3),
          P_B = round(P_B, 3)
        ),
      digits = 2,
      caption = "Problem Category Pairs by Lift (Strongest Associations)",
      col.names = c("Country", "Category A", "Category B",
                    "Lift", "Support", "Co-occ.", "P(A)", "P(B)")
    ) %>%
      kableExtra::kable_classic(
        html_font = "Cambria"
      ) %>%
      kableExtra::kable_styling(
        font_size = 14,
        latex_options = "striped"
      ) %>%
      kableExtra::pack_rows(
        index = table(results_tbl$country)[unique(results_tbl$country)]
      ) %>%
      safe_save_kable("output/tabs/png/1_5_lift_top_pairs.png")
  )

  return(results_tbl)
}
