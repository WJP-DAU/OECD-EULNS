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
  
  total_sample_df <- master %>%
    group_by(country_name_ltn, nuts_id) %>%
    summarise(
      total_sample = n(),
      .groups = "keep"
    )
  
  base_df <- master %>%
    select(
      country_year_id, country_name_ltn, nuts_id,
      ends_with(c("_bin", "_sev"))
    ) %>%
    select(
      -starts_with("AJR")
    ) %>%
    pivot_longer(
      !c(country_year_id, country_name_ltn, nuts_id),
      names_to = c("problem", "variable"),
      names_pattern = "AJP_(.*)_(.*)",
      values_to = "value"
    ) %>%
    pivot_wider(
      id_cols = c(country_year_id, country_name_ltn, nuts_id, problem),
      names_from = variable,
      values_from = value
    ) %>%
    mutate(
      bin = case_when(
        bin == 1 ~ 1,
        bin == 2 ~ 0
      ),
      severity_group = case_when(
        sev %in% c(0,1,2,3)  ~ "Low",
        sev %in% c(4,5,6)    ~ "Medium",
        sev %in% c(7,8,9,10) ~ "High",
        sev %in% c(98,99)    ~ "Unknown",
        is.na(sev) ~ "Unknown"
      )
    )
  
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
          n = 10
        ) %>%
        mutate(
          proportion = proportion*100
        )
      
      country_table <- bind_cols(
        as.data.frame(
          list(
            "level" = rep(level, 10),
            "rank"  = seq(1,10,1) 
          )
        ),
        results_by_country %>% ungroup() %>%
          filter(country_name_ltn == "Italy") %>%
          select(-country_name_ltn, problem_italy=category, proportion_italy = proportion),
        results_by_country %>% ungroup() %>%
          filter(country_name_ltn == "Malta") %>%
          select(-country_name_ltn, problem_malta=category, proportion_malta = proportion)
      )
      
      return(country_table)

    }
  ) %>%
    reduce(bind_rows)
  
  write_csv(results_tbl, "output/tabs/csv/1_1_freq_probs_by_severity.csv")
  
  suppressMessages(
    kableExtra::kbl(
      results_tbl %>% select(-level), 
      digits = 1, 
      align = "c",
      caption = "Most Frequent Problems By Severity Level",
      col.names = c("Rank", "Problem", "%", "Problem", "%")
    ) %>%
      kableExtra::add_header_above(
        c(" " = 1, "Italy" = 2, "Malta" = 2)
      ) %>%
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
      kableExtra::save_kable("output/tabs/png/1_1_freq_probs_by_severity.png")
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
  
  # convert severity to 0/1
  master_sev <- master %>%
    mutate(
      across(
        all_of(legprob_sev),
        ~ ifelse(. > 3 & . < 98, 1,
                 ifelse(. < 4, 0, NA_real_))
      )
    )
  
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
    filter(country_name_ltn != "Ireland") %>%
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
      col.names = c("Category", "Italy", "Malta")
    ) %>%
      kableExtra::kable_classic(
        # full_width = F, 
        html_font = "Cambria"
      ) %>% 
      kableExtra::kable_styling(
        font_size = 18,
        latex_options = "striped"
      ) %>%
      kableExtra::save_kable("output/tabs/png/1_3_prevalence_nontrivial_problems_by_category.png")
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
      
      master <- master %>% filter(country_name_ltn == country)
      
      legalProblems <- c(
        "F1", "F2", "J1", "J2", "J3", "E3", "C3", "A1", "A2", 
        "A3", "G1", "G2", "G3", "E1", "E2", "D1", "D2", "D3",
        "D4", "D5", "D6", "C1", "C2", "C4", "B1", "B2", "B3",
        "B4", "I1", "L1", "L2", "K1", "K2", "K3", "H1", "H2",
        "H3", "J4"
      )
      legprob_bin <- paste0("AJP_", legalProblems, "_bin")
      legprob_sev <- paste0("AJP_", legalProblems, "_sev")
      
      # Binary data
      data <- master %>%
        select(
          country_year_id,
          all_of(legprob_bin)
        ) %>%
        mutate(
          across(
            starts_with("AJP_"),
            \(x) case_when(
              x == 1 ~ 1,
              x == 2 ~ 0
            )
          )
        )
      
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
      
      # Total frequencies
      frequencies <- colSums(data[, legprob_bin])
      
      # Co-occurence Matrix
      cooccur_matrix <- t(as.matrix(data[, legprob_bin])) %*% 
        as.matrix(data[, legprob_bin])
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
