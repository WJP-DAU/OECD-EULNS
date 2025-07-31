## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Utilities module
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     July 23, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Regional Average Generator ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_nuts_avg <- function(
     data,
     target_col,
     country_col = "country_name_ltn",
     nuts_col = "nuts_id",
     transform_as = NULL,
     disaggregate_by = NULL
){
  
  df <- data %>%
    rename(
      target = all_of(target_col),
      nuts_id = all_of(nuts_col),
      country_name_ltn = all_of(country_col)
    )
  
  if (!is.null(transform_as)){
    df <- df %>%
      mutate(
        across(
          target,
          transform_as
        )
      )
  }
    
  if (is.null(disaggregate_by)){
    grouped_df <- df %>%
      group_by(country_name_ltn, nuts_id)
      
  } else {
    grouped_df <- df %>%
      rename(
        group = all_of(disaggregate_by)
      ) %>%
      filter(
        !is.na(group)
      ) %>%
      group_by(country_name_ltn, nuts_id, group)
    
  }
  
  results <- grouped_df %>%
    summarise(
      proportion = mean(target, na.rm = TRUE),
      count = sum(!is.na(target), na.rm = TRUE),
      .groups = "keep"
    ) %>%
    mutate(
      proportion = if_else(count < 30, NA_real_, proportion)
    ) %>%  
    select(-count)
  
  return(results)
}

  
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  National Weighted Average Generator ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_wgt_national_avg <- function(
    results_by_region, 
    regions, 
    target_col, 
    country_col = "country_name_ltn",
    group_col = NULL,
    nuts_col = "nuts_id",
    wcol = "regionpoppct"
){
  
  df <- results_by_region %>%
    rename(
      country_name_ltn = all_of(country_col),
      proportion = all_of(target_col),
      nuts_id = all_of(nuts_col)
    ) %>%
    left_join(
      regions %>% select(nuts_id, weight = all_of(wcol)),
      by = "nuts_id"
    ) %>%
    mutate(
      weight = if_else(
        is.na(proportion), NA_real_, weight
      )
    )
  
  if (is.null(group_col)){
    grouped_df <- df %>%
      group_by(
        country_name_ltn
      ) %>% 
      mutate(
        wsum = sum(weight, na.rm = TRUE),
        reweight = weight/wsum
      ) %>% 
      mutate(
        weighted_proportion = proportion*reweight
      ) %>% 
      group_by(country_name_ltn)
    
  } else {
    grouped_df <- df %>%
      rename(
        category = all_of(group_col),
      ) %>%
      group_by(
        country_name_ltn, category
      ) %>% 
      mutate(
        wsum = sum(weight, na.rm = TRUE),
        reweight = weight/wsum
      ) %>%
      mutate(
        weighted_proportion = proportion*reweight
      ) %>% 
      group_by(country_name_ltn, category)
    
  }
  
  results <- grouped_df %>%
    summarise(
      proportion = if (all(is.na(weighted_proportion))) NA_real_ else sum(weighted_proportion, na.rm = TRUE),
      .groups = "keep"
    ) %>%
    ungroup()
  
  return(results)
  
}

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Logit Estimation ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

estimate_logit <- function(data, f){
  
  fitted_model <- glm(
    as.formula(f),
    data = data,
    family  = binomial(link = "logit")
  )
  
  mgeffects <- marginaleffects::avg_slopes(
    fitted_model
  )
  
  return(mgeffects)
  
}

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4. Logit Regressors ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

study_countries <- c("Italy", "Malta")
names(study_countries) <- study_countries

independent_vars <- list(
  "Italy" = c(
    "gender", "financial", "residence",
    "age_group", "edu_level", "marital_status", 
    "nationality", "emp_status", "selected_problem_category", "nproblems", 
    "nuts_id"
  ),
  "Malta" = c(
    "gender", "financial",
    "age_group", "edu_level", "marital_status", 
    "emp_status", "nproblems"
  )
)


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5. Typical Results Table ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_results_table <- function(
    master, regions, target, 
    ctype = "full", 
    transform_var = NULL,
    prop = TRUE
  ){
  
  if (ctype == "full"){
    categories <- c(
      "national", "gender", "age_group", "financial", "residence", 
      "selected_problem_category", "cooccurence_group"
    )
    names(categories) <- c(
      "National", "Gender", "Age Group", "Fin. Situation", "Area of Res.", 
      "Category", "Co-occurence Group"
    )
  } 
  
  if (ctype == "short"){
    categories <- c(
      "national", "gender", "age_group", "financial", "residence"
    )
    names(categories) <- c(
      "National", "Gender", "Age Group", "Fin. Situation", "Area of Res."
    )
  }
  
  if (ctype == "trust"){
    categories <- c(
      "national", "gender", "age_group", "financial", "residence", 
      "problem_status", "non_trivial_problem_broader"
    )
    names(categories) <- c(
      "National", "Gender", "Age Group", "Fin. Situation", "Area of Res.", 
      "Problem Status", "Non-Trivial Problem Reported"
    )
  }
  
  if (ctype == "custom_1"){
    categories <- c(
      "national", "gender", "age_group", "financial", "residence", "problem_status", "resolution_favor"
    )
    names(categories) <- c(
      "National", "Gender", "Age Group", "Fin. Situation", "Area of Res.", "Problem Status", "Resolution"
    )
  }
  
  if(!is.null(transform_var)){
    tranforming_fct = transform_var
  } else {
    tranforming_fct = NULL
  }
  
  results_tbl <- imap(
    categories,
    function(category, category_name){
      
      if (category == "national"){
        results_by_region <- get_nuts_avg(
          master,
          target,
          transform_as = tranforming_fct
        )
        
        results_by_country <- get_wgt_national_avg(
          results_by_region, 
          regions, 
          target_col  = "proportion"
        ) %>%
          mutate(
            category = "National",
            grouping = "National"
          )
        
      } else {
        results_by_region <- get_nuts_avg(
          master,
          target,
          disaggregate_by = category,
          transform_as = tranforming_fct
        )
        
        results_by_country <- get_wgt_national_avg(
          results_by_region, 
          regions, 
          target_col  = "proportion",
          group_col   = "group"
        ) %>%
          mutate(
            grouping = category_name
          )
        
      }
      
    }
  ) %>% 
    reduce(
      bind_rows
    ) %>%
    mutate(
      proportion = proportion*100
    ) %>%
    relocate(grouping, category) %>%
    pivot_wider(
      id_cols = c(grouping, category),
      names_from = country_name_ltn,
      values_from = proportion
    )
  
  if (!prop){
    results_tbl <- results_tbl %>%
      mutate(
        across(
          c("Italy", "Malta"),
          \(x) x/100
        )
      )
  }
  
  return(results_tbl)
}


export_results_kable <- function(
    results_tbl, title, file, ctype = "full", country = NULL
){
  
  if (ctype == "full"){
    suppressMessages(
      kableExtra::kbl(
        results_tbl %>% select(-grouping), 
        digits = 1, 
        caption = title,
        col.names = c("Category", "Italy", "Malta")
      ) %>%
        kableExtra::pack_rows("National", 1, 1) %>%
        kableExtra::pack_rows("Gender", 2, 3) %>%
        kableExtra::pack_rows("Age Group", 4, 9) %>%
        kableExtra::pack_rows("Financial Situation", 10, 11) %>%
        kableExtra::pack_rows("Area of Residence", 12, 13) %>%
        kableExtra::pack_rows("Category", 14, 25) %>%
        kableExtra::pack_rows("Co-occurence", 26, 29) %>%
        kableExtra::kable_classic(
          # full_width = F, 
          html_font = "Cambria"
        ) %>% 
        kableExtra::kable_styling(
          font_size = 18,
          latex_options = "striped"
        ) %>%
        kableExtra::save_kable(file)
    )
  }
  
  if (ctype == "short"){
    suppressMessages(
      kableExtra::kbl(
        results_tbl %>% select(-grouping), 
        digits = 1, 
        caption = title,
        col.names = c("Category", "Italy", "Malta")
      ) %>%
        kableExtra::pack_rows("National", 1, 1) %>%
        kableExtra::pack_rows("Gender", 2, 3) %>%
        kableExtra::pack_rows("Age Group", 4, 9) %>%
        kableExtra::pack_rows("Financial Situation", 10, 11) %>%
        kableExtra::pack_rows("Area of Residence", 12, 13) %>%
        kableExtra::kable_classic(
          # full_width = F, 
          html_font = "Cambria"
        ) %>% 
        kableExtra::kable_styling(
          font_size = 18,
          latex_options = "striped"
        ) %>%
        kableExtra::save_kable(file)
    )
  }
  
  if (ctype == "trust"){
    suppressMessages(
      kableExtra::kbl(
        results_tbl %>% select(-grouping), 
        digits = 1, 
        caption = title,
        col.names = c("Category", "Italy", "Malta")
      ) %>%
        kableExtra::pack_rows("National", 1, 1) %>%
        kableExtra::pack_rows("Gender", 2, 3) %>%
        kableExtra::pack_rows("Age Group", 4, 9) %>%
        kableExtra::pack_rows("Financial Situation", 10, 11) %>%
        kableExtra::pack_rows("Area of Residence", 12, 13) %>%
        kableExtra::pack_rows("Problem Status", 14, 15) %>%
        kableExtra::pack_rows("Non-Trivial Problem Reported", 16, 17) %>%
        kableExtra::kable_classic(
          # full_width = F, 
          html_font = "Cambria"
        ) %>% 
        kableExtra::kable_styling(
          font_size = 18,
          latex_options = "striped"
        ) %>%
        kableExtra::save_kable(file)
    )
  }
  
  if (ctype == "custom_1"){
    suppressMessages(
      kableExtra::kbl(
        results_tbl %>% select(-grouping), 
        digits = 1, 
        caption = title,
        col.names = c("Category", "Italy", "Malta")
      ) %>%
        kableExtra::pack_rows("National", 1, 1) %>%
        kableExtra::pack_rows("Gender", 2, 3) %>%
        kableExtra::pack_rows("Age Group", 4, 9) %>%
        kableExtra::pack_rows("Financial Situation", 10, 11) %>%
        kableExtra::pack_rows("Area of Residence", 12, 13) %>%
        kableExtra::pack_rows("Problem Status", 14, 15) %>%
        kableExtra::pack_rows("Resolution", 16, 17) %>%
        kableExtra::kable_classic(
          # full_width = F, 
          html_font = "Cambria"
        ) %>% 
        kableExtra::kable_styling(
          font_size = 18,
          latex_options = "striped"
        ) %>%
        kableExtra::save_kable(file)
    )
  }
  
  if (ctype == "logit"){
    
    results_tbl <- results_tbl %>%
      mutate(
        term = factor(
          term, levels = c(
            "gender", "edu_level", "financial", "marital_status", "nationality", 
            "emp_status", "age_group", "residence", "nuts_id", "nproblems", 
            "selected_problem_category"
          )
        )
      ) %>% 
      arrange(
        term
      )
    
    kable <- kableExtra::kbl(
      results_tbl %>% 
        mutate(
          contrast = if_else(
            term == "nproblems",
            "Co-occurrent Problems (dY/dX)",
            contrast
          )
        ) %>% 
        select(-c(term, statistic, s.value)), 
      digits = 3, 
      align = "c",
      caption = title,
      col.names = c("Sociodemographic", "Estimate", "Std. Error", "p.value", "CI low", "CI high")
    )
    
    if (country == "Italy") {
      
      kable <- kable %>%
        kableExtra::pack_rows("Sociodemographics", 1, 6) %>%
        kableExtra::pack_rows("Age Group", 7, 11) %>%
        kableExtra::pack_rows("Geographic Location", 12, 16) %>% 
        kableExtra::pack_rows("Co-Occurrence & Problem Category", 17, 28)
      
    } 
    
    if (country == "Malta") {
      
      kable <- kable %>%
        kableExtra::pack_rows("Sociodemographics", 1, 5) %>%
        kableExtra::pack_rows("Age Group", 6, 10) %>%
        kableExtra::pack_rows("Co-Occurrence", 11, 11)
      
    }
    
    suppressMessages(
      kable <- kable %>%
        kableExtra::kable_classic(
          # full_width = F, 
          html_font = "Cambria"
        ) %>% 
        kableExtra::kable_styling(
          font_size = 18,
          latex_options = "striped"
        ) %>%
        kableExtra::save_kable(
          glue::glue(file)
        )
    )
    
  }
  
}
