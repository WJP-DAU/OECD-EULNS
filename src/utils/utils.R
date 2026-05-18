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

study_countries <- c("Italy", "Malta", "Poland")
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
  ),
  "Poland" = c(
    "gender", "financial", "residence",
    "age_group", "edu_level", "marital_status",
    "nationality", "emp_status", "selected_problem_category", "nproblems",
    "nuts_id"
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
    clabels <- c(
      "National", "Gender", "Age Group", "Financial Situation", "Area of Residence", 
      "Category", "Co-occurrent Problems"
    )
    names(categories) <- clabels
  } 
  
  if (ctype == "short"){
    categories <- c(
      "national", "gender", "age_group", "financial", "residence"
    )
    clabels <- c(
      "National", "Gender", "Age Group", "Financial Situation", "Area of Residence"
    )
    names(categories) <- clabels
  }
  
  if (ctype == "trust"){
    categories <- c(
      "national", "gender", "age_group", "financial", "residence", 
      "problem_status", "non_trivial_problem_broader"
    )
    clabels <- c(
      "National", "Gender", "Age Group", "Financial Situation", "Area of Residence", 
      "Problem Status", "Non-Trivial Problem Experienced"
    )
    names(categories) <- clabels
  }
  
  if (ctype == "custom_1"){
    categories <- c(
      "national", "gender", "age_group", "financial", "residence", 
      "problem_status", "resolution_favor"
    )
    clabels <- c(
      "National", "Gender", "Age Group", "Financial Situation", "Area of Residence", 
      "Problem Status", "Resolution"
    )
    names(categories) <- clabels
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
    ) %>% 
    mutate(
      grouping = factor(
        grouping,
        levels = clabels
      )
    )
  
  if (!prop){
    results_tbl <- results_tbl %>%
      mutate(
        across(
          any_of(study_countries),
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
      safe_save_kable(
        kableExtra::kbl(
        results_tbl %>% select(-grouping), 
        digits = 1, 
        caption = title,
        col.names = c("Category", study_countries)
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
        ),
        file
      )
    )
  }
  
  if (ctype == "short"){
    suppressMessages(
      safe_save_kable(
        kableExtra::kbl(
        results_tbl %>% select(-grouping), 
        digits = 1, 
        caption = title,
        col.names = c("Category", study_countries)
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
        ),
        file
      )
    )
  }
  
  if (ctype == "trust"){
    suppressMessages(
      safe_save_kable(
        kableExtra::kbl(
        results_tbl %>% select(-grouping), 
        digits = 1, 
        caption = title,
        col.names = c("Category", study_countries)
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
        ),
        file
      )
    )
  }
  
  if (ctype == "custom_1"){
    suppressMessages(
      safe_save_kable(
        kableExtra::kbl(
        results_tbl %>% select(-grouping), 
        digits = 1, 
        caption = title,
        col.names = c("Category", study_countries)
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
        ),
        file
      )
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

    if (country == "Poland") {

      kable <- kable %>%
        kableExtra::pack_rows("Sociodemographics", 1, 6) %>%
        kableExtra::pack_rows("Age Group", 7, 11) %>%
        kableExtra::pack_rows("Geographic Location", 12, 18) %>%
        kableExtra::pack_rows("Co-Occurrence & Problem Category", 19, 30)

    }
    
    suppressMessages(
      safe_save_kable(
        kable %>%
        kableExtra::kable_classic(
          # full_width = F, 
          html_font = "Cambria"
        ) %>% 
        kableExtra::kable_styling(
          font_size = 18,
          latex_options = "striped"
        ),
        glue::glue(file)
      )
    )
    
  }
  
}


safe_save_kable <- function(kable_obj, file) {
  ext <- tolower(tools::file_ext(file))

  if (ext %in% c("png", "jpg", "jpeg", "pdf") && !chromote_is_available()) {
    warning(
      glue::glue(
        "Skipping kable export to '{file}': Chrome / Chromote is not available in this environment."
      ),
      call. = FALSE
    )
    return(invisible(kable_obj))
  }

  tryCatch(
    kableExtra::save_kable(kable_obj, file),
    error = function(e) {
      if (file.exists(file)) {
        file_info <- file.info(file)
        if (!is.na(file_info$size) && file_info$size == 0) {
          unlink(file)
        }
      }
      warning(
        glue::glue(
          "Skipping kable export to '{file}': {conditionMessage(e)}"
        ),
        call. = FALSE
      )
      invisible(kable_obj)
    }
  )
}


chromote_is_available <- function() {
  cached <- getOption("oecd_eulns.chromote_available")
  if (!is.null(cached)) {
    return(isTRUE(cached))
  }

  available <- FALSE

  if (requireNamespace("chromote", quietly = TRUE)) {
    info <- tryCatch(
      chromote::chromote_info(),
      error = function(e) NULL
    )

    available <- is.list(info) &&
      is.list(info$.check) &&
      identical(info$.check$status, 0L)
  }

  options(oecd_eulns.chromote_available = available)
  available
}
