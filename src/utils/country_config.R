## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Country configuration
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     June 2026
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## To add a new country:
##   1. Add a new entry to country_config below.
##   2. Add its NUTS regions to data/regions.csv.
##   3. That's it — no other file needs to change.
##
## Fields per country:
##   small_sample    : TRUE skips Category/Age Group breakdowns in charts (like Malta).
##   in_upset_plot   : TRUE includes the country in the co-occurrence upset plot.
##   ylim_upset      : Y-axis upper limit for the justice gap upset plot. Tune after first run.
##   logit_vars      : Independent variables for the access/justice gap logit models.
##   logit_packrows  : Named list of row-grouping labels → c(start, end) for the logit kable.
##                     Row counts depend on logit_vars: binary vars = 1 row each,
##                     age_group = 5 rows, nuts_id = (n_regions - 1) rows,
##                     selected_problem_category = 11 rows.
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

country_config <- list(

  "Italy" = list(
    small_sample  = FALSE,
    in_upset_plot = TRUE,
    ylim_upset    = 20.5,
    logit_vars    = c(
      "gender", "financial", "residence",
      "age_group", "edu_level", "marital_status",
      "nationality", "emp_status", "selected_problem_category", "nproblems",
      "nuts_id"
    ),
    logit_packrows = list(
      "Sociodemographics"                = c(1, 6),   # gender, edu, financial, marital, nationality, emp
      "Age Group"                        = c(7, 11),  # 5 age contrasts
      "Geographic Location"              = c(12, 16), # residence + 4 nuts contrasts (5 regions)
      "Co-Occurrence & Problem Category" = c(17, 28)  # nproblems + 11 category contrasts
    )
  ),

  "Malta" = list(
    small_sample  = TRUE,
    in_upset_plot = FALSE,
    ylim_upset    = 18.5,
    logit_vars    = c(
      "gender", "financial",
      "age_group", "edu_level", "marital_status",
      "emp_status", "nproblems"
    ),
    logit_packrows = list(
      "Sociodemographics" = c(1, 5),   # gender, edu, financial, marital, emp (no nationality)
      "Age Group"         = c(6, 10),  # 5 age contrasts
      "Co-Occurrence"     = c(11, 11)  # nproblems only (no nuts, no category)
    )
  ),

  "Poland" = list(
    small_sample  = FALSE,
    in_upset_plot = TRUE,
    ylim_upset    = 20.5,
    logit_vars    = c(
      "gender", "financial", "residence",
      "age_group", "edu_level", "marital_status",
      "nationality", "emp_status", "selected_problem_category", "nproblems",
      "nuts_id"
    ),
    logit_packrows = list(
      "Sociodemographics"                = c(1, 6),   # gender, edu, financial, marital, nationality, emp
      "Age Group"                        = c(7, 11),  # 5 age contrasts
      "Geographic Location"              = c(12, 18), # residence + 6 nuts contrasts (7 regions)
      "Co-Occurrence & Problem Category" = c(19, 30)  # nproblems + 11 category contrasts
    )
  ),

  "Hungary" = list(
    small_sample  = FALSE,
    in_upset_plot = TRUE,
    ylim_upset    = 20.5,
    logit_vars    = c(
      "gender", "financial", "residence",
      "age_group", "edu_level", "marital_status",
      "nationality", "emp_status", "selected_problem_category", "nproblems",
      "nuts_id"
    ),
    logit_packrows = list(
      "Sociodemographics"                = c(1, 6),   # gender, edu, financial, marital, nationality, emp
      "Age Group"                        = c(7, 11),  # 5 age contrasts
      "Geographic Location"              = c(12, 14), # residence + 2 nuts contrasts (3 regions)
      "Co-Occurrence & Problem Category" = c(15, 25)  # nproblems + 10 category contrasts (Law Enforcement absent)
    )
  )

)
