## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Functions for constructing special variables
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     July 22, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Special Demographics ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

add_special_demographics <- function(data){
  
  data %>%
    mutate(
      gender = case_when(
        gend == 1 ~ "Male",
        gend == 2 ~ "Female"
      ),
      gender = factor(gender, levels = c("Female", "Male")),
      
      financial = case_when(
        fin %in% c(1,2) ~ "Constrained",
        fin %in% c(3,4,5,98) ~ "Unconstrained"
      ),
      financial = factor(financial, levels = c("Constrained", "Unconstrained")),
      
      residence = case_when(
        urban == 1 ~ "Urban",
        urban == 2 ~ "Rural"
      ),
      residence = factor(residence, levels = c("Rural", "Urban")),
      
      age_group = case_when(
        age == 1 | (age >= 18 & age <= 24)  ~ "18-24",
        age == 2 | (age >= 25 & age <= 34)  ~ "25-34",
        age == 3 | (age >= 35 & age <= 44)  ~ "35-44",
        age == 4 | (age >= 45 & age <= 54)  ~ "45-54",
        age == 5 | (age >= 55 & age <= 64)  ~ "55-64",
        age == 6 | (age >= 65 & age <= 100) ~ "65-100"
      ),
      # age_group = factor(age_group, levels = c(...)),
      
      edu_level = case_when(
        edu %in% c(5,6) ~ "Higher Education",
        edu %in% c(1,2,3,4,7,98) ~ "No Higher Education"
      ),
      edu_level = factor(edu_level, levels = c("No Higher Education", "Higher Education")),
      
      marital_status = case_when(
        marital %in% c(2,3) ~ "Married",
        marital %in% c(1,4,5,98) ~ "Not married"
      ),
      marital_status = factor(marital_status, levels = c("Married", "Not married")),
      
      nationality = case_when(
        nation == 1 ~ "National",
        nation == 2 ~ "Foreigner",
      ),
      nationality = factor(nationality, levels = c("Foreigner", "National")),
      
      emp_status = case_when(
        emp %in% c(1,2) ~ "Employed",
        emp %in% c(3,4,5,6,7,8,98) ~ "Not employed",
      ),
      emp_status = factor(emp_status, levels = c("Not employed", "Employed")),
    ) %>%
    select(
      country_year_id,
      gender, financial, residence, age_group, edu_level, marital_status, nationality, emp_status
    )
  
}

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Special A2J variables ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

add_a2j_vars <- function(data){

  ### Legal Problem prevalence ----
  legalProblems <- c(
    "A1", "A2", "A3", "B1", "B2", "B3", "B4", "C1", "C2", "C3", "C4", "D1", "D2", "D3", "D4", "D5", "D6", "E1", 
    "E2", "E3", "F1", "F2", "G1", "G2", "G3", "H1", "H2", "H3", "I1", "J1", "J2", "J3", "J4", "K1", "K2", "K3", 
    "L1", "L2"
  )
  legprob_bin <- paste0("AJP_", legalProblems, "_bin")
  legprob_sev <- paste0("AJP_", legalProblems, "_sev")
  
  # Extracting severity of problem selected
  selec_sev <- data %>%
    pivot_longer(
      !c(country_year_id, AJP_problem), 
      names_to      = c("set", ".value"), 
      names_pattern = "AJP_(.*)_(.*)"
    ) %>%
    mutate(
      sev = if_else(AJP_problem == set, sev, NA_real_)
    ) %>%
    group_by(country_year_id) %>%
    summarise(
      AJP_problem = first(AJP_problem),
      sev_problem_selected = sum(sev, na.rm = T)
    ) %>%
    mutate(
      sev_problem_selected = if_else(
        AJP_problem == "", 
        NA_real_, 
        sev_problem_selected 
      )
    ) %>%
    select(-AJP_problem)
  
  # Estimating problem prevalence
  probPrev <- reduce( 
    list(
      
      # Data 1: incidence
      data %>%
        select(country_year_id, all_of(legprob_bin)) %>%
        pivot_longer(
          !country_year_id,
          names_to  = "problem",
          values_to = "answer"
        ) %>%
        mutate(
          problem = str_remove_all(problem, "AJP|_|bin")
        ),
      
      # Data 2: severity
      data %>%
        select(country_year_id, all_of(legprob_sev)) %>%
        pivot_longer(
          !country_year_id,
          names_to  = "problem",
          values_to = "severity"
        ) %>%
        mutate(
          problem = str_remove_all(problem, "AJP|_|sev")
        )
    ),
    left_join,
    by = c("country_year_id", "problem")
  ) %>%
    mutate(
      prevalence1 = case_when(
        answer == 1 ~ 1,
        answer == 2 ~ 0
      ),
      prevalence2 = case_when(
        answer == 1 & severity >= 98 ~ NA_real_, # we don't know if the problem was non-trivial or not
        answer == 1 & severity >= 4  ~ 1,
        answer == 1 & severity  < 4  ~ 0,
        answer == 2 ~ 0
      )
    ) %>%
    group_by(country_year_id) %>%
    summarise(
      across(
        starts_with("prevalence"),
        \(x) sum(x, na.rm = T)
      )
    ) %>%
    mutate(
      nproblems = prevalence2,
      cooccurence_group = case_when(
        nproblems == 0 ~ NA_character_,
        nproblems == 1 ~ "1 problem",
        nproblems <= 3 ~ "2-3 problems",
        nproblems <= 5 ~ "4-5 problems",
        nproblems >= 6 ~ "5 or more problems"
      ),
      across(
        starts_with("prevalence"),
        \(x) if_else(x > 0, 1, 0)
      )
    )
  
  ### A2J special wranglings ----
  legal_needs <- data %>%
    left_join(
      selec_sev,
      by = "country_year_id"
    ) %>%
    mutate(
      
      # Triviality of problem
      non_trivial_problem = case_when(
        is.na(sev_problem_selected) ~ NA_real_,
        sev_problem_selected <= 3  ~ 0,
        sev_problem_selected <= 10 ~ 1,
        sev_problem_selected <= 99 ~ 0
      ),
      
      # Legal vulnerability: official proof of identity
      vulnerability1 = case_when(
        A5_1 == 1  | A5_2 == 1  ~ 1,
        A5_1 == 99 & A5_2 == 99 ~ NA_real_,
        A5_1 >= 2  | A5_2 >= 2  ~ 0,
      ),
      
      # Legal vulnerability: official proof of housing or land tenure
      vulnerability2 = case_when(
        A5b  == 1 ~ 1,
        A5b <= 98 ~ 0,
      ),
      
      # Legal vulnerability: written agreement
      vulnerability3 = case_when(
        wagreement == 1  ~ 1,
        wagreement <= 98 ~ 0,
        work == 1 ~ 1
        # work == 8 ~ 0
      ),
      
      # Access to appropriate information and advice
      access2info = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJE_infosource <= 2        ~ 1,
        AJE_infosource <= 98       ~ 0
      ),
      
      # Access to appropriate assistance and representation
      access2rep = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJD_inst_advice == 1 & (
          AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 | 
            AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 |
            AJD_adviser_8 == 1
        ) ~ 1,
        AJD_inst_advice == 1 & AJD_adviser_1 == 1 & AJD_expert_adviser == 1 ~ 1, # Friend/Family with legal background
        AJD_inst_advice == 1 & (
          AJD_adviser_1 == 1 | AJD_adviser_9 == 1 | AJD_adviser_98 == 1
        ) ~ 0,
        AJD_inst_advice == 2 & (AJD_noadvice_reason %in% c(1,2,3)) ~ 1,
        AJD_inst_advice == 2 & (AJD_noadvice_reason %in% c(4,5,6,7,8,9,10,98)) ~ 0,
        AJD_inst_advice == 98 ~ 0
      ),
      
      # Access to a dispute resolution mechanism
      access2drm = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_resolution == 1        ~ 1,
        AJR_resolution == 2 & (AJR_noresol_reason %in% c(3,5,6,7,8)) ~ 0
        # AJR_resolution == 98 (We don't know if they really needed the DRM, so we exclude 98s)
      ),
      
      # Court mentioned as drm
      court_as_DRM = case_when(
        AJR_court_bin == 1 ~ "Court or Tribunal",
        AJR_court_bin == 0 ~ "Other"
      ),
      
      # Timeliness of the resolution process
      rp_time = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_noresol  %in% c(1,2,98,99) ~ NA_real_,
        AJR_solvingtime == -9999    ~ NA_real_,
        AJR_solvingtime == -8888    ~ 0,
        AJR_solvingtime >  12       ~ 0,
        AJR_solvingtime <= 12       ~ 1
      ),
      
      # Quickness of the resolution process
      # rp_quick = case_when(
      #   is.na(non_trivial_problem) ~ NA_real_,
      #   non_trivial_problem == 0   ~ NA_real_,
      #   AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
      #   AJR_state_noresol  %in% c(1,2,98,99) ~ NA_real_,
      #   AJR_slow == 99 ~ NA_real_,
      #   AJR_slow == 98 ~ 0,
      #   AJR_slow == 1  ~ 0,
      #   AJR_slow == 2  ~ 1
      # ),
      rp_quick = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_noresol  %in% c(1,2,98,99) ~ NA_real_,
        AJR_slow == 99 ~ NA_real_,
        AJR_slow == 98 ~ 0,
        AJR_slow == 1  ~ 1,
        AJR_slow == 2  ~ 0
      ),
      
      # Costliness of the resolution process
      rp_cost = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_noresol  %in% c(1,2,98,99) ~ NA_real_,
        AJR_solvingcosts == 2 ~ 1,
        AJR_solvingcosts == 1 & (AJR_costdiff %in% c(1,2)) ~ 1,
        AJR_solvingcosts == 1 & (AJR_costdiff %in% c(3,4,98)) ~ 0
      ),
      
      # Resolution process was NOT expensive
      rp_expensive = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_noresol  %in% c(1,2,98,99) ~ NA_real_,
        AJR_expensive == 1 ~ 1,
        AJR_expensive %in% c(2,98) ~ 0,
      ),
      
      # Fairness of the resolution process
      rp_fair = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_noresol  %in% c(1,2,98,99) ~ NA_real_,
        AJR_fair == 1         ~ 1,
        AJR_fair %in% c(2,98) ~ 0
      ),
      
      # Outcome of the resolution process
      rp_outcome = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_state_resol   %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_noresol %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_resol    == 3     ~ 0,
        AJR_state_resol    == 4     ~ 1,
        AJR_state_noresol  == 3     ~ 0,
        AJR_state_noresol  == 4     ~ 1
      ),
      
      # Problem Status
      problem_status = case_when(
        AJR_state_resol   %in% c(1,2) ~ "Ongoing",
        AJR_state_noresol %in% c(1,2) ~ "Ongoing",
        AJR_state_resol   %in% c(3,4) ~ "Done",
        AJR_state_noresol %in% c(3,4) ~ "Done"
      ),
      
      # Outcome resolution was in favor?
      resolution_favor = case_when(
        AJR_outcome %in% c(1,2) ~ "In favor of respondent",
        AJR_outcome %in% c(3,98) ~ "Not in favor / Unknown",
      ),
      
      # Satisfaction with the process
      rp_satisfaction = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_state_resol   %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_noresol %in% c(1,2,98,99) ~ NA_real_,
        AJR_satis_outcome %in% c(1,2) ~ 1,
        # AJR_satis_ongoing %in% c(1,2) ~ 1,
        AJR_satis_outcome %in% c(3,4,98) ~ 0,
        # AJR_satis_ongoing %in% c(3,4,98) ~ 0,
      ),
      
      # Problem Category
      selected_problem_category = case_when(
        AJP_problem %in% c("A1", "A2", "A3") ~ "Consumer",
        AJP_problem %in% c("B1", "B2", "B3", "B4") ~ "Land",
        AJP_problem %in% c("C1", "C2", "C4") ~ "Housing",
        AJP_problem %in% c("D1", "D2", "D3", "D4", "D5", "D6") ~ "Family",
        AJP_problem %in% c("E1", "E2") ~ "Education",
        AJP_problem %in% c("F1", "F2") ~ "Accidental",
        AJP_problem %in% c("G1", "G2", "G3") ~ "Employment",
        AJP_problem %in% c("H1", "H2", "H3", "J4") ~ "Public Services",
        AJP_problem %in% c("I1") ~ "Law Enforcement",
        AJP_problem %in% c("J1", "J2", "J3") ~ "Citizenship & ID",
        AJP_problem %in% c("K1", "K2", "K3", "L1", "L2") ~ "Money & Debt",
        AJP_problem %in% c("E3", "C3") ~ "Community"
      ),
      
      # Hardships
      across(
        c(AJE_health, AJE_emotional, AJE_income, AJE_drugs),
        \(x) case_when(
          is.na(non_trivial_problem) ~ NA_real_,
          non_trivial_problem == 0   ~ NA_real_,
          x == 1 ~ 1,
          x %in% c(2,98) ~ 0
        ),
        .names = "hardships_{str_remove(.col, 'AJE_')}"
      )
    
    ) %>% 
    select(
      country_year_id, 
      vulnerability1, vulnerability2, vulnerability3,
      access2info, access2rep, access2drm,
      rp_time, rp_cost, rp_fair, rp_outcome, rp_quick, rp_expensive,
      problem_status, resolution_favor, rp_satisfaction,
      non_trivial_problem, selected_problem_category,
      starts_with("hardships_")
    )
  
  ### Justice Gap wranglings ----
  jg_vars <- c(
    "jg_access2info", "jg_access2rep",
    "jg_time", "jg_cost", "jg_fair", 
    "jg_outcome"
  )
  
  jg_inverse_vars <- c(
    "jg_inverse_access2info", "jg_inverse_access2rep",
    "jg_inverse_time", "jg_inverse_cost", "jg_inverse_fair", 
    "jg_inverse_outcome"
  )
  
  jg_legal_needs <- data %>%
    left_join(
      selec_sev,
      by = "country_year_id"
    ) %>%
    mutate(
      
      # Triviality of problem
      non_trivial_problem = case_when(
        is.na(sev_problem_selected) ~ NA_real_,
        sev_problem_selected <= 3  ~ 0,
        sev_problem_selected <= 10 ~ 1,
        sev_problem_selected <= 99 ~ 0
      ),
      
      # Access to appropriate information and advice
      jg_access2info = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJE_infosource <= 2        ~ 1,
        AJE_infosource <= 4        ~ 0
      ),
      jg_inverse_access2info = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJE_infosource <= 2        ~ 0,
        AJE_infosource <= 4        ~ 1,
        AJE_infosource <= 98       ~ 0
      ),
      
      # Access to appropriate assistance and representation
      jg_access2rep = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJD_inst_advice == 1 & (
          AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 | 
            AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 |
            AJD_adviser_8 == 1
        ) ~ 1,
        AJD_inst_advice == 1 & AJD_adviser_1 == 1 & AJD_expert_adviser == 1 ~ 1,
        AJD_inst_advice == 1 & (
          AJD_adviser_1 == 1 | AJD_adviser_9 == 1
        ) ~ 0,
        AJD_inst_advice == 2 & (AJD_noadvice_reason %in% c(1,2,3)) ~ 1,
        AJD_inst_advice == 2 & (AJD_noadvice_reason %in% c(4,5,6,7,8,9,10)) ~ 0,
        AJD_inst_advice == 98 ~ NA_real_
      ),
      jg_inverse_access2rep = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJD_inst_advice == 1 & (
          AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 | 
            AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 |
            AJD_adviser_8 == 1
        ) ~ 0,
        AJD_inst_advice == 1 & AJD_adviser_1 == 1 & AJD_expert_adviser == 1 ~ 0,
        AJD_inst_advice == 1 & (
          AJD_adviser_1 == 1 | AJD_adviser_9 == 1
        ) ~ 1,
        AJD_inst_advice == 1 & AJD_adviser_98 == 1 ~ 0,
        AJD_inst_advice == 2 & (AJD_noadvice_reason %in% c(1,2,3)) ~ 0,
        AJD_inst_advice == 2 & (AJD_noadvice_reason %in% c(4,5,6,7,8,9,10)) ~ 1,
        AJD_inst_advice == 2 & (AJD_noadvice_reason %in% c(98)) ~ 0,
        AJD_inst_advice == 98 ~ 0
      ),
      
      # Timeliness of the resolution process
      jg_time = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_noresol  %in% c(1,2,98,99) ~ NA_real_,
        AJR_solvingtime == -9999    ~ NA_real_,
        AJR_solvingtime == -8888    ~ NA_real_,
        AJR_solvingtime >  12       ~ 0,
        AJR_solvingtime <= 12       ~ 1
      ),
      jg_inverse_time = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_noresol  %in% c(1,2,98,99) ~ NA_real_,
        AJR_solvingtime == -9999    ~ NA_real_,
        AJR_solvingtime == -8888    ~ 0,
        AJR_solvingtime >  12       ~ 1,
        AJR_solvingtime <= 12       ~ 0
      ),
      
      # Costliness of the resolution process
      jg_cost = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_noresol  %in% c(1,2,98,99) ~ NA_real_,
        AJR_solvingcosts == 2 ~ 1,
        AJR_solvingcosts == 1 & (AJR_costdiff %in% c(1,2)) ~ 1,
        AJR_solvingcosts == 1 & (AJR_costdiff %in% c(3,4)) ~ 0,
        AJR_solvingcosts == 1 & (AJR_costdiff %in% c(98))  ~ NA_real_
      ),
      jg_inverse_cost = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_noresol  %in% c(1,2,98,99) ~ NA_real_,
        AJR_solvingcosts == 2 ~ 0,
        AJR_solvingcosts == 1 & (AJR_costdiff %in% c(1,2)) ~ 0,
        AJR_solvingcosts == 1 & (AJR_costdiff %in% c(3,4)) ~ 1,
        AJR_solvingcosts == 1 & (AJR_costdiff %in% c(98))  ~ 0
      ),
      
      # Fairness of the resolution process
      jg_fair = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_noresol  %in% c(1,2,98,99) ~ NA_real_,
        AJR_fair == 1  ~ 1,
        AJR_fair == 2  ~ 0,
        AJR_fair == 98 ~ NA_real_
      ),
      jg_inverse_fair = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_noresol  %in% c(1,2,98,99) ~ NA_real_,
        AJR_fair == 1  ~ 0,
        AJR_fair == 2  ~ 1,
        AJR_fair == 98 ~ 0
      ),
      
      # Outcome of the resolution process
      jg_outcome = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_state_resol   %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_noresol %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_resol    == 3     ~ 0,
        AJR_state_resol    == 4     ~ 1,
        AJR_state_noresol  == 3     ~ 0,
        AJR_state_noresol  == 4     ~ 1
      ),
      jg_inverse_outcome = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_state_resol   %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_noresol %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_resol    == 3     ~ 1,
        AJR_state_resol    == 4     ~ 0,
        AJR_state_noresol  == 3     ~ 1,
        AJR_state_noresol  == 4     ~ 0
      )
    )
  
  justice_gap_data <- imap(
    list(
      "noDK" = jg_vars,
      "keepDK" = jg_inverse_vars
    ),
    function(var_set, method){
      
      jg_data_comp4th <- jg_legal_needs %>%
        filter(
          non_trivial_problem == 1
        ) %>%
        select(
          country_year_id,
          all_of(var_set)
        ) %>%
        select(
          country_year_id,
          ends_with(c("time", "cost", "fair"))
        ) %>% 
        pivot_longer(
          !country_year_id,
          names_to = "variable",
          values_to = "value"
        ) %>% 
        group_by(country_year_id) %>% 
        summarise(
          jg_resol_process = mean(value, na.rm = TRUE)
        )
      
      jg_data <- left_join(
        jg_legal_needs %>%
          select(
            country_year_id,
            all_of(var_set)
          ),
        jg_data_comp4th,
        by = "country_year_id"
      ) %>% 
        left_join(
          
          jg_legal_needs %>%
            select(
              country_year_id,
              all_of(var_set)
            ) %>% 
            left_join(
              jg_data_comp4th,
              by = "country_year_id"
            ) %>%
            select(
              country_year_id,
              ends_with(c("info", "rep", "process", "outcome"))
            ) %>% 
            pivot_longer(
              !country_year_id,
              names_to = "variable",
              values_to = "value"
            ) %>% 
            group_by(country_year_id) %>% 
            summarise(
              a2j_score = mean(value, na.rm = TRUE)
            ),
          by = "country_year_id"
        )
      
      if(method == "keepDK"){
        jg_data <- jg_data %>%
          rename(
            jg_inverse_resol_process = jg_resol_process,
            a2j_inverse_score = a2j_score
          ) %>%
          mutate(
            a2j_score = if_else(
              is.na(a2j_inverse_score),
              NA_real_,
              1-a2j_inverse_score,
              missing = NA_real_
            )
          )
      }
      
      jg_data <- jg_data %>%
        mutate(
          inside_gap = if_else(
            a2j_score < 0.67,
            1, 0,
            missing = NA_real_
          )
        )
      
      return(jg_data)
    }
  )
  
  ### Special Sankey Wranglings ----
  sankey_data <- data %>%
    left_join(
      selec_sev,
      by = "country_year_id"
    ) %>%
    mutate(
      
      # Triviality of problem
      non_trivial_problem = case_when(
        is.na(sev_problem_selected) ~ NA_real_,
        sev_problem_selected <= 3  ~ 0,
        sev_problem_selected <= 10 ~ 1,
        sev_problem_selected <= 99 ~ 0
      ),
      
      # Advice & Representation: First Stage
      sankey_advice_stage_1 = case_when(
        is.na(non_trivial_problem)    ~ NA_character_,
        non_trivial_problem == 0      ~ NA_character_,
        AJD_inst_advice == 1          ~ "Contacted an advisor",
        AJD_inst_advice %in% c(2, 98) ~ "Did not contact an advisor"
      ),
      
      # Advice & Representation:  Second Stage
      sankey_advice_stage_2 = case_when(
        is.na(non_trivial_problem) ~ NA_character_,
        non_trivial_problem == 0   ~ NA_character_,
        AJD_inst_advice == 1 & (
          AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 | 
            AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 |
            AJD_adviser_8 == 1
        ) ~ "Appropriate Advisor",
        AJD_inst_advice == 1 & AJD_adviser_1 == 1 & AJD_expert_adviser == 1 ~ "Appropriate Advisor",
        AJD_inst_advice == 1 & (
          AJD_adviser_1 == 1 | AJD_adviser_9 == 1 | AJD_adviser_98 == 1
        ) ~ "Non-Appropriate Advisor",
        AJD_inst_advice == 2 & (AJD_noadvice_reason %in% c(1,2,3)) ~ "Did not need an advisor",
        AJD_inst_advice == 2 & (AJD_noadvice_reason %in% c(4,5,6,7,8,9,10,98)) ~ "Needed but did not have access to an advisor",
        AJD_inst_advice == 98 ~ "Unknown Reason"
      ),
      
      # Access to DRM: Stage 1
      sankey_drm_stage_1 = case_when(
        is.na(non_trivial_problem) ~ NA_character_,
        non_trivial_problem == 0   ~ NA_character_,
        AJR_resolution == 1        ~ "Turned to a DRM",
        AJR_resolution == 2 & (
          AJR_noresol_reason %in% c(3,5,6,7,8)
        ) ~ "Needed but did not have access to a DRM",
        AJR_resolution == 2 & (
          AJR_noresol_reason %in% c(1,2,4,9,10)
        ) ~ "Did not need to turn to a DRM",
        AJR_resolution == 2 & (
          AJR_noresol_reason %in% c(11,98)
        ) ~ "Unknown Reason"
      ),
      
      # Access to DRM: Stage 2
      sankey_drm_stage_2 = case_when(
        is.na(non_trivial_problem) ~ NA_character_,
        non_trivial_problem == 0   ~ NA_character_,
        AJR_resolution == 1        ~ "Had access to a DRM",
        AJR_resolution == 2 & (
          AJR_noresol_reason %in% c(3,5,6,7,8)
        ) ~ "Did not have access to a DRM"
      )
      
    ) %>% 
    select(
      country_year_id,
      sankey_advice_stage_1, sankey_advice_stage_2, sankey_drm_stage_1, sankey_drm_stage_2
    )
  
  # Saving Justice Gap data
  write_csv(
    justice_gap_data[["noDK"]],
    "data/jg_data_nodk.csv"
  )
  write_csv(
    justice_gap_data[["keepDK"]],
    "data/jg_data_keepdk.csv"
  )
  
  # Listing individual data
  specialData <- reduce(
    list(
      probPrev,
      legal_needs,
      justice_gap_data[["noDK"]] %>%
        select(country_year_id, inside_justice_gap_nodk = inside_gap),
      justice_gap_data[["keepDK"]] %>%
        select(country_year_id, inside_justice_gap_keepdk = inside_gap),
      sankey_data
    ),
    left_join,
    by = "country_year_id"
  )
  
  return(specialData)
}