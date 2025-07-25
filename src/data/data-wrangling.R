add_a2j_vars <- function(data){

  # Legal Problem prevalence
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
    by = "country_year_id"
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
      across(
        starts_with("prevalence"),
        \(x) if_else(x > 0, 1, 0)
      )
    )
  
  # Other special wranglings
  agg_data <- data %>%
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
        A5b == 1  ~ 1,
        A5b <= 98 ~ 0
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
        AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_noresol %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_resol    == 3     ~ 0,
        AJR_state_resol    == 4     ~ 1,
        AJR_state_noresol  == 3     ~ 0,
        AJR_state_noresol  == 4     ~ 1
      ),
      
      # Police and Community Safety
      psafe1 = case_when(
        (LEP_safecom %in% c(1,2)) & (LEP_safefam %in% c(1,2)) & 
          (LEP_policehelp %in% c(1,2)) & (LEP_kindpol %in% c(1,2)) & 
          (LEP_polservcom %in% c(1,2)) ~ 1,
        is.na(LEP_safecom) & is.na(LEP_safefam) & is.na(LEP_policehelp) &
          is.na(LEP_kindpol) & is.na(LEP_polservcom) ~ NA_real_,
        (LEP_safecom %in% c(3,4,98)) | (LEP_safefam %in% c(3,4,98)) | 
          (LEP_policehelp %in% c(3,4,98)) | (LEP_kindpol %in% c(3,4,98)) | 
          (LEP_polservcom %in% c(3,4,98)) ~ 0
      ),
      
      # Government Services and Bribes
      bribery1 = case_when(
        (
          BRB_permit_B == 1 | BRB_benefits_B == 1 | BRB_id_B == 1 |
            BRB_school_B == 1 | BRB_health_B == 1
        ) ~ 1,
        (
          BRB_permit_B == 2 | BRB_benefits_B == 2 | BRB_id_B == 2 |
            BRB_school_B == 2 | BRB_health_B == 2
        ) ~ 0,
        (
          BRB_permit_B == 98 | BRB_benefits_B == 98 | BRB_id_B == 98 |
            BRB_school_B == 98 | BRB_health_B == 98
        ) ~ 0,
      )
    ) %>%
    select(
      country_year_id, 
      vulnerability1, vulnerability2, 
      access2info, access2rep, access2drm,
      rp_time, rp_cost, rp_fair, rp_outcome,
      psafe1, bribery1, non_trivial_problem
    )
  
  # Listing individual data
  specialData <- reduce(
    list(
      discrimination,
      probPrev,
      agg_data
    ),
    left_join,
    by = "country_year_id"
  )
  
  return(specialData)
}