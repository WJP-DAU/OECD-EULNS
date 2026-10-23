## 02_variables.R — variables derivadas por encuestado: demografía, quality check, A2J, justice gap, sankey y las
## variables del reporte (add_report_vars) ------------------------------------------------------------------------
## Origen: src/data/special-vars.R:14-75 (add_special_demographics), 78-80 (constantes del quality check), 83-226
## (build_quality_check, apply_quality_check_to_*, get_a2j_quality_check_spec, apply_a2j_quality_check) y 234-1013
## (add_a2j_vars); src/hungary/setup.R:52-67 (problem_map, categories, binary_agree) y 101-119 (problem_flags,
## has_lns); src/hungary/R03.R:4-38 (journey_vars: cuerpo del mutate), analysis_helpers.R:3-17 (trust_actors,
## build_analysis_frame), A03.R:7-10, A04.R:3-14 y 23, A05.R:5-14 (recodificaciones por encuestado).
## Copias literales (misma aritmética). Correcciones permitidas (§1.5), marcadas `# corregido:` en el código:
##   - special-vars.R:360 "5 or more problems" → "6 or more problems" (la etiqueta corresponde a nproblems >= 6).
##   - add_a2j_vars() ya no escribe data/jg_data_*.csv: devuelve attr(, "jg") = list(noDK, keepDK).
## Nada se ejecuta al cargar. 01_load.R::build_master() llama, en este orden: apply_a2j_quality_check(data) →
## add_a2j_vars(data) → add_special_demographics(data) → add_report_vars(df) (df = master de 27 países, sin etiquetas).
## Ningún valor de país va literal: el país es ctx$country; estas funciones son vectorizadas sobre todos los países.


## 1. Demografía recodificada ------------------------------------------------------------------------------------

# origen: special-vars.R:14-75. Devuelve country_year_id + 8 factores; build_master() los pasa a character
# (gender, financial, residence, age_group, edu_level, marital_status, nationality, emp_status)
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


## 2. Quality check (más de 25 problemas no triviales → fuera de los análisis de experiencia) ----------------------

# origen: special-vars.R:78-80
QUALITY_CHECK_NONTRIVIAL_THRESHOLD <- 25
QUALITY_CHECK_SEVERITY_THRESHOLD <- 4
QUALITY_FLAG_COLUMN <- "quality_flag_nontrivial_25"

# origen: special-vars.R:83-134. Cuenta problemas no triviales (bin == 1 y 4 <= sev < 98) por encuestado
build_quality_check <- function(data, legprob_bin, legprob_sev){
  
  quality_tbl <- reduce(
    list(
      data %>%
        select(
          country_year_id,
          all_of(legprob_bin)
        ) %>%
        pivot_longer(
          !country_year_id,
          names_to  = "problem",
          values_to = "answer"
        ) %>%
        mutate(
          problem = str_remove_all(problem, "AJP|_|bin"),
          answer  = as.numeric(haven::zap_labels(answer))
        ),
      
      data %>%
        select(
          country_year_id,
          all_of(legprob_sev)
        ) %>%
        pivot_longer(
          !country_year_id,
          names_to  = "problem",
          values_to = "severity"
        ) %>%
        mutate(
          problem  = str_remove_all(problem, "AJP|_|sev"),
          severity = as.numeric(haven::zap_labels(severity))
        )
    ),
    left_join,
    by = c("country_year_id", "problem")
  ) %>%
    mutate(
      non_trivial_problem_item = case_when(
        answer == 1 & severity >= QUALITY_CHECK_SEVERITY_THRESHOLD & severity < 98 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    group_by(country_year_id) %>%
    summarise(
      total_nontrivial_problems = sum(non_trivial_problem_item, na.rm = T),
      quality_flag_nontrivial_25 = total_nontrivial_problems > QUALITY_CHECK_NONTRIVIAL_THRESHOLD
    )
  
  return(quality_tbl)
  
}


# origen: special-vars.R:137-151. Encuestados marcados: variables de experiencia a NA
apply_quality_check_to_experience_vars <- function(data, experience_vars){
  
  data %>%
    mutate(
      across(
        all_of(experience_vars),
        \(x) if_else(
          quality_flag_nontrivial_25,
          NA,
          x
        )
      )
    )
  
}

# origen: special-vars.R:153-167. Encuestados marcados: AJP_*_bin a 2 (sin problema)
apply_quality_check_to_problem_vars <- function(data, problem_vars){
  
  data %>%
    mutate(
      across(
        all_of(problem_vars),
        \(x) if_else(
          quality_flag_nontrivial_25,
          2,
          as.numeric(haven::zap_labels(x))
        )
      )
    )
  
}

# origen: special-vars.R:169-198. Lista de problemas y de variables afectadas por el quality check
get_a2j_quality_check_spec <- function(){
  
  legalProblems <- c(
    "A1", "A2", "A3", "B1", "B2", "B3", "B4", "C1", "C2", "C3", "C4", "D1", "D2", "D3", "D4", "D5", "D6", "E1", 
    "E2", "E3", "F1", "F2", "G1", "G2", "G3", "H1", "H2", "H3", "I1", "J1", "J2", "J3", "J4", "K1", "K2", "K3", 
    "L1", "L2"
  )
  legprob_bin <- paste0("AJP_", legalProblems, "_bin")
  legprob_sev <- paste0("AJP_", legalProblems, "_sev")
  
  list(
    legalProblems = legalProblems,
    legprob_bin = legprob_bin,
    legprob_sev = legprob_sev,
    problem_vars = legprob_bin,
    experience_vars = c(
      "AJP_problem",
      legprob_sev,
      "AJE_infosource",
      "AJD_inst_advice", "AJD_expert_adviser", "AJD_noadvice_reason",
      paste0("AJD_adviser_", c(1:9, 98)),
      "AJR_resolution", "AJR_noresol_reason", "AJR_court_bin",
      "AJR_solvingtime", "AJR_slow", "AJR_solvingcosts", "AJR_costdiff",
      "AJR_expensive", "AJR_fair", "AJR_state_resol", "AJR_state_noresol",
      "AJR_outcome", "AJR_satis_outcome",
      "AJE_health", "AJE_emotional", "AJE_income", "AJE_drugs"
    )
  )
  
}

# origen: special-vars.R:200-226. Punto de entrada: build_master() la aplica una vez al .dta filtrado
apply_a2j_quality_check <- function(data){
  
  spec <- get_a2j_quality_check_spec()
  
  quality_check <- build_quality_check(
    data = data,
    legprob_bin = spec$legprob_bin,
    legprob_sev = spec$legprob_sev
  )
  
  data %>%
    left_join(
      quality_check,
      by = "country_year_id"
    ) %>%
    apply_quality_check_to_problem_vars(
      problem_vars = spec$problem_vars
    ) %>%
    apply_quality_check_to_experience_vars(
      experience_vars = spec$experience_vars
    ) %>%
    select(
      -total_nontrivial_problems,
      -quality_flag_nontrivial_25
    )
  
}


## 3. Variables A2J, justice gap y sankey -------------------------------------------------------------------------

# origen: special-vars.R:234-1013. Devuelve una fila por encuestado (country_year_id) con prevalence1/2, nproblems,
# cooccurence_group, vulnerability1-3, access2info/rep/drm, rp_*, problem_status, resolution_favor, rp_satisfaction,
# non_trivial_problem, selected_problem_category, hardship, hardships_*, inside_justice_gap_nodk/keepdk y
# sankey_*_stage_*. Además attr(, "jg") = list(noDK, keepDK): tablas por encuestado con jg_* (o jg_inverse_*),
# jg_resol_process, a2j_score (keepDK: también a2j_inverse_score) e inside_gap (a2j_score < 0.67).
add_a2j_vars <- function(data){

  ### Legal Problem prevalence ----
  quality_spec <- get_a2j_quality_check_spec()
  legalProblems <- quality_spec$legalProblems
  legprob_bin <- quality_spec$legprob_bin
  legprob_sev <- quality_spec$legprob_sev
  
  quality_check <- build_quality_check(
    data = data,
    legprob_bin = legprob_bin,
    legprob_sev = legprob_sev
  )
  
  data <- data %>%
    left_join(
      quality_check,
      by = "country_year_id"
    ) %>%
    apply_quality_check_to_problem_vars(
      problem_vars = quality_spec$problem_vars
    ) %>%
    apply_quality_check_to_experience_vars(
      experience_vars = quality_spec$experience_vars
    )
  
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
    left_join(
      quality_check,
      by = "country_year_id"
    ) %>%
    group_by(country_year_id) %>%
    summarise(
      quality_flag_nontrivial_25 = first(quality_flag_nontrivial_25),
      has_lns_data = any(!is.na(answer)),  # Check if respondent has LNS module data
      across(
        starts_with("prevalence"),
        \(x) sum(x, na.rm = T)
      )
    ) %>%
    mutate(
      # If no LNS data (all answers are NA), set prevalence to NA
      across(
        starts_with("prevalence"),
        \(x) if_else(!has_lns_data, NA_real_, x)
      )
    ) %>%
    select(-has_lns_data) %>%
    mutate(
      across(
        starts_with("prevalence"),
        \(x) if_else(quality_flag_nontrivial_25, NA_real_, x)
      ),
      nproblems = prevalence2,
      cooccurence_group = case_when(
        quality_flag_nontrivial_25 ~ NA_character_,
        nproblems == 0 ~ NA_character_,
        nproblems == 1 ~ "1 problem",
        nproblems <= 3 ~ "2-3 problems",
        nproblems <= 5 ~ "4-5 problems",
        nproblems >= 6 ~ "6 or more problems"   # corregido: special-vars.R:360 decía "5 or more problems"
      ),
      across(
        starts_with("prevalence"),
        \(x) if_else(x > 0, 1, 0)
      )
    ) %>%
    select(
      -quality_flag_nontrivial_25
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
      # access2rep = case_when(
      #   is.na(non_trivial_problem) ~ NA_real_,
      #   non_trivial_problem == 0   ~ NA_real_,
      #   AJD_inst_advice == 1 & (
      #     AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 | 
      #       AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 |
      #       AJD_adviser_8 == 1
      #   ) ~ 1,
      #   AJD_inst_advice == 1 & AJD_adviser_1 == 1 & AJD_expert_adviser == 1 ~ 1, # Friend/Family with legal background
      #   AJD_inst_advice == 1 & (
      #     AJD_adviser_1 == 1 | AJD_adviser_9 == 1 | AJD_adviser_98 == 1
      #   ) ~ 0,
      #   AJD_inst_advice == 2 & (AJD_noadvice_reason %in% c(1,2,3)) ~ 1,
      #   AJD_inst_advice == 2 & (AJD_noadvice_reason %in% c(4,5,6,7,8,9,10,98)) ~ 0,
      #   AJD_inst_advice == 98 ~ 0
      # ),
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
        AJD_inst_advice == 2 & (AJD_noadvice_reason %in% c(1,2,3)) ~ NA_real_,
        AJD_inst_advice == 2 & (AJD_noadvice_reason %in% c(4,5,6,7,8,9,10,98)) ~ 0,
        AJD_inst_advice == 98 ~ NA_real_
      ),

      # Access to a dispute resolution mechanism
      # access2drm = case_when(
      #   is.na(non_trivial_problem) ~ NA_real_,
      #   non_trivial_problem == 0   ~ NA_real_,
      #   AJR_resolution == 1        ~ 1,
      #   AJR_resolution == 2 & (AJR_noresol_reason %in% c(3,5,6,7,8)) ~ 0
      #   # AJR_resolution == 98 (We don't know if they really needed the DRM, so we exclude 98s)
      # ),
      
      # Access to a dispute resolution mechanism
      access2drm = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_resolution == 1        ~ 1,
        AJR_resolution == 2 & (AJR_noresol_reason %in% c(3,5,6,7,8)) ~ 0,
        AJR_resolution == 98 ~ NA_real_ 
      ),
      
      # Court mentioned as drm
      court_as_DRM = case_when(
        AJR_court_bin == 1 ~ "Court or Tribunal",
        AJR_court_bin == 0 ~ "Other"
      ),
      
      # Timeliness of the resolution process
      # rp_time = case_when(
      #   is.na(non_trivial_problem) ~ NA_real_,
      #   non_trivial_problem == 0   ~ NA_real_,
      #   AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
      #   AJR_state_noresol  %in% c(1,2,98,99) ~ NA_real_,
      #   AJR_solvingtime == -9999    ~ NA_real_,
      #   AJR_solvingtime == -8888    ~ 0,
      #   AJR_solvingtime >  12  ~ 0,
      #   AJR_solvingtime <= 12  ~ 1
      # ),

      # Timeliness of the resolution process
      rp_time = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
        AJR_state_noresol  %in% c(1,2,98,99) ~ NA_real_,
        AJR_solvingtime == -9999    ~ NA_real_,
        AJR_solvingtime == -8888    ~ 0,
        AJR_solvingtime >  12 & AJR_solvingtime < 121 ~ 0,
        AJR_solvingtime >= 0 & AJR_solvingtime <= 12  ~ 1
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
      
      # Hardships (individual components)
      across(
        c(AJE_health, AJE_emotional, AJE_income, AJE_drugs),
        \(x) case_when(
          is.na(non_trivial_problem) ~ NA_real_,
          non_trivial_problem == 0   ~ NA_real_,
          x == 1 ~ 1,
          x %in% c(2,98) ~ 0
        ),
        .names = "hardships_{str_remove(.col, 'AJE_')}"
      ),

      # Hardship (combined indicator)
      # Following Stata logic: 99 (No Answer) is treated as missing
      # 1 = at least one hardship reported
      # 0 = no hardships (all answered 2 or 98, ignoring 99/NA)
      # NA = non-trivial problem is NA/0, or all responses are 99/NA

      # First convert 99 to NA for hardship calculation
      h_health = if_else(AJE_health == 99, NA_real_, as.numeric(AJE_health)),
      h_emotional = if_else(AJE_emotional == 99, NA_real_, as.numeric(AJE_emotional)),
      h_income = if_else(AJE_income == 99, NA_real_, as.numeric(AJE_income)),
      h_drugs = if_else(AJE_drugs == 99, NA_real_, as.numeric(AJE_drugs)),

      # Combined hardship using cleaned variables
      hardship = case_when(
        is.na(non_trivial_problem) ~ NA_real_,
        non_trivial_problem == 0   ~ NA_real_,
        # At least one hardship reported
        h_health == 1 | h_emotional == 1 | h_income == 1 | h_drugs == 1 ~ 1,
        # No hardships: all are 2 or 98 (ignoring 99/NA)
        (h_health %in% c(2, 98) | is.na(h_health)) &
        (h_emotional %in% c(2, 98) | is.na(h_emotional)) &
        (h_income %in% c(2, 98) | is.na(h_income)) &
        (h_drugs %in% c(2, 98) | is.na(h_drugs)) ~ 0
      )

    ) %>% 
    select(
      country_year_id,
      vulnerability1, vulnerability2, vulnerability3,
      access2info, access2rep, access2drm,
      rp_time, rp_cost, rp_fair, rp_outcome, rp_quick, rp_expensive,
      problem_status, resolution_favor, rp_satisfaction,
      non_trivial_problem, selected_problem_category,
      hardship, starts_with("hardships_")
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
  
  # corregido: sin write_csv (special-vars.R:987-995 escribía data/jg_data_nodk.csv y data/jg_data_keepdk.csv);
  # justice_gap_data se devuelve como attr(, "jg") = list(noDK, keepDK) para 01_load.R::build_master()
  
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
  
  attr(specialData, "jg") <- justice_gap_data
  return(specialData)
}


## 4. Mapa de problemas y banderas por categoría ------------------------------------------------------------------

# origen: setup.R:52-67. Código AJP → categoría del reporte (misma agrupación que selected_problem_category);
# categories en orden alfabético; binary_agree: confianza = mucha + algo (1, 2) frente a (3, 4, 98)
problem_map <- c(
  A1 = "Consumer", A2 = "Consumer", A3 = "Consumer",
  B1 = "Land", B2 = "Land", B3 = "Land", B4 = "Land",
  C1 = "Housing", C2 = "Housing", C4 = "Housing", C3 = "Community", E3 = "Community",
  D1 = "Family", D2 = "Family", D3 = "Family", D4 = "Family", D5 = "Family", D6 = "Family",
  E1 = "Education", E2 = "Education",
  F1 = "Accidental", F2 = "Accidental",
  G1 = "Employment", G2 = "Employment", G3 = "Employment",
  H1 = "Public Services", H2 = "Public Services", H3 = "Public Services", J4 = "Public Services",
  I1 = "Law Enforcement",
  J1 = "Citizenship & ID", J2 = "Citizenship & ID", J3 = "Citizenship & ID",
  K1 = "Money & Debt", K2 = "Money & Debt", K3 = "Money & Debt", L1 = "Money & Debt", L2 = "Money & Debt"
)
categories <- sort(unique(problem_map))

binary_agree <- function(x) case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4, 98) ~ 0)

# origen: setup.R:101-119. Indicadores por encuestado y categoría: no trivial (bin == 1 y 4 <= sev < 98) o de
# cualquier severidad; el quality check ya viene aplicado (marcados: AJP_*_bin = 2 y severidades NA).
# has_lns(): TRUE si el encuestado tiene algún AJP_*_bin no NA (módulo LNS aplicado)
problem_flags <- function(df, min_sev = 4, any_severity = FALSE) {
  out <- lapply(categories, function(cat) {
    codes <- names(problem_map)[problem_map == cat]
    hits <- sapply(codes, function(code) {
      b <- df[[paste0("AJP_", code, "_bin")]]
      s <- df[[paste0("AJP_", code, "_sev")]]
      if (any_severity) !is.na(b) & b == 1
      else !is.na(b) & b == 1 & !is.na(s) & s >= min_sev & s < 98
    })
    rowSums(matrix(hits, nrow = nrow(df))) > 0
  })
  names(out) <- categories
  as_tibble(out)
}

has_lns <- function(df) {
  bins <- df %>% select(matches("^AJP_[A-L][0-9]_bin$"))
  rowSums(!is.na(bins)) > 0
}


## 5. Variables del reporte: add_report_vars(df) ------------------------------------------------------------------

## Añade al master (27 países; una fila por encuestado) las variables por encuestado que usan los recorridos (R03)
## y las tarjetas A03/A04/A05 (figuras 32, 41, 42). Solo recodificaciones vectorizadas: sin regions ni estimación.
## Las bases de cada cruce quedan para la estimación (03_estimators.R / figures/): run_A03 filtra has_nt antes de
## cruzar (resuelto, favor); run_A04/run_A05 no filtran; xt() descarta los NA del desenlace y del grupo.
## Columnas que añade (39):
##   R03.R:5-37 (journey_vars):     adv, drm, adv_contacted, adv_s_appropriate, adv_s_nonappr, adv_s_needed_no,
##                                  adv_s_noneed, adv_s_unknown, rep_a_all, rep_b_need_unk, rep_c_published,
##                                  rep_d_textdef, drm_s_turned, drm_s_needed_no, drm_s_noneed, drm_s_unknown,
##                                  drm_nc_needed_no, drm_nc_noneed, drm_nc_unknown, drm_a_all, drm_b_need_unk,
##                                  drm_c_published, drm_d_textdef
##   analysis_helpers.R:6-17:       trust_TRT_judges, trust_TRT_prosecutors, trust_TRT_pda, has_nt, resolved,
##                                  in_favor, cooc3, housing_land_nt
##   A03.R:7-10 (run_A03):          resuelto, favor
##   A04.R:3-13 y 23:               exposicion (exposure_ladder), prob
##   A05.R:5-14 (run_A05):          justo, tribunal, asistencia, escalera_hardship (favor: misma fórmula que A03)

# origen: analysis_helpers.R:3
trust_actors <- c("Judges" = "TRT_judges", "Prosecutors" = "TRT_prosecutors", "Public defense attorneys" = "TRT_pda")

# origen: A04.R:3-14. Escalera de exposición al sistema; razón desconocida y AJR_resolution = 98 quedan NA
exposure_ladder <- function(af) {
  af %>% mutate(
    exposicion = case_when(
      !has_nt ~ "No non-trivial problem",
      sankey_drm_stage_1 == "Did not need to turn to a DRM" ~ "Problem, did not need a DRM",
      sankey_drm_stage_1 == "Needed but did not have access to a DRM" ~ "Needed a DRM, no access",
      sankey_drm_stage_1 == "Turned to a DRM" ~ "Needed a DRM, had access",
      TRUE ~ NA_character_   # unknown reason (decisión: NA) or AJR_resolution = 98 (no DRM stage)
    )
  )
}
ladder_levels <- c("No non-trivial problem", "Problem, did not need a DRM", "Needed a DRM, no access", "Needed a DRM, had access")

# Nueva (contrato §5.A): build_analysis_frame() de analysis_helpers.R:6-17 aplicada a df en vez de hu, con el cuerpo
# de journey_vars() (R03.R:4-38) copiado dentro, más las recodificaciones por encuestado de A03, A04 y A05.
add_report_vars <- function(df) {
  nt <- problem_flags(df)                                   # origen: analysis_helpers.R:7
  df %>%
    # origen: R03.R:5-37 (journey_vars; la versión que estima vive en 03_estimators.R)
    mutate(
      adv = sankey_advice_stage_2,
      drm = sankey_drm_stage_1,
      # Asistencia
      adv_contacted       = if_else(!is.na(adv), as.numeric(sankey_advice_stage_1 == "Contacted an advisor"), NA_real_),
      adv_s_appropriate   = if_else(!is.na(adv), as.numeric(adv == "Appropriate Advisor"), NA_real_),
      adv_s_nonappr       = if_else(!is.na(adv), as.numeric(adv == "Non-Appropriate Advisor"), NA_real_),
      adv_s_needed_no     = if_else(!is.na(adv), as.numeric(adv == "Needed but did not have access to an advisor"), NA_real_),
      adv_s_noneed        = if_else(!is.na(adv), as.numeric(adv == "Did not need an advisor"), NA_real_),
      adv_s_unknown       = if_else(!is.na(adv), as.numeric(adv == "Unknown Reason"), NA_real_),
      # Variantes del indicador de acceso a asistencia
      rep_a_all        = adv_s_appropriate,                                                    # (a) sobre todos
      rep_b_need_unk   = if_else(adv %in% c("Did not need an advisor"), NA_real_, adv_s_appropriate),      # (b) excluye no necesitó
      rep_c_published  = if_else(adv %in% c("Did not need an advisor", "Unknown Reason"), NA_real_, adv_s_appropriate), # (c) excluye no necesitó y desconocida = publicado
      rep_d_textdef    = case_when(adv == "Unknown Reason" ~ NA_real_,
                                   adv %in% c("Appropriate Advisor", "Did not need an advisor") ~ 1,
                                   !is.na(adv) ~ 0),                                           # (d) definición del texto p.22
      # DRM
      drm_s_turned     = if_else(!is.na(drm), as.numeric(drm == "Turned to a DRM"), NA_real_),
      drm_s_needed_no  = if_else(!is.na(drm), as.numeric(drm == "Needed but did not have access to a DRM"), NA_real_),
      drm_s_noneed     = if_else(!is.na(drm), as.numeric(drm == "Did not need to turn to a DRM"), NA_real_),
      drm_s_unknown    = if_else(!is.na(drm), as.numeric(drm == "Unknown Reason"), NA_real_),
      drm_nc_needed_no = if_else(drm %in% c("Turned to a DRM") | is.na(drm), NA_real_, drm_s_needed_no),
      drm_nc_noneed    = if_else(drm %in% c("Turned to a DRM") | is.na(drm), NA_real_, drm_s_noneed),
      drm_nc_unknown   = if_else(drm %in% c("Turned to a DRM") | is.na(drm), NA_real_, drm_s_unknown),
      drm_a_all        = drm_s_turned,
      drm_b_need_unk   = if_else(drm %in% c("Did not need to turn to a DRM"), NA_real_, drm_s_turned),
      drm_c_published  = if_else(drm %in% c("Did not need to turn to a DRM", "Unknown Reason"), NA_real_, drm_s_turned),
      drm_d_textdef    = case_when(drm == "Unknown Reason" ~ NA_real_,
                                   drm %in% c("Turned to a DRM", "Did not need to turn to a DRM") ~ 1,
                                   !is.na(drm) ~ 0)
    ) %>%
    # origen: analysis_helpers.R:9-16 (build_analysis_frame)
    mutate(
      across(all_of(unname(trust_actors)), binary_agree, .names = "trust_{.col}"),
      has_nt = non_trivial_problem %in% 1,
      resolved = rp_outcome,
      in_favor = case_when(resolution_favor == "In favor of respondent" ~ 1, resolution_favor == "Not in favor / Unknown" ~ 0),
      cooc3 = case_when(nproblems == 1 ~ "1 problem", nproblems %in% 2:3 ~ "2-3 problems", nproblems >= 4 ~ "4 or more problems"),
      housing_land_nt = as.numeric(nt[["Housing"]] | nt[["Land"]])
    ) %>%
    # origen: A03.R:8-9 (run_A03; allí tras filter(has_nt))
    mutate(
      resuelto = case_when(resolved == 1 ~ "Fully resolved", resolved == 0 ~ "Not resolved"),
      favor = case_when(in_favor == 1 ~ "In favor of respondent", in_favor == 0 ~ "Not in favor / Unknown")
    ) %>%
    # origen: A04.R:18 y 23 (run_A04)
    exposure_ladder() %>%
    mutate(prob = if_else(has_nt, "Yes", "No")) %>%
    # origen: A05.R:7-13 (run_A05)
    mutate(
      justo = case_when(rp_fair == 1 ~ "Process was fair", rp_fair == 0 ~ "Process was not fair"),
      tribunal = case_when(sankey_drm_stage_1 == "Turned to a DRM" & AJR_court_bin %in% 1 ~ "Went to a court",
                           sankey_drm_stage_1 == "Turned to a DRM" ~ "Other mechanisms only"),
      asistencia = case_when(rep_c_published == 1 ~ "Needed assistance, got it", rep_c_published == 0 ~ "Needed assistance, did not get it"),
      escalera_hardship = case_when(!has_nt ~ "No non-trivial problem",
                                    has_nt & hardship == 0 ~ "Problem, no hardship",
                                    has_nt & hardship == 1 ~ "Problem, with hardship")
    )
}


## Prueba ---------------------------------------------------------------------------------------------------------
## Comando: cd /tmp && Rscript --vanilla SCRATCH/agente_A/prueba.R (SCRATCH = scratchpad de la sesión; 6 s en total).
## Carga: parse() ok; source() de 00_setup.R + 01_load.R + 02_variables.R sin error; nada se ejecuta al cargar.
## Bloque 1 (primeras 4000 filas del .dta: 444 columnas, 147 AJP_/AJD_/AJR_/AJE_, 3 países):
##   apply_a2j_quality_check: 4000 filas y las mismas columnas que la entrada; 33 encuestados marcados (> 25 no triviales).
##   add_a2j_vars: 4000 x 33, una fila por country_year_id; attr(, "jg") = list(noDK, keepDK); keepDK 4000 x 11 con las
##   mismas columnas que data/jg_data_keepdk.csv; cooccurence_group = {1 problem 676, 2-3 problems 722, 4-5 problems 361,
##   6 or more problems 510, NA 1731}; ninguna etiqueta "5 or more problems".
##   add_special_demographics: 4000 x 9 (country_year_id + 8 recodificaciones).
##   build_master(raw, regions) de 01_load.R: 4000 x 249; select(any_of(MASTER_VARS), starts_with(MASTER_PREFIXES)) da
##   200 columnas, el mismo conjunto que data/master.csv (que tiene 200, no 199 como dice checks$ncol_master_csv de
##   config/<pais>.yml); las 39 columnas de add_report_vars están; add_report_vars es idempotente (reaplicarla no cambia nada).
## Bloque 2 (fidelidad): trozo de 4000 filas desde la primera fila del país del reporte (skip = 32408) → build_master →
##   2712 encuestados comunes con data/master.csv; 198 de 200 columnas idénticas. Diferencias: cooccurence_group (473,
##   todas "6 or more problems" frente a "5 or more problems": la corrección permitida) y AJP_problem (644, todas "" en
##   el .dta frente a NA de read_csv). Ninguna fila de microdatos impresa.
