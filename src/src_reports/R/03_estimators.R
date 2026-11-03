## 03_estimators.R — estimadores del reporte: promedio NUTS ponderado, tabla de resultados, media UE y recorrido -----
## Origen: src/utils/utils.R:19-150 (get_nuts_avg, get_wgt_national_avg; literales); utils.R:203-335 (get_results_table;
## adaptada a ctx); src/hungary/setup.R:125-166 (nat_est, eu_est, nuts_mean_long; adaptadas a ctx); src/hungary/R03.R:4-38
## (journey_vars; literal); src/hungary/F06.R:3-15 (perception_experience; adaptada a ctx); src/hungary/analysis_helpers.R:20-51
## (xt, xt_nat, diff_stratified; adaptadas a ctx); src/hungary/F02.R:3-12 (problems_long, figuras 3 y 7; literal, al
## final); src/hungary/F01.R:6-7 (cooc_flags, figuras 5 y 6; adaptada a ctx, al final); src/analysis/module_2_2.R:235-263,
## 303-348 y module_2_3.R:203-231, 300-346 (contacted_shares, reason_shares; figuras 13, 14, 18, 19; al final). Nuevas:
## memo(), eu_value(), journey(), JOURNEY_VARS, with_journey(), fn_id().
## Adaptación común a ctx: `regions` global → ctx$regions; `hu`/`master` → df (por defecto ctx$national); `pct` → `value`;
## el país es ctx$country (sin study_countries). Sin efectos secundarios: nada escribe ni imprime (log_msg solo agregados).
## Firma: ctx primero en todo, salvo problems_long(df, extra), que es literal y no estima: solo pasa a formato largo el
## df de encuestados que recibe (ctx$national en la fig 3, ctx$master en la 7).
## Usa de 02_variables.R: binary_agree() (perception_experience), problem_flags() (cooc_flags) y problem_map
## (problems_long); se resuelven al llamar.
## Lo consumen 01_load.R (data_checks: nat_est, eu_est, journey) y figures/*.R (data(ctx)): ver figures/_template.R.
## Convención: todas las estimaciones son promedios de las regiones NUTS ponderados por regionpoppct, con regiones
## de n < 30 suprimidas (NA); value en % 0-100 (o media 0-10 si target ya es una escala). Nada se redondea aquí.


## 1. Promedio regional y promedio nacional ponderado -------------------------------------------------------------

# origen: utils.R:19-73 (literal). Media de target por país x NUTS (x grupo); regiones con n < 30 → NA.
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


# origen: utils.R:82-150 (literal). Promedio nacional = suma de medias regionales x regionpoppct renormalizado a las
# regiones con dato; todas NA → NA.
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


## 2. Tabla de resultados por grupos ------------------------------------------------------------------------------

# origen: utils.R:203-335 — adaptada a ctx: sin `master`/`regions`/`study_countries` (df por defecto ctx$national, regiones
# de ctx$regions); una sola columna `value` (la de ctx$country); `transform_var` → `transform`; `omit` = clabels a excluir
# (p. ej. "Category" para muestras pequeñas, como omit.cats en src/viz). Devuelve tibble(grouping, category, value):
# grouping es factor con niveles = clabels (orden original de los grupos; as.character() para el df de §4.4), la primera
# fila es grouping "National" / category "National Average", y n < 30 → NA. Dentro de cada grupo las categorías salen en
# orden alfabético (C); rows_grouped() (05_charts.R) fija el orden de dibujo.
get_results_table <- function(
    ctx, target,
    ctype = "short",
    prop = TRUE,
    transform = NULL,
    omit = NULL,
    df = NULL
  ){

  if (is.null(df)) df <- ctx$national
  master  <- with_journey(df, target)       # adaptado: el cuerpo literal usa `master` y `regions`
  regions <- ctx$regions
  if (!ctype %in% c("full", "short", "trust", "custom_1")) stop("get_results_table: ctype desconocido: ", ctype, call. = FALSE)

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

  if (!is.null(omit)) {                     # adaptado: groupings a excluir (por clabel)
    keep <- !names(categories) %in% omit
    categories <- categories[keep]
    clabels <- clabels[keep]
  }

  if(!is.null(transform)){                  # origen: utils.R:256-260 con transform_var → transform
    tranforming_fct = transform
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
            category = "National Average",   # adaptado: contrato §5.B (utils.R:279 ponía "National")
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

  # adaptado: utils.R:324-332 dividía por 100 las columnas de study_countries; aquí la única columna es la de ctx$country
  if (!ctx$country %in% names(results_tbl)) stop("get_results_table: df sin encuestados de ", ctx$country, call. = FALSE)
  results_tbl <- results_tbl %>%
    select(grouping, category, value = all_of(ctx$country))

  if (!prop){
    results_tbl <- results_tbl %>%
      mutate(value = value/100)
  }

  return(results_tbl)
}


## 3. Estimadores nacionales y media UE ---------------------------------------------------------------------------

# origen: setup.R:125-139 — adaptada: `regions` global → ctx$regions; `pct` → `value`; df por defecto ctx$national
# (with_journey() añade target o by si son del recorrido y df no los trae). Devuelve tibble(country_name_ltn, <by>, value, n): una
# fila por país presente en df (varios solo si se pasa df = ctx$master, como hace eu_est); by = NULL → una fila.
# n = encuestados con dato (sin ponderar, sin suprimir).
nat_est <- function(ctx, target, transform = NULL, by = NULL, df = NULL) {
  if (is.null(df)) df <- ctx$national
  df <- with_journey(df, c(target, by))
  regions <- ctx$regions
  df <- df %>% mutate(.t = .data[[target]])
  if (!is.null(transform)) df <- df %>% mutate(.t = transform(.t))
  n_tbl <- df %>%
    filter(!is.na(.t), if (is.null(by)) TRUE else !is.na(.data[[by]])) %>%
    count(country_name_ltn, across(any_of(by)), name = "n")
  reg <- get_nuts_avg(df, ".t", disaggregate_by = by)
  res <- get_wgt_national_avg(reg, regions, target_col = "proportion", group_col = if (is.null(by)) NULL else "group") %>%
    mutate(value = proportion * 100) %>%    # adaptado: pct → value
    select(-proportion)
  if (!is.null(by)) {
    res <- res %>% rename(!!by := category)
  }
  left_join(res, n_tbl, by = c("country_name_ltn", by))
}

# origen: setup.R:142-150 — adaptada: devuelve el escalar eu_pct (media simple de las estimaciones nacionales de
# ctx$meta$countries sobre ctx$master; países sin dato fuera), memorizado por (ctx, target, transform). Sin `by`.
# n_countries queda en el log. Países sin módulo LNS dan NA y no cuentan.
eu_est <- function(ctx, target, transform = NULL) {
  countries <- ctx$meta$countries %||% unique(ctx$master$country_name_ltn)
  memo(ctx, paste("eu_est", target, fn_id(transform), sep = "|"), {
    est <- nat_est(ctx, target, transform, df = ctx$master %>% filter(country_name_ltn %in% countries))
    eu_pct <- mean(est$value, na.rm = TRUE)
    log_msg("eu_est(", target, ") = ", formatC(eu_pct, format = "f", digits = 2), " · ",
            sum(!is.na(est$value)), " países con dato de ", length(countries))
    eu_pct
  })
}

# origen: setup.R:152-166 — adaptada: `regions` global → ctx$regions. `long` = tabla a nivel problema (una fila por
# encuestado x problema) con country_name_ltn y nuts_id; devuelve tibble(country_name_ltn, <groups>, est, n).
nuts_mean_long <- function(ctx, long, value, groups = NULL) {
  regions <- ctx$regions
  long %>%
    filter(!is.na(.data[[value]])) %>%
    group_by(country_name_ltn, nuts_id, across(all_of(groups))) %>%
    summarise(m = mean(.data[[value]]), n = n(), .groups = "drop") %>%
    mutate(m = if_else(n < 30, NA_real_, m)) %>%
    left_join(regions %>% select(nuts_id, wgt = regionpoppct), by = "nuts_id") %>%
    group_by(country_name_ltn, across(all_of(groups))) %>%
    summarise(
      est = if (all(is.na(m))) NA_real_ else sum(m * wgt, na.rm = TRUE) / sum(wgt[!is.na(m)]),
      n = sum(n), .groups = "drop"
    )
}


## 4. Recorrido: asistencia y DRM ---------------------------------------------------------------------------------

# origen: R03.R:4-38 (literal). Variables por encuestado del recorrido sobre las sankey_* de special-vars.R; base común:
# problema no trivial con etapa definida. `regions` no se usa (la firma del contrato lo trae; la estimación la hace
# journey()). Recalcularlas sobre un df que ya las traiga (add_report_vars) da lo mismo.
journey_vars <- function(df, regions = NULL) {
  df %>%
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
    )
}

# Variables numéricas que crea journey_vars() (adv y drm son las etapas en texto), en su orden
JOURNEY_VARS <- c(
  "adv_contacted", "adv_s_appropriate", "adv_s_nonappr", "adv_s_needed_no", "adv_s_noneed", "adv_s_unknown",
  "rep_a_all", "rep_b_need_unk", "rep_c_published", "rep_d_textdef",
  "drm_s_turned", "drm_s_needed_no", "drm_s_noneed", "drm_s_unknown",
  "drm_nc_needed_no", "drm_nc_noneed", "drm_nc_unknown",
  "drm_a_all", "drm_b_need_unk", "drm_c_published", "drm_d_textdef"
)

# Nuevo: estimaciones nacionales (nat_est, % 0-100, sin redondear) de cada JOURNEY_VARS sobre ctx$national, memorizadas.
# Lista con esos nombres más no_need_r1/unknown_r1 (adv_s_noneed/adv_s_unknown a 1 decimal) y drm_no_need_r1/
# drm_unknown_r1 (ídem DRM): los nodos "excluded" de los sankeys suman las partes redondeadas (standard_figures.R:129-133,
# 148, 154, 167: excluded = round(no_need_r1 + unknown_r1)). `n` = vector con nombres (encuestados con dato por variable).
journey <- function(ctx) {
  memo(ctx, "journey", {
    hj <- journey_vars(ctx$national, ctx$regions)
    est <- lapply(JOURNEY_VARS, function(v) nat_est(ctx, v, df = hj))
    out <- setNames(lapply(est, function(e) e$value[[1]]), JOURNEY_VARS)
    out$no_need_r1 <- round(out$adv_s_noneed, 1)
    out$unknown_r1 <- round(out$adv_s_unknown, 1)
    out$drm_no_need_r1 <- round(out$drm_s_noneed, 1)
    out$drm_unknown_r1 <- round(out$drm_s_unknown, 1)
    out$n <- setNames(vapply(est, function(e) as.integer(e$n[[1]]), 1L), JOURNEY_VARS)
    log_msg("journey(ctx): ", length(JOURNEY_VARS), " variables · base asistencia n = ", out$n[["adv_contacted"]],
            " · base DRM n = ", out$n[["drm_s_turned"]])
    out
  })
}


## 5. Percepción contra experiencia -------------------------------------------------------------------------------

# origen: F06.R:3-15 — adaptada: ctx (df por defecto ctx$national, con journey_vars() si exp lo exige); añade n_p/n_e (n de
# nat_est) y omite `brecha` (= experience − perception). Fila National como el original: grouping "" y category
# "National Average" (en get_results_table la fila National lleva grouping "National"). jse se binariza con
# binary_agree() (02_variables.R; 1-2 = acuerdo); exp se toma tal cual. Devuelve tibble(grouping, category, perception,
# experience, n_p, n_e) con los cortes Gender, Age Group, Financial Situation, Area of Residence.
perception_experience <- function(ctx, jse, exp, df = NULL) {
  if (is.null(df)) df <- ctx$national
  df <- with_journey(df, c(jse, exp))
  cuts <- c("Gender" = "gender", "Age Group" = "age_group", "Financial Situation" = "financial", "Area of Residence" = "residence")
  p <- nat_est(ctx, jse, binary_agree, df = df)
  e <- nat_est(ctx, exp, df = df)
  nat <- tibble(grouping = "", category = "National Average",
                perception = p$value, experience = e$value, n_p = p$n, n_e = e$n)
  by_cut <- imap_dfr(cuts, function(v, lab) {
    left_join(
      nat_est(ctx, jse, binary_agree, by = v, df = df) %>% select(category = all_of(v), perception = value, n_p = n),
      nat_est(ctx, exp, by = v, df = df) %>% select(category = all_of(v), experience = value, n_e = n),
      by = "category"
    ) %>% mutate(grouping = lab)
  })
  bind_rows(nat, by_cut) %>% select(grouping, category, perception, experience, n_p, n_e)
}


## 6. Cruces de los análisis adicionales (A0x) --------------------------------------------------------------------

# origen: analysis_helpers.R:19-24 — adaptada: ctx (df por defecto ctx$national); nat_est da `value` (antes `pct`); la
# salida conserva los nombres originales: desenlace, corte, grupo, pct, n, n_menor_30.
xt <- function(ctx, outcome, group, label_outcome = outcome, label_group = group, df = NULL) {
  if (is.null(df)) df <- ctx$national
  df <- with_journey(df, c(outcome, group))              # también si el grupo es del recorrido (adv, drm, …)
  nat_est(ctx, outcome, by = ".g", df = df %>% mutate(.g = as.character(.data[[group]]))) %>%
    transmute(desenlace = label_outcome, corte = label_group, grupo = .g, pct = value, n, n_menor_30 = n < 30)
}
xt_nat <- xt

# origen: analysis_helpers.R:26-51 — adaptada: ctx (df por defecto ctx$national); pick("pct") → pick("value"). Desenlace
# por exposición binaria (1 frente a 0), crudo y por estratos; la diferencia estandarizada promedia las diferencias
# por estrato ponderadas por su tamaño.
diff_stratified <- function(ctx, outcome, exposure, strata = NULL, df = NULL) {
  if (is.null(df)) df <- ctx$national
  df <- with_journey(df, c(outcome, exposure, strata))   # también si la exposición o el estrato son del recorrido
  est <- function(d) {
    r <- nat_est(ctx, outcome, by = ".e", df = d %>% mutate(.e = as.character(.data[[exposure]])))
    pick <- function(col, lev, default) { v <- r[[col]][r$.e == lev]; if (length(v) == 0) default else v }
    tibble(con = pick("value", "1", NA_real_), sin = pick("value", "0", NA_real_),
           n_con = pick("n", "1", 0L), n_sin = pick("n", "0", 0L))
  }
  d <- df %>% filter(!is.na(.data[[outcome]]), !is.na(.data[[exposure]]))
  out <- est(d) %>% mutate(estrato = "Sin control", .before = 1)
  if (!is.null(strata)) {
    levels <- sort(unique(na.omit(as.character(d[[strata]]))))
    by_s <- map_dfr(levels, \(s) est(d %>% filter(.data[[strata]] == s)) %>% mutate(estrato = paste0(strata, ": ", s), .before = 1))
    valid <- by_s %>% filter(!is.na(con), !is.na(sin))
    out <- bind_rows(
      out %>% mutate(diferencia = con - sin), by_s %>% mutate(diferencia = con - sin),
      tibble(estrato = paste0("Estandarizada por ", strata), con = NA_real_, sin = NA_real_,
             n_con = sum(valid$n_con), n_sin = sum(valid$n_sin),
             diferencia = if (nrow(valid) > 0) weighted.mean(valid$con - valid$sin, valid$n_con + valid$n_sin) else NA_real_)
    )
  } else {
    out <- out %>% mutate(diferencia = con - sin)
  }
  out %>% mutate(celda_menor_30 = n_con < 30 | n_sin < 30)
}


## 7. Memo, media UE declarada y auxiliares -----------------------------------------------------------------------

# Nuevo: evalúa expr una vez por clave y la guarda en .sr$memo. La clave lleva ctx$meta$ctx_key (= cache/ctx_<slug>.key:
# .dta, regions.csv, código, eu_values, slug y país; si falta, meta$key), ctx$country y nrow(ctx$national): un ctx de
# otro país o con national = subgrupo no recibe los resultados de otro (antes meta$key no distinguía el país). Un
# national modificado con las mismas filas sí comparte clave: vacíe .sr$memo. load_ctx() lo vacía. NULL no se guarda.
memo <- function(ctx, key, expr) {
  k <- paste(ctx$meta$ctx_key %||% ctx$meta$key %||% "sin-clave", ctx$country %||% "", nrow(ctx$national %||% data.frame()),
             key, sep = "|")
  if (!is.list(.sr$memo)) .sr$memo <- list()
  if (!is.null(.sr$memo[[k]])) return(.sr$memo[[k]])
  val <- expr
  .sr$memo[[k]] <- val
  val
}

# Nuevo: media UE que declara cada figura (metodo_ue de su spec). source = "data4web": escalar de
# ctx$eu_values (reference/<slug>/eu_values.csv, escrito por tools/build_reference.R; sin transform). source = "eu_est":
# eu_est(ctx, id, transform) sobre ctx$master, que debe caer en 0-100: fuera de ese rango la variable no es binaria y
# falta transform (p. ej. binary_agree; para escalas use eu_est() directamente). Errores legibles si el id no está, si
# está sin valor (NA) o si ningún país tiene dato.
eu_value <- function(ctx, source = "data4web", id, transform = NULL) {
  source <- match.arg(source, c("data4web", "eu_est"))
  if (source == "eu_est") {
    v <- eu_est(ctx, id, transform)
    if (!is.finite(v)) stop(sprintf("Media UE '%s' (eu_est): ningún país de ctx$master tiene dato", id), call. = FALSE)
    if (v < 0 || v > 100) {
      stop(sprintf("Media UE '%s' (eu_est) = %.1f, fuera de 0-100: la variable no es binaria; pase transform (p. ej. binary_agree)",
                   id, v), call. = FALSE)
    }
    return(v)
  }
  if (!is.null(transform)) stop("eu_value: transform solo se usa con source = 'eu_est' (data4web ya trae la media)", call. = FALSE)
  v <- ctx$eu_values
  rows <- if (is.data.frame(v) && all(c("id", "value") %in% names(v))) v$value[!is.na(v$id) & v$id == id] else numeric()
  if (length(rows) == 0) {
    stop(sprintf("Media UE '%s' no está en reference/%s/eu_values.csv; declare el id en la columna eu_id del manifiesto y corra tools/build_reference.R", id, ctx$slug), call. = FALSE)
  }
  hit <- unique(as.numeric(rows[!is.na(rows)]))
  if (length(hit) == 0) {
    stop(sprintf("Media UE '%s' está en reference/%s/eu_values.csv pero sin valor (NA); revise tools/build_reference.R",
                 id, ctx$slug), call. = FALSE)
  }
  if (length(hit) > 1) {
    stop(sprintf("Media UE '%s' repetida con valores distintos en reference/%s/eu_values.csv", id, ctx$slug), call. = FALSE)
  }
  hit
}

# Añade a df las variables del recorrido (adv, drm y JOURNEY_VARS, de journey_vars()) que falten cuando alguna de las
# pedidas en target (uno o varios nombres: desenlace, exposición, estrato, grupo, by) es del recorrido; las columnas
# que df ya trae no se tocan. Antes solo miraba el desenlace y fallaba si la exposición o el grupo eran del recorrido.
with_journey <- function(df, target) {
  miss <- setdiff(intersect(target, c("adv", "drm", JOURNEY_VARS)), names(df))
  if (length(miss) == 0) return(df)
  df[miss] <- journey_vars(df)[miss]
  df
}

# Identificador de una función de transformación para las claves de memo ("" si NULL): su texto y, si es una closure
# (entorno que no es el global ni un namespace), la dirección de su entorno: dos closures de una misma fábrica tienen el
# mismo texto. El entorno se guarda en .sr$fn_envs para que R no reutilice su dirección tras un gc() mientras viva la
# entrada de memo (load_ctx() vacía ambos).
fn_id <- function(f) {
  if (is.null(f)) return("")
  id <- paste(trimws(deparse(f)), collapse = " ")
  env <- environment(f)
  if (is.environment(env) && !identical(env, globalenv()) && !isNamespace(env)) {
    if (!is.list(.sr$fn_envs)) .sr$fn_envs <- list()
    .sr$fn_envs[[format(env)]] <- env
    id <- paste(id, format(env))
  }
  id
}


## Prueba ---------------------------------------------------------------------------------------------------------
## Comando: cd /tmp && Rscript --vanilla SCRATCH/agente_B/prueba.R (carga los R/0N_*.R existentes en orden; ctx de prueba
## con REPO/data/master.csv [11346 x 200, 4 países], load_regions(cfg), eu_values = tibble(id = "access2info", value = 61),
## meta$key = "prueba"; master.csv no trae non_trivial_problem_broader y la prueba la añade como 01_load.R:86-90).
## Resultado (2026-10-22): parse ok; carga tras 00_setup.R y 01_load.R sin error; 12/12 comprobaciones ok:
##  - nat_est(ctx, "access2info")$value = 61.46 (n = 1176); nat_est(ctx, "inside_justice_gap_keepdk")$value = 47.84;
##    by = "gender" → 2 filas (Male 60.50); df = ctx$master → una fila por país (4).
##  - journey(ctx): 26 nombres (21 JOURNEY_VARS + no_need_r1, unknown_r1, drm_no_need_r1, drm_unknown_r1, n);
##    adv_contacted = 29.17, rep_c_published = 31.93, drm_c_published = 28.47; round(no_need_r1 + unknown_r1) = 33
##    (fig 11) y round(drm_no_need_r1 + drm_unknown_r1) = 56 (fig 16); segunda llamada idéntica desde .sr$memo.
##  - get_results_table(ctx, "access2info", "full"): 29 filas, grouping factor con los 7 clabels, primera fila
##    National / National Average / 61.46; frente a finales/datos/Figure_09_access_information.csv 28 de 29 etiquetas
##    con el mismo valor (±0.01; 4 N/A = 4 NA: Law Enforcement, Housing, Education, Citizenship & ID); la 29.ª,
##    "6 or more problems" (56.22), en master.csv aún se llama "5 or more problems" (56.22; corrección de
##    special-vars.R:360 en 02_variables.R). omit = "Category" → 17 filas; "short" con prop = FALSE → 0.6146; "trust"
##    con transform = binary_agree → 17 filas (National 27.19); "custom_1" → 17 filas; ctype desconocido → error legible.
##  - perception_experience(ctx, "JSE_access2info", "access2info"): 13 filas x 6 columnas; National = 27.19 / 61.46
##    (n_p 897, n_e 1176); exp = "rep_c_published" (vía with_journey) → 31.93.
##  - eu_est(ctx, "access2rep") = 41.91 con los 4 países de master.csv (config espera 37.68 con los 27); memo idéntico;
##    eu_value(ctx, "data4web", "access2info") = 61; id ausente → "Media UE 'zzz' no está en reference/hungary/...".
##  - xt → columnas originales (pct Male 60.5); diff_stratified(strata = "residence") → 4 filas; nuts_mean_long sobre
##    AJP_*_sev en largo → est 5.552 (n 8287), por grupo 12 filas.
##  - data_checks(ctx, cfg) con este ctx: 4/5 (difiere eu_est con 4 países; esperable; ncol_master_csv = 200 ok).
## Correcciones de la revisión (2026-10-23): cd /tmp && Rscript --vanilla G2/pruebas/t03_memo.R, t06_eu_value.R y
## t07_with_journey.R (G2 = SCRATCH/fix/G2; ctx reales de cache/ y una copia con config/poland.yml). Antes → ahora:
##  - memo(): con el ctx del reporte y uno de Poland en la misma sesión, journey(ctx_pl)$adv_contacted daba 29.17 (el del
##    reporte); ahora 30.22 = nat_est sin memo, y el del reporte sigue en 29.17. Con national = mujeres,
##    journey(sub)$rep_c_published daba 31.93 (el nacional); ahora 30.83 = nat_est del subgrupo.
##  - fn_id(): eu_est(ctx, "nproblems", thr(4)) tras thr(2) (closures de una misma fábrica) daba 32.43; ahora 16.12.
##    200 closures → 200 huellas; una función del global no lleva dirección.
##  - eu_value(): data4web igual (10 ids: 49 61 40 74 54 82 72 57 57 52). eu_est sin transform daba 1093.2 para
##    AJE_legalrights; ahora error legible (fuera de 0-100) y con binary_agree 72.6. Id con value NA: el mensaje decía que
##    no estaba; ahora "está … pero sin valor (NA)".
##  - with_journey(): sobre master.csv (sin variables del recorrido), xt con grupo adv, diff_stratified con exposición
##    rep_c_published o estrato drm y nat_est(by = "drm") daban error; ahora dan lo mismo que con df = journey_vars(df).
##    Con el ctx real, xt, diff_stratified, get_results_table y perception_experience son idénticos a los de antes.



## Problemas en formato largo (figuras 3 y 7) ------------------------------------------------------------------

# origen: src/hungary/F02.R:3-12 (problems_long, literal): una fila por problema declarado (AJP_<código>_bin == 1) con
# su severidad 0-10 (98/99 fuera) y su categoría (problem_map, 02_variables.R); extra = columna del encuestado que se
# arrastra (p. ej. residence). La usan fig_03 (severidad media por categoría × área) y fig_07 (distribución de la
# severidad). Firma (df, extra) del original, sin ctx: no pondera ni usa regions, solo cambia la forma del df que
# recibe (ctx$national en la 3, ctx$master filtrado a los países en la 7); pondera después nuts_mean_long(ctx, …).
problems_long <- function(df, extra = NULL) {
  map_dfr(names(problem_map), function(code) {
    b <- df[[paste0("AJP_", code, "_bin")]]; s <- df[[paste0("AJP_", code, "_sev")]]
    keep <- !is.na(b) & b == 1 & !is.na(s) & s <= 10
    out <- tibble(country_name_ltn = df$country_name_ltn, nuts_id = df$nuts_id, sev = s, category = problem_map[[code]])
    if (!is.null(extra)) out[[extra]] <- as.character(df[[extra]])
    out[keep, ]
  })
}

## Coocurrencia de problemas (figuras 5 y 6) ----------------------------------------------------------------------

# origen: F01.R:6-7 — adaptada: `hu` → ctx$national. Una fila por encuestado del país: country_name_ltn, nuts_id y una
# columna 0/1 por categoría (`categories`, orden alfabético) = al menos un problema no trivial en ella (AJP_*_bin = 1 y
# 4 <= sev < 98: problem_flags() de 02_variables.R, quality check ya aplicado). La 6 la estima con nat_est (P(fila |
# columna), F01.R:8-19); la 5 la cuenta sin ponderar. Es la misma bandera que la red obtiene recodificando AJP_*_bin
# (F01.R:46-50: 1 solo si es no trivial) y agregando por categoría (network_graph.R:35-56): las 12 columnas son
# idénticas en el país del reporte (1750 encuestados; comprobado el 2026-10-23). Memorizada por ctx (la usan la 5 y la 6).
cooc_flags <- function(ctx) {
  memo(ctx, "cooc_flags", {
    nt <- problem_flags(ctx$national)
    bind_cols(ctx$national %>% select(country_name_ltn, nuts_id), nt %>% mutate(across(everything(), as.numeric)))
  })
}
## Prueba (2026-10-23): cd /tmp && Rscript SCRATCH/e3_L3/pruebas/p01_explorar.R y p05_contraste.R (ctx real de cache/):
## cooc_flags(ctx) 1750 × 14; sus 12 columnas = recodificación F01.R:46-50 + network_graph.R:35-56 (identical 12/12);
## fig 6 = analisis adicionales/F01.csv (144 celdas, |Δ| ≤ 7e-15, mismas N/A); fig 5: lifts = tabs/csv/
## 1_5_lift_matrix_<país>.csv (66 parejas, |Δ| ≤ 2e-15) y prevalencias = P_A/P_B de 1_5_lift_top_pairs.csv.
## Revisión 3 (grupo F2, 2026-10-23): solo comentarios (encabezado y estas dos secciones); problems_long y cooc_flags
## sin cambios de código (las tablas de 3, 5, 6 y 7 del país del reporte son las de antes).

## Barras horizontales: a quién se acudió y motivos para no acudir (figuras 13, 14, 18, 19) ----------------------
## Las cuatro comparten el estimador del original, que no es get_nuts_avg/get_wgt_national_avg: por región NUTS, casos /
## encuestados de la región × regionpoppct, sumado sobre las regiones del país × 100, SIN renormalizar a las regiones
## con dato y SIN suprimir n < 30. 13 y 18 (contactados) comparten la agregación; 14 y 19 (motivos) toda la función.

# origen: module_2_2.R:235-263 (contacted_advisers, fig 13) = module_2_3.R:203-231 (contacted_mechanisms, fig 18), la
# agregación ponderada, literal salvo lo marcado. df_full = una fila por encuestado del país con country_name_ltn,
# nuts_id, none (lógico: no acudió a nadie) y las columnas cols (lógicas: acudió o no a cada uno), ya recodificadas por
# la figura (la recodificación difiere entre 13 y 18 y vive en su archivo). Devuelve tibble(category = cols, value) en
# el orden de cols (value % 0-100, sin redondear). Un NA en cols (código fuera de la recodificación) da NA en su región
# y en el país, como sum(x) del original. adaptado: regions → ctx$regions; across(everything()) y
# starts_with("AJD_adviser_"/"AJR_") → all_of(cols) (mismas columnas); sin pivot a la columna del país (country_name_ltn
# = "national_avg"): una sola columna value.
contacted_shares <- function(ctx, df_full, cols) {
  regions <- ctx$regions
  out <- df_full %>%
    group_by(country_name_ltn, nuts_id) %>%
    filter(none == FALSE) %>%
    summarise(across(all_of(cols), \(x) sum(x)), nuts_size = n(), .groups = "keep") %>%
    left_join(regions %>% select(nuts_id, wgt = regionpoppct), by = "nuts_id") %>%
    mutate(across(all_of(cols), \(x) (x/nuts_size)*wgt)) %>%
    group_by(country_name_ltn) %>%
    summarise(across(all_of(cols), \(x) sum(x)*100)) %>%
    pivot_longer(!country_name_ltn, names_to = "category", values_to = "value") %>%
    select(category, value)
  # nuevo: solo agregados al log (base, base con none == FALSE, regiones; los valores los registra rows_hbars)
  log_msg("contacted_shares: base ", nrow(df_full), " encuestados · ", sum(df_full$none %in% FALSE), " con none == FALSE · ",
          n_distinct(df_full$nuts_id), " regiones NUTS")
  out
}

# origen: module_2_2.R:303-348 (access2representation_barriers, fig 14) = module_2_3.R:300-346 (access2DRM_barriers,
# fig 19), literal salvo lo marcado. Motivo `target` (código) entre los encuestados con `filter_var` == 0 y motivo con
# etiqueta; recode = el case_when de códigos a etiquetas de cada figura (función; NA = sin etiqueta → fuera). Por NUTS:
# casos del motivo / encuestados de la región × regionpoppct; país = suma × 100 (na.rm, sin renormalizar ni n < 30).
# Devuelve tibble(category, value) en el orden de summarise (etiqueta, locale C). adaptado: df por defecto
# ctx$national (el original corría sobre los 27 países y filtraba después), regions → ctx$regions; sin write_csv
# (2_x_5_*.csv), sin kable y sin pivot_wider a la columna del país.
reason_shares <- function(ctx, target, filter_var, recode, df = NULL) {
  if (is.null(df)) df <- ctx$national
  regions <- ctx$regions
  base <- df %>%
    select(country_name_ltn, nuts_id, target = all_of(target), filtering = all_of(filter_var)) %>%
    mutate(target = recode(target)) %>%
    filter(filtering == 0 & !is.na(target))
  # nuevo: solo agregados al log (base y motivos presentes; los valores los registra rows_hbars)
  log_msg("reason_shares(", target, " entre ", filter_var, " == 0): base ", nrow(base), " encuestados · ",
          n_distinct(base$target), " motivos · ", n_distinct(base$nuts_id), " regiones NUTS")
  base %>%
    group_by(country_name_ltn, nuts_id) %>%
    mutate(total_sample = n()) %>%
    group_by(country_name_ltn, nuts_id, target) %>%
    summarise(reason_count = n(), total_sample = first(total_sample), .groups = "keep") %>%
    left_join(regions %>% select(nuts_id, weight = regionpoppct), by = "nuts_id") %>%
    mutate(proportion_weighted = (reason_count/total_sample)*weight) %>%
    group_by(country_name_ltn, target) %>%
    summarise(proportion_national = sum(proportion_weighted, na.rm = TRUE)*100, .groups = "keep") %>%
    ungroup() %>%
    select(category = target, value = proportion_national)
}

# rows_hbars() (R/05_charts.R) arma el df de las barras horizontales desde contacted_shares() y reason_shares().
## Prueba (2026-10-28, copia fix5/L2): cd /private/tmp && Rscript L2/pruebas/p01_explorar.R (PR_COUNTRY = hungary,
## poland, malta; ctx de cache/; solo agregados) y main.R --country <país> --stage all --figs 13,14,18,19 [--strict]
## con WJPr 1.1.0 (SRC_REPORTS_LIB) y 1.2.0: país del reporte 4/4 ok en verify en las dos builds (etiquetas, textos y
## orden = reference/hungary; max |Δ| 0.0013 en la 13, 0.0019 en la 18, < 0.0001 en 14 y 19); los valores de control
## del plan (13: 54.32/22.26/21.15; 18: 59.75/56.03/51.19) salen en la línea rows_hbars del log. across() del mutate de
## la 13 ve none creada en ese mismo mutate (TRUE → TRUE, NA → FALSE): sin eso filter(none == FALSE) vaciaría la tabla.
## Sin códigos fuera de la recodificación (ningún NA) en el país del reporte, Poland ni Malta (configs de prueba):
## figures ok, verify «sin referencia»; Malta da 7 filas en la 14 (sin el código 9). Nada de lo existente cambió.

# Nuevo (lote 20-27, 2026-10-29): ¿muestra pequeña? Clave small_sample de config/<pais>.yml (FALSE si falta), como
# country_config[[country]]$small_sample de src/viz/worker-viz.R:166 y siguientes. Cada figura decide qué grupos quita
# entonces (omit de get_results_table), con las listas de worker-viz.R: 9 y 12 Category (:166-167, 236-237); 17 y
# 20-25 además Age Group (:340-341, 417-584); 26 Age Group y Problem Status (:616-617). Con menos filas, el h_in del
# país va en manifest/figures_<slug>.csv (regla de CLAUDE.md, «Fijar h_in»).
small_sample <- function(ctx) isTRUE(ctx$cfg$small_sample)

# Nuevo (lote 38-42, 2026-10-29): confianza por escalera (figuras 41 y 42; A04.R:18-21, A05.R:26-28). Por actor
# (TRUST_ACTORS: trust_TRT_* de add_report_vars = binary_agree de TRT_*), xt() sobre todos los encuestados del país
# (como build_analysis_frame) por el corte `cut` (exposicion o escalera_hardship), en el orden `levels`. Devuelve una
# matriz levels × actores (% sin redondear). trust_cards_scale(): la escala de color común a las dos figuras, rango de
# los valores redondeados de ambas (lo publicado: 29/55).
TRUST_ACTORS <- c("Judges" = "trust_TRT_judges", "Prosecutors" = "trust_TRT_prosecutors",
                  "Public defense attorneys" = "trust_TRT_pda")
# origen: standard_figures.R:195, 200 (orden publicado de las filas; no el de ladder_levels)
TRUST_LADDERS <- list(
  exposicion = c("No non-trivial problem", "Problem, did not need a DRM", "Needed a DRM, had access", "Needed a DRM, no access"),
  escalera_hardship = c("No non-trivial problem", "Problem, no hardship", "Problem, with hardship")
)
trust_ladder <- function(ctx, cut) {
  levels <- TRUST_LADDERS[[cut]]
  if (is.null(levels)) stop("trust_ladder: corte desconocido: ", cut, call. = FALSE)
  t <- imap_dfr(TRUST_ACTORS, \(v, lab) xt(ctx, v, cut, lab, cut))
  m <- vapply(names(TRUST_ACTORS), \(a) t$pct[t$desenlace == a][match(levels, t$grupo[t$desenlace == a])], numeric(length(levels)))
  matrix(m, nrow = length(levels), dimnames = list(levels, names(TRUST_ACTORS)))
}
trust_cards_scale <- function(ctx) {
  range(round(c(trust_ladder(ctx, "exposicion"), trust_ladder(ctx, "escalera_hardship"))), na.rm = TRUE)
}
