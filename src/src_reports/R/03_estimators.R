## 03_estimators.R — estimadores del reporte: promedio NUTS ponderado, tabla de resultados, media UE y recorrido -----
## Origen: src/utils/utils.R:19-150 (get_nuts_avg, get_wgt_national_avg; literales); utils.R:203-335 (get_results_table;
## adaptada a ctx); src/hungary/setup.R:125-166 (nat_est, eu_est, nuts_mean_long; adaptadas a ctx); src/hungary/R03.R:4-38
## (journey_vars; literal); src/hungary/F06.R:3-15 (perception_experience; adaptada a ctx); src/hungary/analysis_helpers.R:20-51
## (xt, xt_nat, diff_stratified; adaptadas a ctx). Nuevas: memo(), eu_value(), journey(), JOURNEY_VARS, with_journey(), fn_id().
## Adaptación común a ctx: `regions` global → ctx$regions; `hu`/`master` → df (por defecto ctx$national); `pct` → `value`;
## el país es ctx$country (sin study_countries). Sin efectos secundarios: nada escribe ni imprime (log_msg solo agregados).
## Usa de 02_variables.R: binary_agree() (perception_experience; se resuelve al llamar, no al cargar).
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
# (con journey_vars() si target es una variable del recorrido). Devuelve tibble(country_name_ltn, <by>, value, n): una
# fila por país presente en df (varios solo si se pasa df = ctx$master, como hace eu_est); by = NULL → una fila.
# n = encuestados con dato (sin ponderar, sin suprimir).
nat_est <- function(ctx, target, transform = NULL, by = NULL, df = NULL) {
  if (is.null(df)) df <- ctx$national
  df <- with_journey(df, target)
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
  df <- with_journey(df, exp)
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
  df <- with_journey(df, outcome)
  nat_est(ctx, outcome, by = ".g", df = df %>% mutate(.g = as.character(.data[[group]]))) %>%
    transmute(desenlace = label_outcome, corte = label_group, grupo = .g, pct = value, n, n_menor_30 = n < 30)
}
xt_nat <- xt

# origen: analysis_helpers.R:26-51 — adaptada: ctx (df por defecto ctx$national); pick("pct") → pick("value"). Desenlace
# por exposición binaria (1 frente a 0), crudo y por estratos; la diferencia estandarizada promedia las diferencias
# por estrato ponderadas por su tamaño.
diff_stratified <- function(ctx, outcome, exposure, strata = NULL, df = NULL) {
  if (is.null(df)) df <- ctx$national
  df <- with_journey(df, outcome)
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

# Nuevo: evalúa expr una vez por clave y la guarda en .sr$memo[[ctx$meta$key|key]] (load_ctx() vacía .sr$memo al
# reconstruir ctx; la clave del caché distingue .dta, regions.csv, código y país). Un resultado NULL no se guarda.
memo <- function(ctx, key, expr) {
  k <- paste(ctx$meta$key %||% "sin-clave", key, sep = "|")
  if (!is.list(.sr$memo)) .sr$memo <- list()
  if (!is.null(.sr$memo[[k]])) return(.sr$memo[[k]])
  val <- expr
  .sr$memo[[k]] <- val
  val
}

# Nuevo: media UE que declara cada figura (columna metodo_ue del manifiesto). source = "data4web": escalar de
# ctx$eu_values (reference/<slug>/eu_values.csv, escrito por tools/build_reference.R); source = "eu_est": eu_est(ctx, id)
# sobre ctx$master. Error legible si el id no está.
eu_value <- function(ctx, source = "data4web", id) {
  source <- match.arg(source, c("data4web", "eu_est"))
  if (source == "eu_est") return(eu_est(ctx, id))
  v <- ctx$eu_values
  hit <- if (is.data.frame(v) && all(c("id", "value") %in% names(v))) v$value[!is.na(v$id) & v$id == id] else numeric()
  hit <- unique(as.numeric(hit[!is.na(hit)]))
  if (length(hit) == 0) {
    stop(sprintf("Media UE '%s' no está en reference/%s/eu_values.csv; corra tools/build_reference.R", id, ctx$slug), call. = FALSE)
  }
  if (length(hit) > 1) {
    stop(sprintf("Media UE '%s' repetida con valores distintos en reference/%s/eu_values.csv", id, ctx$slug), call. = FALSE)
  }
  hit
}

# Añade las variables del recorrido cuando target es una de JOURNEY_VARS y df no la trae
with_journey <- function(df, target) {
  if (!target %in% names(df) && target %in% JOURNEY_VARS) journey_vars(df) else df
}

# Identificador textual de una función de transformación para las claves de memo ("" si NULL)
fn_id <- function(f) if (is.null(f)) "" else paste(trimws(deparse(f)), collapse = " ")


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
##  - data_checks(ctx, cfg) con este ctx: 3/5 (difieren ncol_master_csv = 200 ≠ 199 y eu_est con 4 países; esperable).
