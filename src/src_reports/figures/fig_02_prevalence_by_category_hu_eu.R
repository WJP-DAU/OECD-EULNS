## Figura 02 · Prevalence of Non-Trivial Justice Problems in {country}, by Problem Category
## Fuente de datos: prevalence_category_country_indicators (eu_main_indicators.R:91-205; sin n<30 ni renormalización;
##   flag → 0; 'Citinzenship' corregido) del país vs media simple de los países con módulo LNS (F04.R:5-11: 26;
##   Irlanda NA); orden desc(país) (F04.R:21)
## Método UE: eu_est_category
## Código de origen: report_charts.R:150 chart_paired ← viz_helpers.R:126 paired_bars (F04.R:21)

# origen: eu_main_indicators.R:91-205 (prevalence_category_country_indicators), compactada sin cambiar la aritmética
# (comprobado: idéntica, ==, a la original en 27 países x 12 categorías). Devuelve tibble(country_name_ltn, category,
# pct): % de TODA la muestra con algún problema no trivial de la categoría. Los 38 problemas (:92-100) y su agrupación
# (:145-160) son los de problem_map (02_variables.R), la misma tabla.
# corregido: "Citinzenship & ID" (:192) → "Citizenship & ID", como F04.R:7 (problem_map ya la trae así).
prev_category_by_country <- function(master, regions) {
  codes <- names(problem_map)
  bin <- as.matrix(master[paste0("AJP_", codes, "_bin")])
  sev <- as.matrix(master[paste0("AJP_", codes, "_sev")])
  # origen: :102-133. Se recalcula el quality check (> 25 problemas con bin == 1 y 4 <= sev < 98); sobre el master
  # ya depurado (marcados: bin = 2, severidades NA) no marca a nadie y esos encuestados cuentan 0 por sus NA
  flag <- rowSums(bin == 1 & sev >= 4 & sev < 98, na.rm = TRUE) > 25
  # origen: :135-144. Severidad 4-97 → 1; 0-3 → 0; 98, 99 o NA → NA; marcados → 0
  nt <- ifelse(sev > 3 & sev < 98, 1, ifelse(sev < 4, 0, NA_real_))
  nt[flag, ] <- 0
  colnames(nt) <- codes
  # origen: :145-160. Categoría = if_else(s1 == 1 | s2 == 1 | ..., 1, 0): 1 si algún problema es 1; NA si ninguno
  # lo es y alguno es NA; 0 si no
  cats <- vapply(categories, function(k) {
    if_else(Reduce(`|`, lapply(codes[problem_map == k], function(cd) nt[, cd] == 1)), 1, 0)
  }, numeric(nrow(nt)))
  # origen: :162-181. Por región NUTS: casos / n() de toda la muestra (NA cuenta 0) x regionpoppct, sin supresión
  # n < 30; suma por país x 100, sin renormalizar: el valor del país sale multiplicado por la suma de regionpoppct de
  # sus regiones con muestra (una región sin encuestados o sin peso queda fuera). Esa suma es 1 ± 0.002 en 26 países
  # y 0.968 en Francia (regiones con peso en regions.csv que no tienen encuestados): data() la registra en el log
  bind_cols(master %>% select(country_name_ltn, nuts_id), as_tibble(cats)) %>%
    left_join(regions %>% select(country, nuts_id, regionpoppct), by = c("country_name_ltn" = "country", "nuts_id")) %>%
    group_by(country_name_ltn, nuts_id) %>%
    summarise(total_sample = n(), across(all_of(categories), ~ sum(.x, na.rm = TRUE) / total_sample),
              pop_weight = mean(regionpoppct, na.rm = TRUE), .groups = "drop") %>%
    mutate(across(all_of(categories), ~ .x * pop_weight)) %>%
    group_by(country_name_ltn) %>%
    summarise(across(all_of(categories), ~ sum(.x, na.rm = TRUE) * 100), .groups = "drop") %>%
    pivot_longer(all_of(categories), names_to = "category", values_to = "pct")
}

fig_02 <- list(

  # País y media UE por categoría. origen: F04.R:5-11: los países sin módulo LNS (ningún prevalence2: Irlanda) → NA
  # y la media UE es la media simple de los que tienen dato (cuántos, al log). Orden: % del país descendente
  # (F04.R:21; empates en desc(diferencia), F04.R:16). value/value2 sin redondear; text/text2 = round() + "%"
  # (viz_helpers.R:132), vía fmt_num()
  data = function(ctx) {
    m <- ctx$master %>% filter(country_name_ltn %in% (ctx$meta$countries %||% unique(ctx$master$country_name_ltn)))
    lns <- m %>% filter(!is.na(prevalence2)) %>% distinct(country_name_ltn) %>% pull()
    est <- prev_category_by_country(m, ctx$regions) %>%
      mutate(pct = if_else(country_name_ltn %in% lns, pct, NA_real_))
    eu <- est %>% group_by(category) %>%
      summarise(eu = mean(pct, na.rm = TRUE), n_countries = sum(!is.na(pct)), .groups = "drop")
    log_msg("fig 02: media UE = media simple de ", paste(unique(range(eu$n_countries)), collapse = "-"),
            " países con módulo LNS de ", n_distinct(m$country_name_ltn), " (sin módulo: ",
            paste(setdiff(sort(unique(m$country_name_ltn)), lns), collapse = ", "), ")")
    # corregido: la suma de regionpoppct de las regiones con muestra de cada país, que escala sus valores (el método
    # publicado no renormaliza y no cambia), va al log, con aviso si la del país se aleja de 1 en más de 0.01: las
    # demás figuras renormalizan al promedio NUTS de las regiones con dato y esta saldría desalineada sin que nada lo
    # dijera (Francia: 0.968, −3.2 % relativo; revisión V4-07)
    w <- m %>% distinct(country_name_ltn, nuts_id) %>%
      left_join(ctx$regions %>% select(country, nuts_id, regionpoppct),
                by = c("country_name_ltn" = "country", "nuts_id")) %>%
      group_by(country_name_ltn) %>% summarise(w = sum(regionpoppct, na.rm = TRUE), .groups = "drop")
    w_pais <- w$w[w$country_name_ltn == ctx$country]
    lejos <- w %>% filter(abs(w - 1) > 0.01)
    log_msg("fig 02: suma de regionpoppct de las regiones con muestra: ", ctx$country, " ", sprintf("%.3f", w_pais),
            "; ", nrow(w), " países entre ", sprintf("%.3f", min(w$w)), " y ", sprintf("%.3f", max(w$w)),
            if (nrow(lejos) > 0) {
              paste0("; lejos de 1 (> 0.01): ",
                     paste(sprintf("%s %.3f", lejos$country_name_ltn, lejos$w), collapse = ", "))
            })
    if (length(w_pais) == 1 && abs(w_pais - 1) > 0.01) {
      log_msg("fig 02 aviso: las regiones con muestra de ", ctx$country, " suman un peso de ", sprintf("%.3f", w_pais),
              ": sus valores salen escalados por esa suma (sin renormalizar, como lo publicado: ",
              "eu_main_indicators.R:162-181)")
    }
    est %>% filter(country_name_ltn == ctx$country) %>% select(category, pais = pct) %>%
      left_join(eu, by = "category") %>%
      arrange(desc(pais), desc(pais - eu)) %>%
      transmute(order = row_number(), kind = "row", grouping = "", label = category, value = pais, value2 = eu,
                text = fmt_num(pais, 0), text2 = fmt_num(eu, 0))
  },

  # Dibujo: WJPr::wjp_bars() de WJPr 1.2.0 con sus argumentos, sin complementos (wjpr_paired_bars, R/09_wjpr.R;
  # metodo wjpr; revisión 5): país y UE lado a lado, con la leyenda {country} | EU Average de spec$country. Respaldo svg_r: chart_paired() de R/05_charts.R (pendiente).
  plot = function(df, spec) wjpr_paired_bars(df, spec)
)
