## R02 · Sensibilidad de la prevalencia al umbral de severidad -------------------------------------------

prevalence_at <- function(df, min_sev) {
  flags <- problem_flags(df, min_sev = min_sev)
  df %>%
    mutate(prev_t = if_else(has_lns(df), as.numeric(rowSums(as.matrix(flags)) > 0), NA_real_))
}

run_R02 <- function() {

  thresholds <- c(4, 6, 8)

  tbl <- map_dfr(thresholds, function(t) {
    h <- nat_est(prevalence_at(hu, t), "prev_t")
    e <- eu_est(prevalence_at(eu, t), "prev_t")
    tibble(
      umbral = paste0(">= ", t),
      hungria = h$pct,
      n_hungria = h$n,
      media_ue = e$eu_pct,
      paises_ue = e$n_countries,
      brecha_hu_menos_ue = h$pct - e$eu_pct,
      razon_hu_ue = h$pct / e$eu_pct
    )
  })

  # Ranking of Hungary among EU countries at each threshold
  ranks <- map_dfr(thresholds, function(t) {
    nat_est(prevalence_at(eu, t), "prev_t") %>%
      filter(!is.na(pct)) %>%
      mutate(rank = rank(-pct)) %>%
      filter(country_name_ltn == HU) %>%
      transmute(umbral = paste0(">= ", t), ranking_hungria = rank, de = sum(!is.na(pct)))
  })
  n_countries <- nat_est(prevalence_at(eu, 4), "prev_t") %>% filter(!is.na(pct)) %>% nrow()
  tbl <- tbl %>% left_join(ranks %>% select(-de), by = "umbral") %>% mutate(paises_ranking = n_countries)

  save_table(tbl, "R02")

  holds <- all(tbl$brecha_hu_menos_ue > 5)
  save_note(
    "R02",
    "# R02 · Sensibilidad de la prevalencia al umbral de severidad",
    "",
    "**Población:** toda la muestra de cada país (entrevistados con módulo LNS). Hungría: ponderación NUTS. UE: promedio simple de los promedios nacionales ponderados (26 países con módulo LNS; Irlanda no lo tiene).",
    "Un entrevistado es prevalente con umbral *t* si reportó al menos un problema con severidad entre *t* y 10.",
    "",
    "**Dato previo:** la encuesta sí registró la severidad de los problemas por debajo de 4 (hay severidades 0-3 en los datos), así que el filtro se aplica en el análisis y no en campo.",
    "",
    md_table(tbl),
    "",
    "## Lectura",
    "",
    if (holds)
      "La brecha Hungría − UE **se mantiene** al subir el umbral: no es un efecto de anclaje alrededor del 4. El reporte debe decirlo explícitamente."
    else
      "La brecha Hungría − UE **se reduce sustancialmente** al subir el umbral: parte de la diferencia depende de cómo se califica la severidad. El reporte debe decirlo.",
    "",
    "Ver F03 para la distribución completa de severidad (Hungría contra UE)."
  )
  invisible(tbl)
}
