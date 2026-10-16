## R01 · Denominadores de la prevalencia por categoría ----------------------------------------------------

run_R01 <- function() {

  nt  <- problem_flags(hu)
  any <- problem_flags(hu, any_severity = TRUE)
  has_nt <- hu$prevalence2 %in% 1

  # NUTS-weighted share of a logical vector over a subset of respondents (Fig. 2 method: numerator =
  # respondents with the category, denominator = every respondent in the subset of the region).
  nuts_share <- function(flag, keep) {
    tibble(nuts_id = hu$nuts_id, x = as.numeric(flag))[keep, ] %>%
      group_by(nuts_id) %>%
      summarise(p = mean(x), n = n(), .groups = "drop") %>%
      left_join(regions %>% select(nuts_id, regionpoppct), by = "nuts_id") %>%
      summarise(pct = 100 * sum(p * regionpoppct) / sum(regionpoppct), n = sum(n)) %>%
      pull(pct)
  }
  all_rows <- rep(TRUE, nrow(hu))

  published_fig2 <- c("Public Services" = 40, "Consumer" = 40, "Community" = 28, "Money & Debt" = 22,
                      "Accidental" = 21, "Employment" = 19, "Land" = 18, "Family" = 18, "Housing" = 10,
                      "Education" = 9, "Citizenship & ID" = 5, "Law Enforcement" = 2)
  published_fig4 <- c("Consumer" = 62, "Public Services" = 49, "Community" = 43, "Money & Debt" = 28,
                      "Accidental" = 27, "Land" = 27, "Employment" = 25, "Family" = 25, "Housing" = 16,
                      "Education" = 12, "Citizenship & ID" = 6, "Law Enforcement" = 3)

  pipeline_cat <- prevalence_category_country_indicators(hu, regions, "1", "x", "x") %>%
    mutate(category = if_else(category == "Citinzenship & ID", "Citizenship & ID", category)) %>%
    { setNames(.$pct, .$category) }

  tbl <- tibble(category = categories) %>%
    mutate(
      publicado_fig2 = published_fig2[category],
      publicado_fig4 = published_fig4[category],
      # Same routine as module_1.R / eu_main_indicators.R, so the value matches F04 and the pipeline CSV
      nt_toda_muestra = pipeline_cat[category],
      nt_entre_con_problema_nt = map_dbl(category, \(k) nuts_share(nt[[k]], has_nt)),
      cualquier_sev_toda_muestra = map_dbl(category, \(k) nuts_share(any[[k]], all_rows)),
      n_toda_muestra = nrow(hu),
      n_con_problema_nt = sum(has_nt)
    ) %>%
    arrange(desc(nt_toda_muestra))

  save_table(tbl, "R01")

  fig2_ok <- all(abs(round(tbl$nt_toda_muestra) - tbl$publicado_fig2) <= 1)
  fig4_ok <- all(abs(round(tbl$cualquier_sev_toda_muestra) - tbl$publicado_fig4) <= 1)
  fig4_alt <- all(abs(round(tbl$nt_entre_con_problema_nt) - tbl$publicado_fig4) <= 1)

  save_note(
    "R01",
    "# R01 · Denominadores de la prevalencia por categoría",
    "",
    "**Población:** Hungría, 1.750 entrevistados (1.749 con módulo LNS). Ponderación NUTS (método del pipeline, `module_1.R`).",
    "",
    md_table(tbl %>% select(-n_toda_muestra, -n_con_problema_nt)),
    "",
    "## Qué reproduce cada figura",
    "",
    paste0("- **Figura 2** = problemas **no triviales** (severidad 4-10) sobre **toda la muestra**. Reproduce: ", ifelse(fig2_ok, "sí", "no"), "."),
    paste0("- **Figura 4** (etiquetas de los nodos) = problemas de **cualquier severidad** sobre **toda la muestra**. Reproduce: ", ifelse(fig4_ok, "sí", "no"),
           ". La hipótesis de la planilla (no triviales entre quienes tuvieron un problema no trivial) ", ifelse(fig4_alt, "también reproduce", "NO reproduce"), " la Figura 4."),
    "",
    "## Por qué Public Services y Money & Debt no seguían la razón 1/0,68",
    "",
    "La razón ~1/0,68 entre las dos figuras era una coincidencia: la Figura 4 no cambia de denominador, cambia de **severidad**.",
    "`src/viz/network_graph.R` marca una categoría con cualquier `AJP_*_bin == 1`, sin filtrar por severidad. Las categorías con muchos problemas",
    "de baja severidad (Public Services, Money & Debt) se alejan de la razón; las demás caen cerca por azar.",
    "",
    "## Consecuencias",
    "",
    "- El pie de la Figura 4 («non-trivial … among respondents who experienced at least one non-trivial problem») es incorrecto en los dos términos para el dibujo publicado.",
    "- Los lifts del texto de §1.2 (11,8; 5,4; 4,8…) sí son de problemas no triviales (`lift_analysis()` en `module_1.R`); el dibujo no. La Figura 4 corregida (`cooccurrence_network.svg` en esta carpeta) usa problemas no triviales, de modo que dibujo, pie y texto coinciden.",
    "- El texto de §1.1 y del resumen ejecutivo («40% of respondents **with non-trivial problems**») debe decir «of respondents»: es la base de toda la muestra."
  )
  invisible(tbl)
}
