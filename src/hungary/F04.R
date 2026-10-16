## F04 · Prevalencia por categoría, Hungría contra la UE --------------------------------------------------

run_F04 <- function() {

  lns_countries <- eu %>% filter(!is.na(prevalence2)) %>% distinct(country_name_ltn) %>% pull()
  eu_cat <- prevalence_category_country_indicators(eu, regions, "1", "x", "x") %>%
    mutate(category = if_else(category == "Citinzenship & ID", "Citizenship & ID", category),
           pct = if_else(country %in% lns_countries, pct, NA_real_))   # Ireland: NA (no LNS module)
  hu_cat <- eu_cat %>% filter(country == HU) %>% select(category, hu = pct)
  eu_avg <- eu_cat %>% group_by(category) %>%
    summarise(eu = mean(pct, na.rm = TRUE), n_countries = sum(!is.na(pct)), .groups = "drop")

  tbl <- hu_cat %>%
    left_join(eu_avg, by = "category") %>%
    mutate(diferencia = hu - eu) %>%
    arrange(desc(diferencia)) %>%
    mutate(aporte_al_exceso = diferencia / sum(diferencia) * 100)
  save_table(tbl, "F04")

  # Correction of Figure 2 (same file name): Hungary and EU Average per category, report order
  paired_bars(tbl %>% arrange(desc(hu)), "Hungary_prevalence_nontrivial_problems_by_category")
  file.remove(file.path(OUT, "Hungary_prevalence_nontrivial_problems_by_category.png"))

  top3 <- tbl %>% slice_head(n = 3)
  save_note(
    "F04",
    "# F04 · Prevalencia por categoría, Hungría contra la UE",
    "",
    "**Población:** toda la muestra de cada país; categoría = al menos un problema no trivial (severidad 4-10) en ella (mismo método y denominador que la Figura 2, `module_1.R`). **Ponderación del reporte.** UE: promedio simple de los promedios nacionales de los 26 países con módulo LNS (Irlanda = NA).",
    "",
    md_table(tbl),
    "",
    "## Lectura",
    "",
    paste0("Hungría está por encima de la UE en las 12 categorías, pero el exceso se concentra: ", paste(top3$category, collapse = ", "),
           " suman ", round(sum(top3$aporte_al_exceso)), " % de la suma de diferencias. La suma de diferencias no es igual a los 19 puntos de prevalencia general porque una persona puede tener problemas en varias categorías."),
    "",
    "## Archivos",
    "",
    "- `Hungary_prevalence_nontrivial_problems_by_category.svg` — CORRECCIÓN de la Figura 2 con la media UE. La tabla de arriba (F04.csv) trae la diferencia Hungría − UE por categoría."
  )
  invisible(tbl)
}
