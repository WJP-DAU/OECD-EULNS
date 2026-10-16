## F07 · Hardship: «al menos uno», desgloses y media UE en la brecha de justicia ---------------------------

run_F07 <- function() {

  hardship_vars <- c("Health issues" = "hardships_health", "Economic issues" = "hardships_income",
                     "Relationship breakdown" = "hardships_emotional", "Alcohol/Drug problems" = "hardships_drugs")
  files <- c("hardship" = "F07_hardship_al_menos_uno", "hardships_health" = "F07_hardship_salud",
             "hardships_income" = "F07_hardship_economico", "hardships_emotional" = "F07_hardship_relaciones",
             "hardships_drugs" = "F07_hardship_alcohol_drogas")

  ## 1. Justice gap (Figure 23) with EU average — CORRECTION, same file name, same function as the report
  jg_tbl <- get_results_table(hu, regions, target = "inside_justice_gap_keepdk", ctype = "short")
  eu_jg <- eu_est(eu, "inside_justice_gap_keepdk")
  render_grouped(jg_tbl, "Hungary_justice_gap_keepdk.svg", 280, 265 + 12, eu_value = eu_jg$eu_pct,
                 margin_set = margin(-20, -25, 0, 0))

  ## 2. Figure 25 with EU average — CORRECTION, same file name
  fig25 <- imap_dfr(hardship_vars, function(v, lab) {
    est <- nat_est(hu, v)
    tibble(category = lab, hu = est$pct, n = est$n, eu = eu_est(eu, v)$eu_pct)
  })
  paired_bars(fig25, "Hungary_hardships")
  file.remove(file.path(OUT, "Hungary_hardships.png"))

  ## 3. "At least one hardship" and each hardship with the Figure 5 layout (national, EU, gender, age,
  ##    financial, area, co-occurrence) — ADDITIONAL, same function as the report
  brk <- imap_dfr(files, function(stem, v) {
    tb <- get_results_table(hu, regions, target = v, ctype = "full")
    eu_v <- eu_est(eu, v)$eu_pct
    render_grouped(tb, paste0(stem, ".svg"), 280, 330, eu_value = eu_v, omit = "Category")
    tb %>% filter(grouping != "Category") %>% transmute(indicador = v, grouping = as.character(grouping), category, hungria = Hungary) %>%
      mutate(media_ue = if_else(grouping == "National", eu_v, NA_real_))
  })

  save_table(bind_rows(
    jg_tbl %>% transmute(indicador = "inside_justice_gap_keepdk", grouping = as.character(grouping), category, hungria = Hungary) %>%
      mutate(media_ue = if_else(grouping == "National", eu_jg$eu_pct, NA_real_)),
    brk
  ), "F07")

  g <- function(ind, grp) round(brk$hungria[brk$indicador == ind & brk$category == grp], 1)
  save_note(
    "F07",
    "# F07 · Hardship: «al menos uno», desgloses y media UE de la brecha",
    "",
    "**Población:** entrevistados con al menos un problema no trivial (universo decidido el 28 sep; el mismo de la Figura 23). **Ponderación del reporte**; celdas con n < 30 en una región quedan como N/A. UE: promedio de 26 países.",
    "",
    "## Archivos",
    "",
    "- `Hungary_justice_gap_keepdk.svg` — CORRECCIÓN de la Figura 23: se agrega la media UE (misma metodología keepDK).",
    "- `Hungary_hardships.svg` — CORRECCIÓN de la Figura 25: se agrega la media UE por hardship.",
    "- `F07_hardship_al_menos_uno.svg` — ADICIONAL, mismo formato que las Figuras 5 y 23, para leerlas lado a lado.",
    "- `F07_hardship_salud.svg`, `F07_hardship_economico.svg`, `F07_hardship_relaciones.svg`, `F07_hardship_alcohol_drogas.svg` — ADICIONALES: cada hardship por género, edad, situación financiera, zona y número de problemas.",
    "",
    "## Cifras principales",
    "",
    paste0("- Brecha de justicia: Hungría ", round(jg_tbl$Hungary[jg_tbl$grouping == "National"], 1), " % contra UE ", round(eu_jg$eu_pct, 1), " %: **Hungría está en la media europea**."),
    paste0("- Al menos un hardship: Hungría ", g("hardship", "National"), " % contra UE ", round(brk$media_ue[brk$indicador == "hardship" & brk$grouping == "National"], 1),
           " %; constreñidos ", g("hardship", "Constrained"), " % contra no constreñidos ", g("hardship", "Unconstrained"), " %."),
    "",
    md_table(fig25),
    "",
    "La brecha mide lo que el sistema no dio; el hardship mide lo que costó. Hungría no se aparta de la UE en la brecha, pero sí en el daño: salud, economía y relaciones están por encima de la media europea.",
    "",
    "Nota: el rótulo «5 or more problems» del pipeline corresponde a 6 o más problemas (se solapa con «4-5 problems»); se mantiene como en el reporte publicado y queda registrado en la auditoría."
  )
  invisible(fig25)
}
