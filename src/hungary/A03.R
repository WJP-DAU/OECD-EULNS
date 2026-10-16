## A03 · Daño según el desenlace ----------------------------------------------------------------------------

run_A03 <- function(af) {

  harms <- c("At least one hardship" = "hardship", "Health issues" = "hardships_health", "Economic issues" = "hardships_income",
             "Relationship breakdown" = "hardships_emotional", "Alcohol/Drug problems" = "hardships_drugs")
  d <- af %>% filter(has_nt) %>% mutate(
    resuelto = case_when(resolved == 1 ~ "Fully resolved", resolved == 0 ~ "Not resolved"),
    favor = case_when(in_favor == 1 ~ "In favor of respondent", in_favor == 0 ~ "Not in favor / Unknown")
  )
  tbl <- bind_rows(
    imap_dfr(harms, \(v, lab) xt(d, v, "resuelto", lab, "Resolución")),
    imap_dfr(harms, \(v, lab) xt(d, v, "favor", lab, "Desenlace en favor"))
  )
  save_table(tbl, "A03")

  for (k in c("Resolución", "Desenlace en favor")) {
    report_bars(tbl %>% filter(corte == k) %>% transmute(grouping = desenlace, category = grupo, pct),
                if_else(k == "Resolución", "A03_hardship_segun_resolucion", "A03_hardship_segun_desenlace_en_favor"))
  }

  gaps <- tbl %>% group_by(desenlace, corte) %>%
    summarise(diferencia = pct[grupo %in% c("Fully resolved", "In favor of respondent")] - pct[grupo %in% c("Not resolved", "Not in favor / Unknown")],
              razon = pct[grupo %in% c("Fully resolved", "In favor of respondent")] / pct[grupo %in% c("Not resolved", "Not in favor / Unknown")],
              .groups = "drop")
  any_res <- gaps %>% filter(desenlace == "At least one hardship", corte == "Resolución")

  save_note(
    "A03",
    "# A03 · Daño según el desenlace",
    "",
    "**Base:** entrevistados con problema no trivial; «resuelto» solo entre quienes terminaron el proceso. Cuatro hardships y el indicador «al menos uno» de F07. **Ponderación del reporte.**",
    "",
    md_table(tbl),
    "",
    md_table(gaps, 2),
    "",
    paste0("**Lectura:** quienes resolvieron su problema reportan ", round(abs(any_res$diferencia)), " puntos menos de daño (al menos un hardship) que quienes no lo resolvieron; la diferencia aparece en los cuatro tipos de daño."),
    "",
    "> Asociación, no efecto: el daño puede preceder a la resolución y empujarla."
  )
  invisible(tbl)
}
