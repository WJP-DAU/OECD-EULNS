## A05 · Confianza contra desenlace, mecanismo, asistencia y daño -------------------------------------------

run_A05 <- function(af) {

  d <- af %>% mutate(
    favor = case_when(in_favor == 1 ~ "In favor of respondent", in_favor == 0 ~ "Not in favor / Unknown"),
    justo = case_when(rp_fair == 1 ~ "Process was fair", rp_fair == 0 ~ "Process was not fair"),
    tribunal = case_when(sankey_drm_stage_1 == "Turned to a DRM" & AJR_court_bin %in% 1 ~ "Went to a court",
                         sankey_drm_stage_1 == "Turned to a DRM" ~ "Other mechanisms only"),
    asistencia = case_when(rep_c_published == 1 ~ "Needed assistance, got it", rep_c_published == 0 ~ "Needed assistance, did not get it"),
    escalera_hardship = case_when(!has_nt ~ "No non-trivial problem",
                                  has_nt & hardship == 0 ~ "Problem, no hardship",
                                  has_nt & hardship == 1 ~ "Problem, with hardship")
  )

  cruce1 <- bind_rows(
    imap_dfr(trust_actors, \(v, lab) xt(d, paste0("trust_", v), "favor", lab, "Desenlace en favor")),
    imap_dfr(trust_actors, \(v, lab) xt(d, paste0("trust_", v), "justo", lab, "Equidad percibida del proceso"))
  )
  cruce2 <- xt(d, "trust_TRT_judges", "tribunal", "Judges", "Mecanismo usado")
  cruce3 <- bind_rows(
    xt(d, "trust_TRT_pda", "asistencia", "Public defense attorneys", "Asistencia (todos)"),
    xt(d %>% filter(financial == "Constrained"), "trust_TRT_pda", "asistencia", "Public defense attorneys", "Asistencia (constreñidos)"),
    xt(d %>% filter(financial == "Unconstrained"), "trust_TRT_pda", "asistencia", "Public defense attorneys", "Asistencia (no constreñidos)")
  )
  cruce4 <- imap_dfr(trust_actors, \(v, lab) xt(d, paste0("trust_", v), "escalera_hardship", lab, "Escalera con hardship")) %>%
    mutate(grupo = factor(grupo, c("No non-trivial problem", "Problem, no hardship", "Problem, with hardship"))) %>%
    arrange(desenlace, grupo) %>% mutate(grupo = as.character(grupo))

  tbl <- bind_rows(cruce1, cruce2, cruce3, cruce4) %>% rename(actor = desenlace)
  save_table(tbl, "A05")
  g <- function(a, k, gr) round(tbl$pct[tbl$actor == a & tbl$corte == k & tbl$grupo == gr], 1)

  report_bars(cruce4 %>% transmute(grouping = desenlace, category = grupo, pct), "A05_confianza_escalera_hardship")
  report_bars(bind_rows(
    cruce1 %>% filter(desenlace == "Judges") %>% transmute(grouping = if_else(corte == "Desenlace en favor", "Outcome", "Fairness of the process"), category = grupo, pct),
    cruce3 %>% filter(corte != "Asistencia (no constreñidos)") %>%
      transmute(grouping = if_else(corte == "Asistencia (todos)", "Trust in public defense attorneys", "Trust in public defense attorneys, financially constrained"), category = grupo, pct)
  ), "A05_confianza_desenlace_y_asistencia")

  save_note(
    "A05",
    "# A05 · Confianza contra desenlace, mecanismo, asistencia y daño",
    "",
    "**Confianza:** mucha + algo. **Bases:** (1) desenlace y equidad = con problema no trivial que respondieron; (2) tribunal = quienes acudieron a un DRM; (3) asistencia = quienes la necesitaron (variante c de R03), en total y por situación financiera; (4) escalera = población general sin problema / con problema sin hardship / con problema con hardship. **Ponderación del reporte**; celdas suprimidas = N/A.",
    "",
    "## Archivos",
    "",
    "- `A05_confianza_escalera_hardship.svg/.png` — escalera con hardship, tres actores.",
    "- `A05_confianza_desenlace_y_asistencia.svg/.png` — confianza en jueces por desenlace y por equidad; confianza en defensores públicos por asistencia (todos y constreñidos).",
    "- El cruce tribunal contra otros mecanismos queda solo en tabla: con la ponderación del reporte «otros mecanismos» queda suprimido y «tribunal» descansa en una sola región con n ≥ 30; no es interpretable.",
    "",
    md_table(tbl),
    "",
    "Lecturas de una línea:",
    "",
    paste0("- **Justicia procesal:** en jueces, la brecha por desenlace (en favor − no en favor) es de ", g("Judges", "Desenlace en favor", "In favor of respondent") - g("Judges", "Desenlace en favor", "Not in favor / Unknown"),
           " puntos y por equidad percibida de ", g("Judges", "Equidad percibida del proceso", "Process was fair") - g("Judges", "Equidad percibida del proceso", "Process was not fair"),
           "; en fiscales, ", g("Prosecutors", "Desenlace en favor", "In favor of respondent") - g("Prosecutors", "Desenlace en favor", "Not in favor / Unknown"), " contra ",
           g("Prosecutors", "Equidad percibida del proceso", "Process was fair") - g("Prosecutors", "Equidad percibida del proceso", "Process was not fair"),
           ". Las dos brechas son de magnitud parecida: tanto el resultado como la equidad del proceso se asocian con la confianza."),
    paste0("- **Defensores públicos × asistencia:** quienes necesitaron asistencia y no la obtuvieron confían ", g("Public defense attorneys", "Asistencia (todos)", "Needed assistance, did not get it"),
           " % contra ", g("Public defense attorneys", "Asistencia (todos)", "Needed assistance, got it"), " % de quienes la obtuvieron; entre constreñidos, ",
           g("Public defense attorneys", "Asistencia (constreñidos)", "Needed assistance, did not get it"), " % contra ", g("Public defense attorneys", "Asistencia (constreñidos)", "Needed assistance, got it"),
           " %. La dirección apoya S5-07, con diferencias de pocos puntos."),
    paste0("- **Hardship:** confianza en jueces ", g("Judges", "Escalera con hardship", "No non-trivial problem"), " → ", g("Judges", "Escalera con hardship", "Problem, no hardship"), " → ",
           g("Judges", "Escalera con hardship", "Problem, with hardship"), " %: la caída se concentra en quienes salieron dañados. Mismo patrón en fiscales y defensores."),
    "",
    "> Asociación, no efecto."
  )
  invisible(tbl)
}
