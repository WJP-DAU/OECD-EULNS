## F10 · Confianza: los tres actores, desglose «mucha/algo» y medias UE de la Sección 4 -------------------

run_F10 <- function() {

  actors <- c("Judges" = "TRT_judges", "Prosecutors" = "TRT_prosecutors", "Public Defense Attorneys" = "TRT_pda")
  a_lot <- function(x) case_when(x == 1 ~ 1, x %in% c(2, 3, 4, 98) ~ 0)
  some  <- function(x) case_when(x == 2 ~ 1, x %in% c(1, 3, 4, 98) ~ 0)
  hu_t <- hu %>% mutate(non_trivial_problem_broader = if_else(non_trivial_problem %in% 1, "Yes", "No"))

  ## 1. Three actors, Hungary vs EU, "a lot" + "some" (table; feeds the corrected Figures 30-32)
  comp <- imap_dfr(actors, function(v, lab) {
    tibble(
      actor = lab,
      hu_total = nat_est(hu, v, binary_agree)$pct, hu_a_lot = nat_est(hu, v, a_lot)$pct, hu_some = nat_est(hu, v, some)$pct,
      eu_total = eu_est(eu, v, binary_agree)$eu_pct, eu_a_lot = eu_est(eu, v, a_lot)$eu_pct, eu_some = eu_est(eu, v, some)$eu_pct,
      n_hu = nat_est(hu, v, binary_agree)$n
    )
  }) %>% mutate(brecha_total = hu_total - eu_total, brecha_mucha = hu_a_lot - eu_a_lot)
  save_table(comp, "F10")

  ## 2. Figures 30-32 with "a lot" and "some" separated — CORRECTIONS, same file names
  files <- c("Judges" = "Hungary_trust_judges", "Prosecutors" = "Hungary_trust_prosecutors", "Public Defense Attorneys" = "Hungary_trust_pda")
  stacked_all <- imap_dfr(actors, function(v, lab) {
    t_lot  <- get_results_table(hu_t, regions, v, ctype = "trust", transform_var = a_lot) %>% select(grouping, category, pct = Hungary)
    t_some <- get_results_table(hu_t, regions, v, ctype = "trust", transform_var = some) %>% select(grouping, category, pct2 = Hungary)
    tb <- left_join(t_lot, t_some, by = c("grouping", "category")) %>% mutate(grouping = as.character(grouping))
    rows <- bind_rows(
      tibble(grouping = "", category = "National Average", pct = tb$pct[tb$grouping == "National"], pct2 = tb$pct2[tb$grouping == "National"]),
      tibble(grouping = "", category = "EU Average", pct = comp$eu_a_lot[comp$actor == lab], pct2 = comp$eu_some[comp$actor == lab]),
      tb %>% filter(grouping != "National")
    )
    report_bars(rows, files[[lab]], legend = c("A lot", "Some"), rev_within = TRUE)
    file.remove(file.path(OUT, paste0(files[[lab]], ".png")))
    rows %>% mutate(actor = lab)
  })
  save_table(stacked_all, "F10", "mucha_algo_por_grupo")

  ## 3. Section 4 figures with EU average (Figs. 26-29) — CORRECTIONS, same file names
  lns_countries <- eu %>% filter(!is.na(prevalence2)) %>% distinct(country_name_ltn) %>% pull()
  kr_tbl <- get_results_table(hu, regions, "AJE_legalrights", ctype = "short", transform_var = binary_agree)
  es_tbl <- get_results_table(hu, regions, "AJE_advice", ctype = "short", transform_var = binary_agree)
  eu_kr <- eu_est(eu, "AJE_legalrights", binary_agree)
  eu_es <- eu_est(eu, "AJE_advice", binary_agree)
  render_grouped(kr_tbl, "Hungary_knowledge_rights.svg", 280, 265 + 12, eu_value = eu_kr$eu_pct)
  render_grouped(es_tbl, "Hungary_expert_support.svg", 280, 265 + 12, eu_value = eu_es$eu_pct)

  causes <- perceived_causes_country_indicators(eu, regions, "4", "x", "x") %>%
    mutate(pct = if_else(country %in% lns_countries, pct, NA_real_))   # Ireland: NA (no LNS module)
  causes_tbl <- causes %>% filter(country == HU) %>% select(category, hu = pct) %>%
    left_join(causes %>% group_by(category) %>% summarise(eu = mean(pct, na.rm = TRUE), .groups = "drop"), by = "category") %>%
    arrange(desc(hu))
  paired_bars(causes_tbl, "Hungary_perceived_causes")
  file.remove(file.path(OUT, "Hungary_perceived_causes.png"))

  docs <- c("Official proof of identity" = "vulnerability1", "Written work agreement" = "vulnerability3", "Proof of housing or land tenure" = "vulnerability2")
  docs_tbl <- imap_dfr(docs, function(v, lab) {
    est <- nat_est(hu, v)
    tibble(category = lab, hu = est$pct, eu = eu_est(eu, v)$eu_pct, n = est$n)
  })
  paired_bars(docs_tbl, "Hungary_legal_empowerment")
  file.remove(file.path(OUT, "Hungary_legal_empowerment.png"))

  sec4 <- bind_rows(
    tibble(indicador = "Knowledge of rights (Fig. 26)", hungria = kr_tbl$Hungary[kr_tbl$grouping == "National"], media_ue = eu_kr$eu_pct, paises = eu_kr$n_countries),
    tibble(indicador = "Expert support (Fig. 27)", hungria = es_tbl$Hungary[es_tbl$grouping == "National"], media_ue = eu_es$eu_pct, paises = eu_es$n_countries),
    causes_tbl %>% transmute(indicador = paste0("Perceived cause: ", category), hungria = hu, media_ue = eu, paises = length(lns_countries)),
    docs_tbl %>% transmute(indicador = category, hungria = hu, media_ue = eu, paises = 27L)
  ) %>% mutate(diferencia = hungria - media_ue)
  save_table(sec4, "F10", "seccion4_media_ue")

  save_note(
    "F10",
    "# F10 · Confianza: tres actores, «mucha» contra «algo», y medias UE de la Sección 4",
    "",
    "**Población:** confianza = población general (pregunta del sondeo general; UE con 27 países, incluida Irlanda). Escala: 1 = mucha, 2 = algo, 3 = poca, 4 = ninguna, 98 = no sabe (cuenta como no confía), 99 = excluido, igual que el pipeline. **Ponderación del reporte.** Causas percibidas: UE con 26 países (Irlanda = NA).",
    "",
    md_table(comp),
    "",
    "## Archivos",
    "",
    "- `Hungary_trust_judges.svg`, `Hungary_trust_prosecutors.svg`, `Hungary_trust_pda.svg` — CORRECCIONES de las Figuras 30-32 con «mucha» y «algo» separadas y media UE desglosada.",
    "- `Hungary_knowledge_rights.svg`, `Hungary_expert_support.svg`, `Hungary_perceived_causes.svg`, `Hungary_legal_empowerment.svg` — CORRECCIONES de las Figuras 26-29 con media UE.",
    "",
    paste0("De la confianza en jueces (", round(comp$hu_total[1]), " %), solo ", round(comp$hu_a_lot[1], 1), " puntos son «mucha» (UE ", round(comp$eu_a_lot[1], 1), " de ", round(comp$eu_total[1]), ")."),
    "",
    "## Sección 4 contra la UE",
    "",
    md_table(sec4),
    "",
    "## Correcciones a la planilla y a los briefs",
    "",
    "- **La Figura 27 no mide «fully satisfied».** `AJE_advice` es una escala de acuerdo sobre la *disponibilidad de ayuda experta*, colapsada «muy de acuerdo + de acuerdo», igual que la confianza colapsa «mucha + algo». La premisa de S5-02 («laxo contra estricto») no se sostiene. El título de la Figura 27 debe cambiar.",
    "- **Las Figuras 26 y 27 no son población general ni solo problemas no triviales:** se preguntaron a quienes seleccionaron un problema, de cualquier severidad (n ≈ 1.443 de 1.750).",
    "- **S4-03:** frente a la UE, el conocimiento de derechos sí es donde Hungría más se distancia hacia arriba; el superlativo se sostiene formulado como comparación con la UE."
  )
  invisible(comp)
}
