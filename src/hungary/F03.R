## F03 · Distribución de la severidad (0-10), Hungría contra la UE -----------------------------------------

# Share of reported problems at each severity value, report weighting (per NUTS region, then weighted).
severity_distribution <- function(long, groups = NULL) {
  long <- long %>% mutate(one = 1)
  map_dfr(0:10, function(v) {
    nuts_mean_long(long %>% mutate(at_v = as.numeric(sev == v)), "at_v", groups) %>% mutate(sev = v)
  }) %>% mutate(pct = 100 * est)
}

run_F03 <- function() {

  hs <- problems_long(hu, "financial")
  es <- problems_long(eu)

  dist_country <- severity_distribution(es)
  hu_dist <- dist_country %>% filter(country_name_ltn == HU) %>% transmute(sev, pct, serie = "Hungary")
  eu_dist <- dist_country %>% group_by(sev) %>% summarise(pct = mean(pct, na.rm = TRUE), .groups = "drop") %>% mutate(serie = "EU Average")
  dist <- bind_rows(hu_dist, eu_dist)

  mean_sev <- nuts_mean_long(es, "sev")
  summ <- tibble(
    serie = c("Hungary", "EU Average"),
    severidad_media = c(mean_sev$est[mean_sev$country_name_ltn == HU], mean(mean_sev$est, na.rm = TRUE)),
    pct_4_o_mas = c(sum(hu_dist$pct[hu_dist$sev >= 4]), sum(eu_dist$pct[eu_dist$sev >= 4])),
    pct_8_o_mas = c(sum(hu_dist$pct[hu_dist$sev >= 8]), sum(eu_dist$pct[eu_dist$sev >= 8])),
    n_problemas = c(nrow(hs), nrow(es))
  )
  save_table(dist %>% pivot_wider(names_from = serie, values_from = pct), "F03")
  save_table(summ, "F03", "resumen")

  sev_plot <- function(d, fills, stem) {
    p <- ggplot(d, aes(x = sev, y = pct, fill = serie)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      geom_vline(xintercept = 3.5, linetype = "dashed", color = COL_TEXT, linewidth = 0.4) +
      scale_fill_manual(values = fills) +
      scale_x_continuous(breaks = 0:10) +
      scale_y_continuous(labels = \(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.08))) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_family = "inter") +
      theme(legend.position = "top", legend.title = element_blank(), legend.text = element_text(size = 13),
            axis.text = element_text(size = 13, color = COL_TEXT), panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "#eeeeee"),
            plot.background = element_rect(fill = "white", color = NA))
    save_fig(p, stem, 280, 120)
  }
  sev_plot(dist %>% mutate(serie = factor(serie, c("Hungary", "EU Average"))), c("Hungary" = COL_MAIN, "EU Average" = COL_LIGHT),
           "F03_distribucion_severidad_HU_UE")

  fin <- severity_distribution(hs %>% filter(!is.na(financial)), "financial") %>%
    transmute(serie = financial, sev, pct, n)
  save_table(fin, "F03", "por_situacion_financiera")
  sev_plot(fin %>% mutate(serie = factor(serie, c("Constrained", "Unconstrained"))), c("Constrained" = COL_ACCENT, "Unconstrained" = COL_MAIN),
           "F03_distribucion_severidad_financiera")

  save_note(
    "F03",
    "# F03 · Distribución de la severidad",
    "",
    "**Unidad:** cada problema reportado (no cada persona), severidad 0-10; se excluyen 98/99. Sí hay severidad por debajo de 4: el filtro no se aplicó en campo.",
    "**Ponderación del reporte:** distribución por región NUTS (regiones con n < 30 suprimidas) ponderada por población; UE = promedio de las 26 distribuciones nacionales. Línea punteada = umbral de no trivial (4 o más).",
    "",
    md_table(summ),
    "",
    md_table(dist %>% pivot_wider(names_from = serie, values_from = pct)),
    "",
    "## Archivos",
    "",
    "- `F03_distribucion_severidad_HU_UE.svg/.png` — ADICIONAL (principal).",
    "- `F03_distribucion_severidad_financiera.svg/.png` — ADICIONAL (secundaria). La versión por categoría no se dibuja: 12 categorías × 11 valores dejan celdas chicas.",
    "",
    "## Lectura",
    "",
    paste0("**Los húngaros no califican sus problemas como más graves que el resto de Europa: los califican como algo menos graves** (severidad media ",
           round(summ$severidad_media[1], 1), " contra ", round(summ$severidad_media[2], 1), "; ", round(summ$pct_4_o_mas[1]), " % de los problemas con 4 o más contra ",
           round(summ$pct_4_o_mas[2]), " % en la UE). La prevalencia más alta viene de que reportan **más problemas**, no de un anclaje hacia arriba en la escala. ",
           "Junto con R02 (la brecha se mantiene con umbrales 6 y 8), esto responde la objeción del umbral.")
  )
  invisible(summ)
}
