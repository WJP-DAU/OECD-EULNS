## F01 · Matriz de co-ocurrencia condicional + corrección de la Figura 4 ----------------------------------

run_F01 <- function() {

  source("src/viz/network_graph.R")
  nt <- problem_flags(hu)
  flags <- bind_cols(hu %>% select(country_name_ltn, nuts_id), nt %>% mutate(across(everything(), as.numeric)))
  prev <- map_dbl(categories, \(k) nat_est(flags, k)$pct) %>% setNames(categories) %>% sort(decreasing = TRUE)
  ord <- names(prev)

  ## 1. Conditional matrix P(row | column), report weighting (NUTS, n < 30 suppressed)
  mat <- expand_grid(col = ord, row = ord) %>%
    mutate(
      est = map2(col, row, \(x, y) if (x == y) NULL else nat_est(flags[nt[[x]], ], y)),
      pct = map_dbl(est, \(e) if (is.null(e)) NA_real_ else e$pct),
      n_base = map_int(col, \(x) sum(nt[[x]])),
      band = cut(pct, c(-Inf, 25, 50, Inf), labels = c("Less than 25%", "25% to 50%", "50% or more"), right = FALSE),
      label = case_when(col == row ~ "–", is.na(pct) ~ "N/A", TRUE ~ paste0(round(pct), "%"))
    ) %>%
    select(-est)
  save_table(mat %>% transmute(entre_quienes_tuvieron = col, tambien_tuvieron = row, pct, n_base), "F01")

  p <- ggplot(mat, aes(x = factor(col, ord), y = factor(row, rev(ord)))) +
    geom_tile(aes(fill = band), color = "white", linewidth = 0.8) +
    geom_text(aes(label = label, color = band %in% "50% or more"), family = "inter", size = 3.6, show.legend = FALSE) +
    scale_fill_manual(values = c("Less than 25%" = COL_BG, "25% to 50%" = COL_LIGHT, "50% or more" = COL_MAIN),
                      na.value = "white", breaks = c("Less than 25%", "25% to 50%", "50% or more")) +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = COL_TEXT)) +
    scale_x_discrete(position = "top") +
    labs(x = "Among people who experienced problems with:", y = "People who also experienced:") +
    theme_minimal(base_family = "inter") +
    theme(
      axis.text.x.top = element_text(angle = 40, hjust = 0, vjust = 0, size = 11, color = COL_TEXT),
      axis.text.y = element_text(size = 11, color = COL_TEXT),
      axis.title.x.top = element_text(face = "bold.italic", color = COL_MAIN, hjust = 0, size = 12),
      axis.title.y = element_text(face = "bold.italic", color = COL_MAIN, size = 12),
      panel.grid = element_blank(), legend.position = "top", legend.title = element_blank(),
      legend.text = element_text(size = 11), plot.margin = margin(10, 45, 10, 10),
      plot.background = element_rect(fill = "white", color = NA)
    )
  save_fig(p, "F01_matriz_coocurrencia_condicional", 300, 270)

  ## 2. Figure 4 corrected: same function (network_graph.R), non-trivial problems only.
  ##    AJP_*_bin is set to 1 only when the problem is non-trivial, so the unchanged function draws
  ##    non-trivial prevalences and lifts (the ones quoted in §1.2). Unweighted, as in the report.
  hu_nt <- hu
  for (code in names(problem_map)) {
    b <- paste0("AJP_", code, "_bin"); s <- paste0("AJP_", code, "_sev")
    hu_nt[[b]] <- if_else(hu[[b]] %in% 1 & hu[[s]] >= 4 & hu[[s]] < 98, 1, 2)
  }
  render_existing(function() gen_network_graph(hu_nt, country = HU, w = 320, h = 320),
                  "cooccurrence_network.svg", "cooccurrence_network.svg")
  file.copy(file.path(CACHE, "render", "output", "viz", HU, "cooccurrence_network.png"),
            file.path(OUT, "cooccurrence_network.png"), overwrite = TRUE)

  ## Lift check: text of §1.2 vs recomputed (non-trivial, unweighted as in module_1.R) and ceilings
  lift <- function(a, b) mean(nt[[a]] & nt[[b]]) / (mean(nt[[a]]) * mean(nt[[b]]))
  quoted <- tribble(
    ~a, ~b, ~lift_texto,
    "Citizenship & ID", "Law Enforcement", 11.8,
    "Citizenship & ID", "Housing", 5.4,
    "Citizenship & ID", "Education", 4.8,
    "Citizenship & ID", "Land", 4.0,
    "Housing", "Law Enforcement", 5.1,
    "Housing", "Education", 3.9,
    "Law Enforcement", "Family", 4.2,
    "Law Enforcement", "Education", 4.1,
    "Law Enforcement", "Land", 3.8,
    "Law Enforcement", "Employment", 3.7,
    "Family", "Citizenship & ID", 3.3
  ) %>%
    mutate(
      lift_no_trivial = map2_dbl(a, b, lift),
      techo = map2_dbl(a, b, \(x, y) 1 / max(mean(nt[[x]]), mean(nt[[y]]))),
      supera_techo = lift_no_trivial > techo + 1e-9,
      n_ambos = map2_int(a, b, \(x, y) sum(nt[[x]] & nt[[y]]))
    )
  save_table(quoted, "F01", "lifts_texto")

  save_note(
    "F01",
    "# F01 · Matriz de co-ocurrencia condicional y Figura 4 corregida",
    "",
    "**Población:** toda la muestra de Hungría (n = 1.750); una categoría cuenta si la persona tuvo al menos un problema **no trivial** (severidad 4-10) en ella.",
    "**Ponderación:** matriz con el método del reporte (promedio NUTS, regiones con n < 30 suprimidas → N/A). Red y lifts sin ponderar, como en el reporte (`network_graph.R`, `module_1.R`).",
    "",
    "## Archivos",
    "",
    "- `F01_matriz_coocurrencia_condicional.svg/.png` — ADICIONAL. Lectura por columna: «entre quienes tuvieron X, % que también tuvo Y». Diagonal en guion. Tres tramos (<25 %, 25-50 %, ≥50 %). Las columnas de categorías chicas (Law Enforcement, Citizenship & ID) quedan total o parcialmente en N/A por tamaño de muestra.",
    "- `cooccurrence_network.svg/.png` — CORRECCIÓN de la Figura 4 (mismo nombre, misma función `network_graph.R`), ahora con problemas **no triviales**. Las prevalencias de los nodos pasan a ser las de la Figura 2 y los lifts del dibujo pasan a ser los que cita el texto.",
    "",
    "## Lifts citados en §1.2 contra los recalculados",
    "",
    md_table(quoted %>% select(a, b, lift_texto, lift_no_trivial, techo, supera_techo, n_ambos)),
    "",
    "Ningún lift supera su techo cuando se calcula con la misma severidad que los nodos. Citizenship & ID ↔ Land es 3,95: el texto debe decir 3,9, no 4,0.",
    "",
    paste0("Tamaños: Law Enforcement n = ", sum(nt[["Law Enforcement"]]), ", Citizenship & ID n = ", sum(nt[["Citizenship & ID"]]), "; el lift de 11,8 descansa en ", quoted$n_ambos[1], " personas."),
    "",
    "## Pie sugerido para la Figura 4 corregida",
    "",
    "> Co-occurrence network of non-trivial justice problem categories (severity 4 or more) among all respondents. Node label = prevalence. Node color = centrality. Edge thickness and proximity = lift.",
    "",
    "(La nota actual dice que el tamaño del nodo codifica la prevalencia; la función dibuja todos los nodos del mismo tamaño, así que esa línea debe quitarse.)"
  )
  invisible(mat)
}
