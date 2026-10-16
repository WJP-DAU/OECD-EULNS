## F02 · Prevalencia × severidad por categoría, por situación financiera y por zona -----------------------

# Every reported problem (any severity 0-10) with its category, for the severity means.
problems_long <- function(df, extra = NULL) {
  map_dfr(names(problem_map), function(code) {
    b <- df[[paste0("AJP_", code, "_bin")]]; s <- df[[paste0("AJP_", code, "_sev")]]
    keep <- !is.na(b) & b == 1 & !is.na(s) & s <= 10
    out <- tibble(country_name_ltn = df$country_name_ltn, nuts_id = df$nuts_id, sev = s, category = problem_map[[code]])
    if (!is.null(extra)) out[[extra]] <- as.character(df[[extra]])
    out[keep, ]
  })
}

category_profile <- function(group_var) {
  nt <- problem_flags(hu)
  flags <- bind_cols(hu %>% select(country_name_ltn, nuts_id, all_of(group_var)), nt %>% mutate(across(everything(), as.numeric)))
  prev <- map_dfr(categories, \(k) nat_est(flags, k, by = group_var) %>%
                    transmute(grupo = as.character(.data[[group_var]]), category = k, prevalencia = pct, n_grupo = n))
  n_nt <- map_dfr(categories, \(k) flags %>% filter(.data[[k]] == 1, !is.na(.data[[group_var]])) %>%
                    count(grupo = as.character(.data[[group_var]]), name = "n_con_problema_nt") %>% mutate(category = k))
  sev <- nuts_mean_long(problems_long(hu, group_var) %>% filter(!is.na(.data[[group_var]])), "sev", c("category", group_var)) %>%
    transmute(grupo = .data[[group_var]], category, severidad_media = est, n_problemas = n)
  prev %>% left_join(n_nt, by = c("grupo", "category")) %>% left_join(sev, by = c("grupo", "category")) %>%
    mutate(n_con_problema_nt = coalesce(n_con_problema_nt, 0L), n_insuficiente = n_con_problema_nt < 30 | is.na(severidad_media))
}

run_F02 <- function() {

  cuts <- c("Financial Situation" = "financial", "Area of Residence" = "residence")
  prof <- imap_dfr(cuts, \(v, lab) category_profile(v) %>% mutate(corte = lab))
  save_table(prof, "F02")

  top_cats <- c("Public Services", "Consumer", "Community", "Money & Debt", "Housing", "Family")
  for (lab in names(cuts)) {
    d <- prof %>% filter(corte == lab) %>% mutate(destacada = category %in% top_cats)
    p <- ggplot(d, aes(x = prevalencia, y = severidad_media)) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = COL_GREY, linewidth = 0.5, linetype = "dashed") +
      geom_point(aes(color = destacada, shape = n_insuficiente), size = 3.4, stroke = 1.1, na.rm = TRUE) +
      ggrepel::geom_text_repel(data = d %>% filter(destacada), aes(label = category), family = "inter", size = 4,
                               color = COL_MAIN, seed = 7, min.segment.length = 0.3, na.rm = TRUE) +
      facet_wrap(~grupo, nrow = 1) +
      scale_color_manual(values = c(`TRUE` = COL_MAIN, `FALSE` = "#c4c4c4"), guide = "none") +
      scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), guide = "none") +
      scale_x_continuous(limits = c(0, 50), labels = \(x) paste0(x, "%")) +
      scale_y_continuous(limits = c(4, 8), breaks = 4:8) +
      labs(x = "Prevalence", y = "Mean seriousness (0-10)") +
      theme_minimal(base_family = "inter") +
      theme(strip.text = element_text(face = "bold.italic", color = COL_MAIN, size = 14),
            axis.text = element_text(size = 12, color = COL_TEXT), axis.title = element_text(size = 12, color = COL_TEXT),
            panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "#eeeeee"),
            plot.background = element_rect(fill = "white", color = NA))
    save_fig(p, paste0("F02_prevalencia_severidad_", if_else(lab == "Financial Situation", "financiera", "zona")), 300, 140)
  }

  shift <- prof %>% select(corte, grupo, category, prevalencia, severidad_media) %>%
    group_by(corte, category) %>%
    summarise(dif_prevalencia = diff(prevalencia), dif_severidad = diff(severidad_media), .groups = "drop")
  rank_cor <- prof %>% group_by(corte) %>%
    summarise(spearman_orden_categorias = cor(prevalencia[grupo == first(grupo)], prevalencia[grupo == last(grupo)], method = "spearman"), .groups = "drop")

  save_note(
    "F02",
    "# F02 · Prevalencia × severidad por categoría, por situación financiera y zona (rural/urbano)",
    "",
    "**Población:** toda la muestra de cada grupo. «Región» = rural/urbano (decisión del 28 sep). **Ponderación del reporte** (promedio NUTS, regiones con n < 30 suprimidas).",
    "Eje X: % de personas del grupo con al menos un problema no trivial (4-10) en la categoría. Eje Y: severidad media (0-10) de todos los problemas reportados en la categoría. Punto hueco: menos de 30 personas con problema no trivial en la categoría o severidad suprimida. Solo se rotulan las categorías principales; el resto en gris.",
    "",
    "## Archivos",
    "",
    "- `F02_prevalencia_severidad_financiera.svg/.png`, `F02_prevalencia_severidad_zona.svg/.png` — ADICIONALES.",
    "",
    "## Diferencias (segundo grupo − primero: Unconstrained − Constrained; Urban − Rural)",
    "",
    md_table(shift),
    "",
    md_table(rank_cor, 2),
    "",
    "Spearman cercano a 1 = el orden de las categorías se mantiene (la nube se desplaza); más bajo = la composición cambia.",
    "",
    md_table(prof)
  )
  invisible(prof)
}
