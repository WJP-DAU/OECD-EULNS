## F08 · Necesidades insatisfechas: Figura 24 legible ------------------------------------------------------

run_F08 <- function() {

  # Same data preparation as src/viz/jg_upset.R (NUTS-weighted national shares, x10 to keep one decimal)
  data <- hu %>%
    select(country_year_id, country_name_ltn, nuts_id) %>%
    left_join(
      hu_jg %>%
        mutate(jg_inverse_rp = case_when(
          jg_inverse_time == 1 | jg_inverse_fair == 1 | jg_inverse_cost == 1 ~ 1,
          !is.na(jg_inverse_time) | !is.na(jg_inverse_fair) | !is.na(jg_inverse_cost) ~ 0
        )) %>%
        select(country_year_id, jg_inverse_access2info, jg_inverse_access2rep, jg_inverse_rp, jg_inverse_outcome, inside_gap),
      by = "country_year_id"
    ) %>%
    filter(inside_gap == 1) %>%
    mutate(across(starts_with("jg_"), \(x) coalesce(x, 0))) %>%
    count(nuts_id, jg_inverse_access2info, jg_inverse_access2rep, jg_inverse_rp, jg_inverse_outcome, name = "count_size") %>%
    group_by(nuts_id) %>% mutate(total = sum(count_size)) %>% ungroup() %>%
    left_join(regions %>% select(nuts_id, wgt = regionpoppct), by = "nuts_id") %>%
    group_by(jg_inverse_access2info, jg_inverse_access2rep, jg_inverse_rp, jg_inverse_outcome) %>%
    summarise(pct = sum(count_size / total * wgt) * 100, n = sum(count_size), .groups = "drop")

  sets <- c("Access to Assistance", "Access to Information", "Resolution Process", "Problem Resolved")
  upset_data <- data %>%
    mutate(rows = round(pct * 10)) %>%
    slice(rep(row_number(), rows)) %>%
    transmute(`Access to Assistance` = jg_inverse_access2rep == 1, `Access to Information` = jg_inverse_access2info == 1,
              `Resolution Process` = jg_inverse_rp == 1, `Problem Resolved` = jg_inverse_outcome == 1)

  save_table(data %>% arrange(desc(pct)) %>%
               mutate(combinacion = pmap_chr(list(jg_inverse_access2rep, jg_inverse_access2info, jg_inverse_rp, jg_inverse_outcome),
                                             \(a, b, c, d) paste(sets[c(a, b, c, d) == 1], collapse = " + "))), "F08")

  max_bar <- max(data$pct)
  p <- ComplexUpset::upset(
    upset_data, sets,
    name = NULL, width_ratio = 0.28, min_size = 1, sort_sets = FALSE, stripes = "white",
    base_annotations = list(
      "% of people within\nthe justice gap" = ComplexUpset::intersection_size(
        mapping = aes(fill = "bar"),
        bar_number_threshold = 1,
        text = list(size = 3.6, family = "inter", vjust = -0.4),
        text_colors = c(on_background = COL_MAIN, on_bar = COL_MAIN),
        text_mapping = aes(label = paste0(format(round(!!ComplexUpset::get_size_mode("exclusive_intersection") / 10, 1), nsmall = 1), "%"))
      ) +
        scale_fill_manual(values = c(bar = COL_MAIN), guide = "none") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = NULL) +
        theme(panel.grid = element_blank(), axis.text.y = element_blank(), axis.title.y = element_text(family = "inter", size = 11))
    ),
    set_sizes = ComplexUpset::upset_set_size(geom = geom_bar(fill = COL_MAIN, width = 0.7)) +
      geom_text(aes(label = paste0(format(round(after_stat(count) / 10, 1), nsmall = 1), "%")), stat = "count",
                hjust = 1.15, family = "inter", size = 3.6, color = COL_TEXT) +
      scale_y_reverse(expand = expansion(mult = c(0.35, 0))) +
      theme(axis.text.x = element_blank(), axis.title.x = element_blank(), panel.grid = element_blank()),
    matrix = ComplexUpset::intersection_matrix(outline_color = list(active = COL_TEXT, inactive = "#e5e8e8")),
    themes = ComplexUpset::upset_default_themes(text = element_text(size = 12, family = "inter"))
  )

  showtext::showtext_auto(FALSE)
  ggsave(file.path(OUT, "Hungary_jg_upset.svg"), p, device = svglite::svglite, width = 320, height = 200, units = "mm", scale = 0.75)
  ggsave(file.path(OUT, "Hungary_jg_upset.png"), p, device = ragg::agg_png, width = 320, height = 200, units = "mm", scale = 0.75, dpi = 300, bg = "white")

  # Values as drawn: the figure sums the combination bars rounded to 0.1 (same as R04 pct_en_figura)
  set_sizes <- map_dbl(c("jg_inverse_access2rep", "jg_inverse_access2info", "jg_inverse_rp", "jg_inverse_outcome"),
                       \(v) sum(round(data$pct[data[[v]] == 1] * 10) / 10))
  n_needs <- data %>% mutate(k = jg_inverse_access2info + jg_inverse_access2rep + jg_inverse_rp + jg_inverse_outcome) %>%
    group_by(k) %>% summarise(pct = sum(pct), .groups = "drop")
  top <- data %>% arrange(desc(pct)) %>% slice_head(n = 3) %>%
    mutate(lbl = pmap_chr(list(jg_inverse_access2rep, jg_inverse_access2info, jg_inverse_rp, jg_inverse_outcome),
                          \(a, b, c, d) paste(sets[c(a, b, c, d) == 1], collapse = " + ")))

  save_note(
    "F08",
    "# F08 · Necesidades insatisfechas: Figura 24 legible",
    "",
    "**Población:** entrevistados dentro de la brecha de justicia (keepDK). Misma preparación que `jg_upset.R`: componentes faltantes cuentan como satisfechos; ponderación NUTS.",
    "",
    "## Archivos",
    "",
    "- `Hungary_jg_upset.svg/.png` — CORRECCIÓN de la Figura 24: márgenes suficientes, todos los valores completos, rótulos sin recortar y nota de lectura en el pie. Se conserva el UpSet porque el argumento del texto descansa en las combinaciones.",
    "",
    "## Valores recuperados (antes recortados)",
    "",
    md_table(tibble(necesidad = sets, pct_dentro_de_la_brecha = set_sizes)),
    "",
    "## Acumulación",
    "",
    md_table(n_needs %>% rename(necesidades_insatisfechas = k, pct_de_la_brecha = pct)),
    "",
    paste0("Combinaciones más frecuentes: ", paste0(top$lbl, " (", round(top$pct, 1), " %)", collapse = "; "), "."),
    "",
    paste0("Frase sugerida para el texto y nota de lectura para el pie: «Each top bar is one combination of unmet needs, marked by the dots below it; left bars show the share of people in the gap with each unmet need.» «Most people in the justice gap face several unmet needs at once: the most common combinations are ", paste(top$lbl, collapse = "; "), ".»")
  )
  invisible(data)
}
