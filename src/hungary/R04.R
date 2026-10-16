## R04 · Composición de la brecha de justicia -------------------------------------------------------------

# Components as drawn in Figure 24 (jg_upset.R): resolution process is unmet if time, cost OR fairness is.
jg_components <- function() {
  hu_jg %>%
    mutate(
      rp_unmet = case_when(
        jg_inverse_time == 1 | jg_inverse_fair == 1 | jg_inverse_cost == 1 ~ 1,
        !is.na(jg_inverse_time) | !is.na(jg_inverse_fair) | !is.na(jg_inverse_cost) ~ 0
      ),
      n_unmet = rowSums(cbind(jg_inverse_access2info, jg_inverse_access2rep, rp_unmet, jg_inverse_outcome), na.rm = TRUE),
      n_answered = rowSums(!is.na(cbind(jg_inverse_access2info, jg_inverse_access2rep, rp_unmet, jg_inverse_outcome)))
    ) %>%
    inner_join(hu %>% select(country_year_id, country_name_ltn, nuts_id), by = "country_year_id") %>%
    filter(!is.na(a2j_score))
}

run_R04 <- function() {

  j <- jg_components() %>%
    mutate(
      rule_published = inside_gap,
      rule_at_least_1 = as.numeric(n_unmet >= 1),
      rule_at_least_2 = as.numeric(n_unmet >= 2),
      rule_majority = as.numeric(n_unmet / n_answered > 0.5)
    )

  rules <- tribble(
    ~regla, ~variable, ~descripcion,
    "Publicada", "rule_published", "a2j_score < 0,67; a2j_score = media de 4 componentes (información, asistencia, proceso = media de tiempo/costo/equidad, problema resuelto), DK cuenta como insatisfecho",
    "Al menos una", "rule_at_least_1", "Al menos 1 de las 4 necesidades insatisfecha",
    "Al menos dos", "rule_at_least_2", "Al menos 2 de las 4 necesidades insatisfechas",
    "Mayoría", "rule_majority", "Más de la mitad de las necesidades respondidas insatisfechas"
  ) %>%
    mutate(
      est = map(variable, \(v) nat_est(j, v)),
      hungria = map_dbl(est, "pct"),
      n = map_int(est, "n")
    ) %>%
    select(-est)

  eu_gap <- eu_est(eu, "inside_justice_gap_keepdk")
  rules <- rules %>% mutate(media_ue = if_else(variable == "rule_published", eu_gap$eu_pct, NA_real_))
  save_table(rules, "R04")

  # Figure 24 values (weighted as in jg_upset.R: missing component = not unmet)
  inside <- j %>% filter(inside_gap == 1) %>%
    mutate(across(c(jg_inverse_access2info, jg_inverse_access2rep, rp_unmet, jg_inverse_outcome), \(x) coalesce(x, 0)))
  set_sizes <- tibble(
    necesidad = c("Access to Assistance", "Access to Information", "Resolution Process", "Problem Resolved"),
    variable = c("jg_inverse_access2rep", "jg_inverse_access2info", "rp_unmet", "jg_inverse_outcome")
  ) %>%
    mutate(pct_dentro_de_la_brecha = map_dbl(variable, \(v) nat_est(inside, v)$pct),
           n_dentro_de_la_brecha = nrow(inside))
  combos <- inside %>%
    mutate(combinacion = pmap_chr(list(jg_inverse_access2rep, jg_inverse_access2info, rp_unmet, jg_inverse_outcome), function(a, b, c, d) {
      paste(c("Asistencia", "Información", "Proceso", "Resolución")[c(a, b, c, d) == 1], collapse = " + ")
    }),
    combinacion = if_else(combinacion == "", "(ninguna marcada)", combinacion)) %>%
    count(nuts_id, combinacion) %>%
    group_by(nuts_id) %>% mutate(p = n / sum(n)) %>% ungroup() %>%
    left_join(regions %>% select(nuts_id, regionpoppct), by = "nuts_id") %>%
    group_by(combinacion) %>%
    summarise(pct = 100 * sum(p * regionpoppct), n = sum(n), .groups = "drop") %>%
    arrange(desc(pct)) %>%
    mutate(n_necesidades = str_count(combinacion, "\\+") + 1)
  # The UpSet figure builds set sizes from the combination bars rounded to 0.1 (x10 row expansion),
  # so the figure shows slightly different totals than the exact weighted share: report both.
  token <- c("Access to Assistance" = "Asistencia", "Access to Information" = "Información",
             "Resolution Process" = "Proceso", "Problem Resolved" = "Resolución")
  set_sizes <- set_sizes %>%
    mutate(pct_en_figura = map_dbl(necesidad, \(x) sum(round(combos$pct[str_detect(combos$combinacion, fixed(token[[x]]))] * 10) / 10)))
  save_table(set_sizes, "R04", "fig24_conjuntos")
  save_table(combos, "R04", "fig24_combinaciones")

  mean_needs <- nat_est(inside %>% mutate(k = jg_inverse_access2info + jg_inverse_access2rep + rp_unmet + jg_inverse_outcome), "k")$pct / 100
  dist <- table(n_unmet = j$n_unmet, dentro = j$inside_gap)

  save_note(
    "R04",
    "# R04 · Composición de la brecha de justicia",
    "",
    paste0("**Población:** entrevistados con al menos un problema no trivial y `a2j_score` definido (Hungría, n = ", nrow(j), "). Ponderación NUTS. Método keepDK (el publicado)."),
    "",
    "## La regla, en una frase",
    "",
    "> Una persona está en la brecha si el promedio de sus cuatro componentes satisfechos —información, asistencia, proceso (promedio de tiempo, costo y equidad) y problema resuelto, contando «no sabe» como insatisfecho— es menor que 0,67.",
    "",
    "No es «al menos una necesidad insatisfecha». Con cuatro componentes respondidos, equivale en la práctica a **dos o más** insatisfechas; con menos componentes o con el proceso parcialmente insatisfecho, una sola puede bastar.",
    "",
    md_table(rules %>% select(regla, hungria, n, media_ue, descripcion)),
    "",
    "Distribución (sin ponderar) de necesidades insatisfechas según si la persona queda dentro de la brecha:",
    "",
    "```",
    capture.output(print(dist)),
    "```",
    "",
    "## Por qué el 68 % sin asistencia no contradice el 48 %",
    "",
    "El 68 % de §2.2 está sobre quienes **necesitaban** asistencia (variante c de R03). En la brecha, «asistencia insatisfecha» es un componente entre cuatro y se mide sobre otra base. Faltar asistencia no basta para entrar en la brecha.",
    "",
    "## Media UE (Figura 23)",
    "",
    paste0("Media UE de la brecha con la misma metodología: **", round(eu_gap$eu_pct, 1), " %** (", eu_gap$n_countries, " países). Hungría (",
           round(rules$hungria[1], 1), " %) está **en la media europea**. Agregada en `Hungary_justice_gap_keepdk.svg` (esta carpeta)."),
    "",
    "## Valores de la Figura 24 (recortados en el PDF)",
    "",
    md_table(set_sizes %>% select(necesidad, pct_exacto = pct_dentro_de_la_brecha, pct_en_figura)),
    "`pct_en_figura` = lo que muestra la Figura 24 (y su versión corregida): suma de las barras de combinación redondeadas a 0,1; `pct_exacto` = participación ponderada sin redondeo.",
    "",
    md_table(head(combos, 15)),
    "",
    paste0("En promedio, quienes están en la brecha acumulan **", round(mean_needs, 1), "** necesidades insatisfechas de cuatro.")
  )
  invisible(rules)
}
