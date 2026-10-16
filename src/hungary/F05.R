## F05 · El embudo del recorrido --------------------------------------------------------------------------

run_F05 <- function() {

  hj <- journey_vars(hu)
  ej <- journey_vars(eu)

  steps <- tribble(
    ~orden, ~escalon, ~variable, ~base,
    1, "Had a non-trivial problem", "prevalence2", "All respondents",
    2, "Had access to information and advice", "access2info", "People with a non-trivial problem",
    3, "Obtained appropriate assistance", "rep_a_all", "People with a non-trivial problem",
    4, "Turned to a dispute resolution mechanism", "drm_a_all", "People with a non-trivial problem"
  ) %>%
    mutate(
      hu = map_dbl(variable, \(v) nat_est(hj, v)$pct),
      n  = map_int(variable, \(v) nat_est(hj, v)$n),
      eu = map_dbl(variable, \(v) eu_est(ej, v)$eu_pct),
      publicado = c(68, 61, 32, 28),
      base_publicada = c("All respondents", "Non-trivial problem", "Non-trivial problem AND needed assistance, known reason",
                         "Non-trivial problem AND needed a DRM, known reason")
    )
  funnel <- steps %>%
    mutate(hu_base = if_else(orden == 1, 100, hu), eu_base = if_else(orden == 1, 100, eu),
           caida_hu = hu_base - lag(hu_base), caida_eu = eu_base - lag(eu_base))
  save_table(funnel, "F05")

  paired_bars(funnel %>% filter(orden > 1) %>% transmute(category = escalon, hu = hu_base, eu = eu_base),
              "F05_embudo_del_recorrido", w = 290)

  save_note(
    "F05",
    "# F05 · El embudo del recorrido",
    "",
    "**Base común:** entrevistados con al menos un problema no trivial (Hungría n ≈ 1.190). **Ponderación del reporte.** UE = promedio de 26 países con la misma definición.",
    "Asistencia y DRM con la variante (a) de R03: sobre todos los que tuvieron un problema, sin excluir a quienes no lo necesitaron; es la única variante en la que los escalones se pueden restar. La figura muestra los tres escalones sobre esa base; la prevalencia (68 % en Hungría, 49 % en la UE) define la base y va en el texto.",
    "",
    md_table(funnel %>% select(escalon, base, hu, eu, n, publicado, base_publicada, caida_hu, caida_eu)),
    "",
    "## Lectura",
    "",
    paste0("De cada 100 personas con un problema no trivial en Hungría, ", round(funnel$hu[2]), " tuvieron acceso a información, ",
           round(funnel$hu[3]), " obtuvieron asistencia apropiada y ", round(funnel$hu[4]), " acudieron a un mecanismo de resolución ",
           "(UE: ", round(funnel$eu[2]), " / ", round(funnel$eu[3]), " / ", round(funnel$eu[4]), "). ",
           "La mayor caída está entre información y asistencia (", round(abs(funnel$caida_hu[3])), " puntos en Hungría, ", round(abs(funnel$caida_eu[3])), " en la UE)."),
    "",
    "Los escalones no son estrictamente anidados: el embudo compara proporciones sobre la misma base, no personas que pasan de un escalón al siguiente."
  )
  invisible(funnel)
}
