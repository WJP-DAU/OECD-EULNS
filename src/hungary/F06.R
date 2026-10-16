## F06 · Percepción contra experiencia, en una sola figura (y dumbbells corregidos) ------------------------

perception_experience <- function(df, perc_var, exp_var) {
  cuts <- c("Gender" = "gender", "Age Group" = "age_group", "Financial Situation" = "financial", "Area of Residence" = "residence")
  nat <- tibble(grouping = "", category = "National Average",
                perception = nat_est(df, perc_var, binary_agree)$pct, experience = nat_est(df, exp_var)$pct)
  by_cut <- imap_dfr(cuts, function(v, lab) {
    left_join(
      nat_est(df, perc_var, binary_agree, by = v) %>% select(category = all_of(v), perception = pct),
      nat_est(df, exp_var, by = v) %>% select(category = all_of(v), experience = pct),
      by = "category"
    ) %>% mutate(grouping = lab)
  })
  bind_rows(nat, by_cut) %>% mutate(brecha = experience - perception)
}

run_F06 <- function() {

  hj <- journey_vars(hu)
  info <- perception_experience(hj, "JSE_access2info", "access2info")
  assis <- perception_experience(hj, "JSE_access2assis", "rep_c_published")
  both <- bind_rows(info %>% mutate(dimension = "Information and advice"), assis %>% mutate(dimension = "Assistance and representation"))
  save_table(both, "F06")

  # CORRECTIONS: Figures 6 and 11, same layout, with the population of each series in the legend
  report_dumbbell(info, "Hungary_access2information_dumbbell")
  report_dumbbell(assis, "Hungary_access2representation_dumbbell")
  file.remove(file.path(OUT, c("Hungary_access2information_dumbbell.png", "Hungary_access2representation_dumbbell.png")))

  nat <- both %>% filter(grouping == "")
  save_note(
    "F06",
    "# F06 · Percepción contra experiencia, en una sola figura",
    "",
    "**Poblaciones (distintas por diseño):** percepción = población general (`JSE_access2info`, `JSE_access2assis`; muy de acuerdo + de acuerdo). Experiencia = entrevistados con problema no trivial (`access2info`; `access2rep` publicado, que excluye a quienes no necesitaron asistencia). **Ponderación del reporte.**",
    paste0("Tamaños (nacional): percepción de información n = ", nat_est(hj, "JSE_access2info", binary_agree)$n, "; percepción de asistencia n = ", nat_est(hj, "JSE_access2assis", binary_agree)$n,
           " (submuestra de la población general); experiencia de información n = ", nat_est(hj, "access2info")$n, "; experiencia de asistencia n = ", nat_est(hj, "rep_c_published")$n, "."),
    "",
    md_table(both %>% select(dimension, grouping, category, perception, experience, brecha)),
    "",
    "## Archivos",
    "",
    "- `Hungary_access2information_dumbbell.svg`, `Hungary_access2representation_dumbbell.svg` — CORRECCIONES de las Figuras 6 y 11: mismo formato, leyenda con la población de cada serie.",
    "",
    "## Lectura",
    "",
    paste0("Información: ", round(nat$perception[1]), " % contra ", round(nat$experience[1]), " % (", sprintf("%+.0f", nat$brecha[1]), " puntos). ",
           "Asistencia: ", round(nat$perception[2]), " % contra ", round(nat$experience[2]), " % (", sprintf("%+.0f", nat$brecha[2]), " puntos). ",
           "La asimetría se confirma: la gente es pesimista donde el sistema funciona y acierta donde falla.")
  )
  invisible(both)
}
