## A04 · Confianza y exposición al sistema ------------------------------------------------------------------

exposure_ladder <- function(af) {
  af %>% mutate(
    exposicion = case_when(
      !has_nt ~ "No non-trivial problem",
      sankey_drm_stage_1 == "Did not need to turn to a DRM" ~ "Problem, did not need a DRM",
      sankey_drm_stage_1 == "Needed but did not have access to a DRM" ~ "Needed a DRM, no access",
      sankey_drm_stage_1 == "Turned to a DRM" ~ "Needed a DRM, had access",
      TRUE ~ NA_character_   # unknown reason (decisión: NA) or AJR_resolution = 98 (no DRM stage)
    )
  )
}
ladder_levels <- c("No non-trivial problem", "Problem, did not need a DRM", "Needed a DRM, no access", "Needed a DRM, had access")

run_A04 <- function(af) {

  d <- exposure_ladder(af)
  ladder <- imap_dfr(trust_actors, \(v, lab) xt(d, paste0("trust_", v), "exposicion", lab, "Escalera de exposición")) %>%
    rename(actor = desenlace) %>%
    mutate(grupo = factor(grupo, ladder_levels)) %>% arrange(actor, grupo) %>% mutate(grupo = as.character(grupo))
  penalty <- imap_dfr(trust_actors, function(v, lab) {
    xt(d %>% mutate(prob = if_else(has_nt, "Yes", "No")), paste0("trust_", v), "prob", lab, "Problema no trivial")
  }) %>%
    group_by(desenlace) %>%
    summarise(sin_problema = pct[grupo == "No"], con_problema = pct[grupo == "Yes"], penalizacion = con_problema - sin_problema,
              n_sin = n[grupo == "No"], n_con = n[grupo == "Yes"], .groups = "drop") %>%
    rename(actor = desenlace)
  sizes <- d %>% count(exposicion) %>% mutate(pct_muestra = 100 * n / sum(n))
  save_table(ladder, "A04")
  save_table(penalty, "A04", "penalizacion")

  report_bars(ladder %>% transmute(grouping = actor, category = grupo, pct), "A04_confianza_escalera_exposicion")

  s34 <- ladder %>% filter(grupo %in% ladder_levels[3:4]) %>% select(actor, grupo, pct) %>% pivot_wider(names_from = grupo, values_from = pct)
  save_note(
    "A04",
    "# A04 · Confianza y exposición al sistema",
    "",
    "**Escalera:** (1) sin problema no trivial · (2) con problema, no necesitó un DRM · (3) necesitó un DRM y no tuvo acceso · (4) necesitó un DRM y sí tuvo acceso. «Razón desconocida» = NA (decisión del 28 sep); también quedan fuera quienes no saben si acudieron a un DRM (`AJR_resolution = 98`). Confianza = mucha + algo. **Ponderación del reporte.**",
    "",
    "Tamaños:",
    "",
    md_table(sizes),
    "",
    md_table(ladder),
    "",
    "## Escalón 3 contra 4",
    "",
    md_table(s34),
    "",
    "## Penalización por haber tenido un problema (reproduce las Figuras 30-32)",
    "",
    md_table(penalty),
    "",
    paste0("**Lectura:** la confianza cae sobre todo entre quienes necesitaron un mecanismo y no tuvieron acceso (jueces ",
           round(s34[[ladder_levels[3]]][s34$actor == "Judges"]), " % contra ", round(s34[[ladder_levels[4]]][s34$actor == "Judges"]),
           " % de quienes sí accedieron). La penalización por tener un problema es parecida en los tres actores, lo que apunta al sistema y no a un actor."),
    "",
    "El escalón 1 sale de la población general; los otros tres, de quienes tuvieron un problema. El escalón 4 alcanza para una estimación puntual, no para subdividirlo."
  )
  invisible(ladder)
}
