## A01 · Tasa de resolución según el acceso en cada escalón ------------------------------------------------

run_A01 <- function(af) {

  pairs <- c("Information and advice" = "access2info", "Appropriate assistance" = "rep_c_published", "Dispute resolution mechanism" = "drm_c_published")

  res <- imap_dfr(pairs, function(v, lab) {
    bind_rows(
      diff_stratified(af, "resolved", v),
      diff_stratified(af, "resolved", v, "financial") %>% filter(estrato != "Sin control"),
      diff_stratified(af, "resolved", v, "cooc3") %>% filter(estrato != "Sin control")
    ) %>% mutate(escalon = lab, .before = 1)
  })
  save_table(res, "A01")

  raw <- res %>% filter(estrato == "Sin control")
  report_bars(
    raw %>% select(escalon, con, sin) %>%
      pivot_longer(c(con, sin), names_to = "k", values_to = "pct") %>%
      transmute(grouping = escalon, category = if_else(k == "con", "With access", "Without access"), pct),
    "A01_resolucion_segun_acceso"
  )

  save_note(
    "A01",
    "# A01 · Tasa de resolución según el acceso en cada escalón",
    "",
    "**Resultado:** problema totalmente resuelto (`rp_outcome`), entre quienes terminaron el proceso. **Bases (R03):** información = con problema no trivial; asistencia y DRM = variante (c), quienes lo necesitaron. **Ponderación del reporte**; celdas con n < 30 en una región quedan suprimidas.",
    "",
    md_table(res),
    "",
    paste0("**Lectura:** con acceso a información el problema se resolvió ", round(raw$diferencia[1]), " puntos más que sin él; con asistencia apropiada, ",
           round(raw$diferencia[2]), " puntos más; con acceso a un DRM, ", round(raw$diferencia[3]), " puntos. Las diferencias de información y asistencia se mantienen al estratificar por situación financiera y número de problemas (filas «Estandarizada»); en DRM no hay diferencia y los estratos son chicos."),
    "",
    paste0("> ", caveat_association)
  )
  invisible(res)
}
