## Figura 27 · Outcome of People's Justice Problems in {country}, by Access at Each Stage of the Justice Pathway
## Fuente de datos: diff_stratified(rp_outcome, acceso en cada etapa: access2info, rep_c_published,
##   drm_c_published) fila 'Sin control' (A01.R:3-38; analysis_helpers.R:28-51; build_analysis_frame). Excepción al
##   bloque grouped (revisión): sin filas avg (ni National ni EU) y sin rows_grouped(): data() arma el df aquí, por
##   etapa (Information and advice, Appropriate assistance, Dispute resolution mechanism) su header y las filas
##   "With access" y "Without access", en ese orden (reference/<slug>/Figure_27_outcome_by_access.csv)
## Método UE: none
## Código de origen: report_charts.R:131 chart_grouped(df, spec, st = ST_A01, fill_w = spec$w_in * 72): la pista
##   llena los 6.5 in (fill_w = W_CALLOUT, standard_figures.R:277-278, 471) ← viz_helpers.R:57 report_bars (A01.R:17)

# Etapas del recorrido (rótulo impreso del encabezado = variable de acceso), en el orden publicado. origen: A01.R:5 (pairs)
fig27_stages <- c(
  "Information and advice" = "access2info",
  "Appropriate assistance" = "rep_c_published",
  "Dispute resolution mechanism" = "drm_c_published"
)

fig_27 <- list(

  # Por etapa, el problema totalmente resuelto (resolved = rp_outcome, add_report_vars) entre quienes tuvieron acceso (1)
  # y quienes no (0): fila «Sin control» de diff_stratified() (A01.R:9 y 17: report_bars con con/sin, sin estratos), con
  # el promedio NUTS del reporte (n < 30 → N/A). Sin filas avg: cada etapa es un header y sus filas With/Without access.
  data = function(ctx) {
    r <- imap_dfr(fig27_stages, function(v, lab) {
      d <- diff_stratified(ctx, "resolved", v)
      tibble(grouping = lab, kind = c("header", "row", "row"), label = c(lab, "With access", "Without access"),
             value = c(NA_real_, d$con[1], d$sin[1]))
    })
    log_msg("fig 27: ", paste(sprintf("%s %s %.2f", r$grouping[r$kind == "row"], r$label[r$kind == "row"],
                                      r$value[r$kind == "row"]), collapse = " · "))
    r %>% transmute(order = row_number(), kind, grouping, label, value, value2 = NA_real_,
                    text = if_else(kind == "header", "", fmt_num(value, 0)), text2 = "")
  },

  # Dibujo: WJPr::wjp_groupbars() con sus argumentos (wjpr_grouped_bars, R/09_wjpr.R; WJPr 1.2.0): la 27 sin filas de
  # promedio y con la pista de 6.5 in (ST_A01). Respaldo sin WJPr: chart_grouped(df, spec) de R/05_charts.R (svg_r).
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
