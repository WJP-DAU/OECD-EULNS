## Figura 27 · Outcome of People's Justice Problems in {country}, by Access at Each Stage of the Justice Pathway
## Fuente de datos: diff_stratified(rp_outcome, acceso en cada etapa: access2info, rep_c_published,
##   drm_c_published) fila 'Sin control' (A01.R:3-38; analysis_helpers.R:28-51; build_analysis_frame). Excepción al
##   bloque grouped (revisión): sin filas avg (ni National ni EU) y sin rows_grouped(): data() arma el df aquí, por
##   etapa (Information and advice, Appropriate assistance, Dispute resolution mechanism) su header y las filas
##   "With access" y "Without access", en ese orden (reference/<slug>/Figure_27_outcome_by_access.csv)
## Método UE: none
## Código de origen: report_charts.R:131 chart_grouped(df, spec, st = ST_A01, fill_w = spec$w_in * 72): la pista
##   llena los 6.5 in (fill_w = W_CALLOUT, standard_figures.R:277-278, 471) ← viz_helpers.R:57 report_bars (A01.R:17)
## Estado: pendiente (esqueleto de la Etapa 1; contrato en figures/_template.R)

fig_27 <- list(
  data = function(ctx) stop("pendiente: fig_27 sin implementar", call. = FALSE),
  plot = function(df, spec) stop("pendiente: fig_27 sin implementar", call. = FALSE)
)
