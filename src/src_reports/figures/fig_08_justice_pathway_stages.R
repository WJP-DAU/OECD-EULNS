## Figura 08 · Stages of the Justice Pathway in {country}
## Fuente de datos: nat_est(ctx, "access2info") (= fila National de get_results_table, 2_1_1: 61.46) + journey(ctx)
##   rep_c_published (31.93) y drm_c_published (28.47); text fmt_num(value, 0); frac = value/max (la dibuja el adaptador)
## Método UE: none
## Código de origen: report_charts.R:284 chart_funnel + standard_figures.R:108-119 funnel_rows('pathway')
##   (diseño charts5.py:119 funnel)

# Etapas del embudo, de arriba abajo: rótulo impreso (texto fijo del diseño, con "\n" donde la figura corta la línea)
# = variable de la que sale el valor. origen: standard_figures.R:110-113 (funnel_rows('pathway'): label y v)
fig08_stages <- c(
  "Had access to information and advice" = "access2info",
  "Obtained appropriate assistance\nwhen they needed it" = "rep_c_published",
  "Turned to a dispute resolution\nmechanism when they needed it" = "drm_c_published"
)

fig_08 <- list(

  # Promedios nacionales ponderados por NUTS (n < 30 → NA → "N/A"): las variables del recorrido salen de journey(ctx)
  # (memorizado; R03.R:19, 33: variantes publicadas, sin "no lo necesitó" ni "razón desconocida") y access2info de
  # nat_est, el estimador de la fila National de la tabla 2_1_1 (standard_figures.R:113 la leía de ese CSV). Una fila
  # por etapa (kind stage) en el orden del diseño (rows_funnel, R/05_charts.R); frac = value / max(value) no va en el
  # df: la calcula wjpr_funnel().
  data = function(ctx) {
    j <- journey(ctx)
    v <- unname(vapply(fig08_stages, function(k) j[[k]] %||% nat_est(ctx, k)$value[[1]], numeric(1)))
    log_msg("fig 08: ", paste0(fig08_stages, " = ", formatC(v, format = "f", digits = 2), collapse = " · "))
    rows_funnel(stats::setNames(v, names(fig08_stages)), decimals = 0)
  },

  # Dibujo: WJPr::wjp_funnel() con sus argumentos (wjpr_funnel, R/09_wjpr.R; WJPr 1.2.0): barras centradas de ancho
  # value / max, conectores y rótulos a la izquierda con el salto de línea del df. Respaldo sin WJPr: chart_funnel(df,
  # spec) con metodo svg_r (pendiente en R/05_charts.R).
  plot = function(df, spec) wjpr_funnel(df, spec)
)
