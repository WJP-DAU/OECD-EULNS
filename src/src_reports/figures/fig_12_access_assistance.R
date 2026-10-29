## Figura 12 · Access to Appropriate Assistance and Representation in {country}
## Fuente de datos: get_results_table(ctx, "access2rep", ctype = "full") (module_2_2.R:14-29 → 2_2_1); UE eu_est(ctx,
##   "access2rep") = 37.68 → 38 (= R03 rep_c_published media_ue; data4web daría 54.2)
## Método UE: eu_est
## Código de origen: report_charts.R:131 chart_grouped ← gen_grouped_bars (worker-viz.R:248; barra UE cambiada
##   a mano con fix_orig.py)

fig_12 <- list(

  # Promedio nacional ponderado por NUTS por grupo (n < 30 → N/A; 8 en el país del reporte: 18-24 y 7 categorías). Media
  # UE = media simple de las estimaciones nacionales de los 27 países sobre ctx$master (eu_est, memorizada; el log da su
  # valor y el número de países con dato): la publicada (38 %) es la de access2rep desde los microdatos, que se puso a
  # mano en el SVG (fix_orig.py); la de data4web, que dibujó gen_grouped_bars, es otra (54.2).
  data = function(ctx) {
    # corregido (lote 20-27): con muestra pequeña, sin el grupo Category (worker-viz.R:236-237; small_sample)
    tbl <- get_results_table(ctx, target = "access2rep", ctype = "full", omit = if (small_sample(ctx)) "Category")
    eu <- eu_est(ctx, "access2rep")
    log_msg("fig 12: National = ", formatC(tbl$value[tbl$grouping == "National"], format = "f", digits = 2),
            " · UE eu_est(access2rep) = ", formatC(eu, format = "f", digits = 2), " · N/A = ", sum(is.na(tbl$value)))
    rows_grouped(tbl, eu = eu, decimals = 0)
  },

  # Dibujo: WJPr::wjp_groupbars() con sus argumentos (wjpr_grouped_bars, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_grouped(df, spec) de R/05_charts.R con metodo svg_r en el manifiesto.
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
