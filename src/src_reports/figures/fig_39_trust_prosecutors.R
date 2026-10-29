## Figura 39 · Public Trust in Prosecutors in {country}
## Fuente de datos: trust(TRT_prosecutors bin 1-2; ctype trust) → 5_1_trust_prosecutors; UE eu_value(data4web,
##   TRT_prosecutors) = 57
## Método UE: data4web
## Código de origen: report_charts.R:131 chart_grouped ← gen_grouped_bars (worker-viz.R:838)

fig_39 <- list(

  # Promedio nacional ponderado por NUTS por grupo (n < 30 → N/A); 1-2 = confía (binary_agree); grupos trust, como la
  # 38; media UE de data4web.
  data = function(ctx) {
    tbl <- get_results_table(ctx, target = "TRT_prosecutors", ctype = "trust", transform = binary_agree)
    eu <- eu_value(ctx, "data4web", "TRT_prosecutors")
    log_msg("fig 39: National = ", formatC(tbl$value[tbl$grouping == "National"], format = "f", digits = 2),
            " · UE data4web(TRT_prosecutors) = ", formatC(eu, format = "f", digits = 2),
            " · N/A = ", sum(is.na(tbl$value)))
    rows_grouped(tbl, eu = eu, decimals = 0)
  },

  # Dibujo: WJPr::wjp_groupbars() con sus argumentos (wjpr_grouped_bars, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_grouped(df, spec) de R/05_charts.R con metodo svg_r en el manifiesto.
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
