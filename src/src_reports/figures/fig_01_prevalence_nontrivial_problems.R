## Figura 01 · Prevalence of Non-Trivial Justice Problems in {country}
## Fuente de datos: get_results_table(ctx, "prevalence2", ctype = "short") (module_1.R:210-227 → 1_2);
##   UE eu_value(ctx, "data4web", "prevalence2") = 49
## Método UE: data4web
## Código de origen: report_charts.R:131 chart_grouped ← grouped_bars.R gen_grouped_bars (worker-viz.R:100)

fig_01 <- list(

  # Promedio nacional ponderado por NUTS por grupo (n < 30 → N/A); media UE de data4web (cacheada)
  data = function(ctx) {
    tbl <- get_results_table(ctx, target = "prevalence2", ctype = "short")
    rows_grouped(tbl, eu = eu_value(ctx, "data4web", "prevalence2"), decimals = 0)
  },

  # Dibujo: WJPr::wjp_groupbars() con sus argumentos (wjpr_grouped_bars, R/09_wjpr.R; WJPr 1.2.0)
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
