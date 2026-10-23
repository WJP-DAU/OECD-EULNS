## Figura 09 · Access to Information and Advice in {country}
## Fuente de datos: get_results_table(ctx, "access2info", ctype = "full") (module_2_1.R:14-29 → 2_1_1);
##   UE eu_value(ctx, "data4web", "access2info") = 61
## Método UE: data4web
## Código de origen: report_charts.R:131 chart_grouped ← grouped_bars.R gen_grouped_bars (worker-viz.R:178)

fig_09 <- list(

  # Promedio nacional ponderado por NUTS por grupo (n < 30 → N/A); media UE de data4web (cacheada)
  data = function(ctx) {
    tbl <- get_results_table(ctx, target = "access2info", ctype = "full")
    rows_grouped(tbl, eu = eu_value(ctx, "data4web", "access2info"), decimals = 0)
  },

  plot = function(df, spec) chart_grouped(df, spec)
)
