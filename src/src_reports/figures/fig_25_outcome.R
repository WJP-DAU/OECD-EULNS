## Figura 25 · Outcome of People's Justice Problems in {country}
## Fuente de datos: get_results_table(rp_outcome, ctype full, omit Category) (module_2_5.R:14-29 → 2_5_1); UE
##   eu_value(data4web, rp_outcome) = 72
## Método UE: data4web
## Código de origen: report_charts.R:131 chart_grouped ← gen_grouped_bars (worker-viz.R:596)

fig_25 <- list(

  # Promedio nacional ponderado por NUTS por grupo (n < 30 → N/A); sin el grupo Category (worker-viz.R:590) y, con
  # muestra pequeña, sin Age Group (:585); media UE de data4web.
  data = function(ctx) {
    tbl <- get_results_table(ctx, target = "rp_outcome", ctype = "full",
                             omit = c("Category", if (small_sample(ctx)) "Age Group"))
    eu <- eu_value(ctx, "data4web", "rp_outcome")
    log_msg("fig 25: National = ", formatC(tbl$value[tbl$grouping == "National"], format = "f", digits = 2),
            " · UE data4web(rp_outcome) = ", formatC(eu, format = "f", digits = 2), " · N/A = ", sum(is.na(tbl$value)))
    rows_grouped(tbl, eu = eu, decimals = 0)
  },

  # Dibujo: WJPr::wjp_groupbars() con sus argumentos (wjpr_grouped_bars, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_grouped(df, spec) de R/05_charts.R con metodo svg_r en el manifiesto.
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
