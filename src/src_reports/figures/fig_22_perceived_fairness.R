## Figura 22 · Perceived Fairness of the Dispute Resolution Process in {country}
## Fuente de datos: get_results_table(rp_fair, ctype full, omit Category) (module_2_4.R:62-77 → 2_4_3); UE
##   eu_value(data4web, rp_fair) = 54 (eu_est daría 53)
## Método UE: data4web
## Código de origen: report_charts.R:131 chart_grouped ← gen_grouped_bars (worker-viz.R:491)

fig_22 <- list(

  # Promedio nacional ponderado por NUTS por grupo (n < 30 → N/A); sin el grupo Category (worker-viz.R:485) y, con
  # muestra pequeña, sin Age Group (:480); media UE de data4web (53.52 → 54 %, decisión del 22-oct; eu_est daría 53).
  data = function(ctx) {
    tbl <- get_results_table(ctx, target = "rp_fair", ctype = "full",
                             omit = c("Category", if (small_sample(ctx)) "Age Group"))
    eu <- eu_value(ctx, "data4web", "rp_fair")
    log_msg("fig 22: National = ", formatC(tbl$value[tbl$grouping == "National"], format = "f", digits = 2),
            " · UE data4web(rp_fair) = ", formatC(eu, format = "f", digits = 2), " · N/A = ", sum(is.na(tbl$value)))
    rows_grouped(tbl, eu = eu, decimals = 0)
  },

  # Dibujo: WJPr::wjp_groupbars() con sus argumentos (wjpr_grouped_bars, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_grouped(df, spec) de R/05_charts.R con metodo svg_r en el manifiesto.
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
