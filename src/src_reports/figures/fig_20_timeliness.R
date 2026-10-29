## Figura 20 · Dispute Resolution Processes Finalized in Less Than One Year in {country}
## Fuente de datos: get_results_table(rp_time, ctype full, omit Category) (module_2_4.R:14-29 → 2_4_1); UE
##   eu_value(data4web, rp_time) = 74
## Método UE: data4web
## Código de origen: report_charts.R:131 chart_grouped ← gen_grouped_bars (worker-viz.R:429)

fig_20 <- list(

  # Promedio nacional ponderado por NUTS por grupo (n < 30 → N/A); sin el grupo Category (omit.cats de worker-viz.R:423)
  # y, con muestra pequeña, sin Age Group (:418); media UE de data4web. «6 or more problems» (la de las figuras finales
  # del 16-oct; v9 imprimía «5 or more») sale de 02_variables.R.
  data = function(ctx) {
    tbl <- get_results_table(ctx, target = "rp_time", ctype = "full",
                             omit = c("Category", if (small_sample(ctx)) "Age Group"))
    eu <- eu_value(ctx, "data4web", "rp_time")
    log_msg("fig 20: National = ", formatC(tbl$value[tbl$grouping == "National"], format = "f", digits = 2),
            " · UE data4web(rp_time) = ", formatC(eu, format = "f", digits = 2), " · N/A = ", sum(is.na(tbl$value)))
    rows_grouped(tbl, eu = eu, decimals = 0)
  },

  # Dibujo: WJPr::wjp_groupbars() con sus argumentos (wjpr_grouped_bars, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_grouped(df, spec) de R/05_charts.R con metodo svg_r en el manifiesto.
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
