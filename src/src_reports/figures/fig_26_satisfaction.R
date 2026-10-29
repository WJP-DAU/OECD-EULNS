## Figura 26 · Satisfaction with the Resolution Outcome for People's Justice Problems in {country}
## Fuente de datos: get_results_table(rp_satisfaction, ctype custom_1 = short + problem_status +
##   resolution_favor; utils.R:244-254) (module_2_5.R:38-55 → 2_5_2)
## Método UE: none
## Código de origen: report_charts.R:131 chart_grouped ← gen_grouped_bars (worker-viz.R:628)

fig_26 <- list(

  # Promedio nacional ponderado por NUTS por grupo (n < 30 → N/A); grupos custom_1 = short + Problem Status + Resolution
  # (utils.R:244-254); con muestra pequeña, sin Age Group ni Problem Status (worker-viz.R:617); sin media UE ni relabel.
  data = function(ctx) {
    tbl <- get_results_table(ctx, target = "rp_satisfaction", ctype = "custom_1",
                             omit = if (small_sample(ctx)) c("Age Group", "Problem Status"))
    log_msg("fig 26: National = ", formatC(tbl$value[tbl$grouping == "National"], format = "f", digits = 2),
            " · N/A = ", sum(is.na(tbl$value)))
    rows_grouped(tbl, decimals = 0)
  },

  # Dibujo: WJPr::wjp_groupbars() con sus argumentos (wjpr_grouped_bars, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_grouped(df, spec) de R/05_charts.R con metodo svg_r en el manifiesto.
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
