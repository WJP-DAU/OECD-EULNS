## Figura 21 · Perceived Speed of the Dispute Resolution Process in {country}
## Fuente de datos: get_results_table(rp_quick, ctype full, omit Category) (module_2_4.R:38-53 → 2_4_2)
## Método UE: none
## Código de origen: report_charts.R:131 chart_grouped ← gen_grouped_bars (worker-viz.R:461)

fig_21 <- list(

  # Promedio nacional ponderado por NUTS por grupo (n < 30 → N/A); sin el grupo Category (worker-viz.R:455) y, con
  # muestra pequeña, sin Age Group (:450); sin media UE.
  data = function(ctx) {
    tbl <- get_results_table(ctx, target = "rp_quick", ctype = "full",
                             omit = c("Category", if (small_sample(ctx)) "Age Group"))
    log_msg("fig 21: National = ", formatC(tbl$value[tbl$grouping == "National"], format = "f", digits = 2),
            " · N/A = ", sum(is.na(tbl$value)))
    rows_grouped(tbl, decimals = 0)
  },

  # Dibujo: WJPr::wjp_groupbars() con sus argumentos (wjpr_grouped_bars, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_grouped(df, spec) de R/05_charts.R con metodo svg_r en el manifiesto.
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
