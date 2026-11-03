## Figura 31 · At Least One Hardship in {country}
## Fuente de datos: get_results_table(hardship, ctype full, omit Category) (F07.R:30); UE eu_est(ctx,
##   'hardship') = 45.8 → 46
## Método UE: eu_est
## Código de origen: report_charts.R:131 chart_grouped ← viz_helpers.R:11 render_grouped (F07.R:30)

fig_31 <- list(

  # Spec: tipo y motor de dibujo, tamaño final (in), recuadro, columna de etiquetas y media UE (figures/_template.R)
  spec = list(tipo = "grouped_bars", metodo = "wjpr", w_in = 6.069, h_in = 5.486, callout = TRUE, label_col = "std",
              metodo_ue = "eu_est", eu_id = "hardship"),

  # Promedio nacional ponderado por NUTS por grupo (n < 30 → N/A); al menos una dificultad por el problema; sin el grupo
  # Category (F07.R:30); media UE eu_est (45.8 → 46 %).
  data = function(ctx) {
    tbl <- get_results_table(ctx, target = "hardship", ctype = "full", omit = "Category")
    eu <- eu_est(ctx, "hardship")
    log_msg("fig 31: National = ", formatC(tbl$value[tbl$grouping == "National"], format = "f", digits = 2),
            " · UE eu_est(hardship) = ", formatC(eu, format = "f", digits = 2), " · N/A = ", sum(is.na(tbl$value)))
    rows_grouped(tbl, eu = eu, decimals = 0)
  },

  # Dibujo: WJPr::wjp_groupbars() con sus argumentos (wjpr_grouped_bars, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_grouped(df, spec) de R/05_charts.R con metodo svg_r en la spec.
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
