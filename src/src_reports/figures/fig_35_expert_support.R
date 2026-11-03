## Figura 35 · Perceived Availability of Expert Support in {country}
## Fuente de datos: get_results_table(AJE_advice bin, ctype short) (module_4.R:152-173 → 4_3); UE eu_est(ctx,
##   'AJE_advice', binary_agree) = 49.43 → 49 (F10.R:43,65)
## Método UE: eu_est
## Código de origen: report_charts.R:131 chart_grouped ← render_grouped (F10.R:45)

fig_35 <- list(

  # Spec: tipo y motor de dibujo, tamaño final (in), recuadro, columna de etiquetas y media UE (figures/_template.R)
  spec = list(tipo = "grouped_bars", metodo = "wjpr", w_in = 6.069, h_in = 4.319, callout = TRUE, label_col = "std",
              metodo_ue = "eu_est", eu_id = "AJE_advice"),

  # Promedio nacional ponderado por NUTS por grupo (n < 30 → N/A); 1-2 = cree tener a quién pedir apoyo experto
  # (binary_agree; module_4.R:158-161); media UE eu_est con la misma transformación (F10.R:43, 65: 49.43 → 49 %).
  data = function(ctx) {
    tbl <- get_results_table(ctx, target = "AJE_advice", ctype = "short", transform = binary_agree)
    eu <- eu_est(ctx, "AJE_advice", binary_agree)
    log_msg("fig 35: National = ", formatC(tbl$value[tbl$grouping == "National"], format = "f", digits = 2),
            " · UE eu_est(AJE_advice, binary_agree) = ", formatC(eu, format = "f", digits = 2),
            " · N/A = ", sum(is.na(tbl$value)))
    rows_grouped(tbl, eu = eu, decimals = 0)
  },

  # Dibujo: WJPr::wjp_groupbars() con sus argumentos (wjpr_grouped_bars, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_grouped(df, spec) de R/05_charts.R con metodo svg_r en la spec.
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
