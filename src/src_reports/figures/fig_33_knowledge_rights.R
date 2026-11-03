## Figura 33 · Knowledge of Legal Rights and Responsibilities in {country}
## Fuente de datos: get_results_table(AJE_legalrights bin 1-2, ctype short) (module_4.R:14-35 → 4_1); UE
##   eu_est(ctx, 'AJE_legalrights', binary_agree) = 72.6 → 73 (F10.R:42,64)
## Método UE: eu_est
## Código de origen: report_charts.R:131 chart_grouped ← render_grouped (F10.R:44)

fig_33 <- list(

  # Spec: tipo y motor de dibujo, tamaño final (in), recuadro, columna de etiquetas y media UE (figures/_template.R)
  spec = list(tipo = "grouped_bars", metodo = "wjpr", w_in = 6.069, h_in = 4.319, callout = TRUE, label_col = "std",
              metodo_ue = "eu_est", eu_id = "AJE_legalrights"),

  # Promedio nacional ponderado por NUTS por grupo (n < 30 → N/A); 1-2 = conoce sus derechos (binary_agree: 1, 2 → 1; 3,
  # 4, 98 → 0; 99 → NA; module_4.R:20-23); media UE eu_est con la misma transformación (F10.R:42, 64: 72.6 → 73 %).
  data = function(ctx) {
    tbl <- get_results_table(ctx, target = "AJE_legalrights", ctype = "short", transform = binary_agree)
    eu <- eu_est(ctx, "AJE_legalrights", binary_agree)
    log_msg("fig 33: National = ", formatC(tbl$value[tbl$grouping == "National"], format = "f", digits = 2),
            " · UE eu_est(AJE_legalrights, binary_agree) = ", formatC(eu, format = "f", digits = 2),
            " · N/A = ", sum(is.na(tbl$value)))
    rows_grouped(tbl, eu = eu, decimals = 0)
  },

  # Dibujo: WJPr::wjp_groupbars() con sus argumentos (wjpr_grouped_bars, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_grouped(df, spec) de R/05_charts.R con metodo svg_r en la spec.
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
