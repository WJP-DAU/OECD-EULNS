## Figura 28 · Justice Gap Because of Unmet Civil and Administrative Justice Needs and Unresolved Justice Problems in {country}
## Fuente de datos: get_results_table(inside_justice_gap_keepdk, ctype short) (module_3.R:14-31 → 3_1); UE
##   eu_est(ctx, 'inside_justice_gap_keepdk') = 47.9 → 48
## Método UE: eu_est
## Código de origen: report_charts.R:131 chart_grouped ← viz_helpers.R:11 render_grouped (F07.R:14)

fig_28 <- list(

  # Spec: tipo y motor de dibujo, tamaño final (in), recuadro, columna de etiquetas y media UE (figures/_template.R)
  spec = list(tipo = "grouped_bars", metodo = "wjpr", w_in = 6.069, h_in = 4.319, callout = TRUE, label_col = "std",
              metodo_ue = "eu_est", eu_id = "inside_justice_gap_keepdk"),

  # Promedio nacional ponderado por NUTS por grupo (n < 30 → N/A); dentro de la brecha de justicia (keepDK: el no sabe
  # cuenta como necesidad no cubierta; a2j_score < 0.67); media UE = media simple de las estimaciones nacionales
  # (eu_est, 47.9 → 48 %).
  data = function(ctx) {
    tbl <- get_results_table(ctx, target = "inside_justice_gap_keepdk", ctype = "short")
    eu <- eu_est(ctx, "inside_justice_gap_keepdk")
    log_msg("fig 28: National = ", formatC(tbl$value[tbl$grouping == "National"], format = "f", digits = 2),
            " · UE eu_est(inside_justice_gap_keepdk) = ", formatC(eu, format = "f", digits = 2),
            " · N/A = ", sum(is.na(tbl$value)))
    rows_grouped(tbl, eu = eu, decimals = 0)
  },

  # Dibujo: WJPr::wjp_groupbars() con sus argumentos (wjpr_grouped_bars, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_grouped(df, spec) de R/05_charts.R con metodo svg_r en la spec.
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
