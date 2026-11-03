## Figura 38 · Public Trust in Judges in {country}
## Fuente de datos: trust(TRT_judges bin 1-2; ctype trust = short + Problem Status + Non-Trivial Problem
##   Experienced; non_trivial_problem_broader NA → 'No', module_5.R:15-21) → 5_1_trust_judges; UE
##   eu_value(data4web, TRT_judges) = 57
## Método UE: data4web
## Código de origen: report_charts.R:131 chart_grouped ← gen_grouped_bars (worker-viz.R:817; module_5.R:14-62)

fig_38 <- list(

  # Spec: tipo y motor de dibujo, tamaño final (in), recuadro, columna de etiquetas y media UE (figures/_template.R)
  spec = list(tipo = "grouped_bars", metodo = "wjpr", w_in = 6.069, h_in = 5.792, callout = TRUE, label_col = "std",
              metodo_ue = "data4web", eu_id = "TRT_judges"),

  # Promedio nacional ponderado por NUTS por grupo (n < 30 → N/A); 1-2 = confía (binary_agree; module_5.R:46-49); grupos
  # trust = short + Problem Status + Non-Trivial Problem Experienced (non_trivial_problem_broader de 01_load.R, NA →
  # «No», como module_5.R:15-21); media UE de data4web.
  data = function(ctx) {
    tbl <- get_results_table(ctx, target = "TRT_judges", ctype = "trust", transform = binary_agree)
    eu <- eu_value(ctx, "data4web", "TRT_judges")
    log_msg("fig 38: National = ", formatC(tbl$value[tbl$grouping == "National"], format = "f", digits = 2),
            " · UE data4web(TRT_judges) = ", formatC(eu, format = "f", digits = 2), " · N/A = ", sum(is.na(tbl$value)))
    rows_grouped(tbl, eu = eu, decimals = 0)
  },

  # Dibujo: WJPr::wjp_groupbars() con sus argumentos (wjpr_grouped_bars, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_grouped(df, spec) de R/05_charts.R con metodo svg_r en la spec.
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
