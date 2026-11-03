## Figura 17 · Access to DRMs in {country}
## Fuente de datos: get_results_table(ctx, "access2drm", ctype = "full", omit = "Category") (module_2_3.R:14-29 →
##   2_3_1); UE eu_value(ctx, "data4web", "access2drm") = 40
## Método UE: data4web
## Código de origen: report_charts.R:131 chart_grouped ← gen_grouped_bars (worker-viz.R:352; omit.cats = "Category")

fig_17 <- list(

  # Spec: tipo y motor de dibujo, tamaño final (in), recuadro, columna de etiquetas y media UE (figures/_template.R)
  spec = list(tipo = "grouped_bars", metodo = "wjpr", w_in = 6.069, h_in = 5.486, callout = TRUE, label_col = "std",
              metodo_ue = "data4web", eu_id = "access2drm",
              nota = "3 N/A (25-34, 18-24, 1 problem)"),

  # Promedio nacional ponderado por NUTS por grupo (n < 30 → N/A; 3 en el país del reporte: 25-34, 18-24 y 1 problem),
  # sin el grupo Category (omit.cats de worker-viz.R:346 en la llamada publicada); media UE de data4web (cacheada en
  # reference/<slug>/eu_values.csv).
  data = function(ctx) {
    # corregido (lote 20-27): con muestra pequeña, también sin Age Group (worker-viz.R:340-341; small_sample)
    tbl <- get_results_table(ctx, target = "access2drm", ctype = "full",
                             omit = c("Category", if (small_sample(ctx)) "Age Group"))
    eu <- eu_value(ctx, "data4web", "access2drm")
    log_msg("fig 17: National = ", formatC(tbl$value[tbl$grouping == "National"], format = "f", digits = 2),
            " · UE data4web(access2drm) = ", formatC(eu, format = "f", digits = 2), " · N/A = ", sum(is.na(tbl$value)))
    rows_grouped(tbl, eu = eu, decimals = 0)
  },

  # Dibujo: WJPr::wjp_groupbars() con sus argumentos (wjpr_grouped_bars, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_grouped(df, spec) de R/05_charts.R con metodo svg_r en la spec.
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
