## Figura 23 · Costliness of Dispute Resolution Processes in {country}
## Fuente de datos: get_results_table(rp_cost, ctype full, omit Category) (module_2_4.R:86-101 → 2_4_4); UE
##   eu_value(data4web, rp_cost) = 82
## Método UE: data4web
## Código de origen: report_charts.R:131 chart_grouped ← gen_grouped_bars (worker-viz.R:523)

fig_23 <- list(

  # Spec: tipo y motor de dibujo, tamaño final (in), recuadro, columna de etiquetas y media UE (figures/_template.R)
  spec = list(tipo = "grouped_bars", metodo = "wjpr", w_in = 6.069, h_in = 5.486, callout = TRUE, label_col = "std",
              metodo_ue = "data4web", eu_id = "rp_cost",
              nota = "1 N/A"),

  # Promedio nacional ponderado por NUTS por grupo (n < 30 → N/A); sin el grupo Category (worker-viz.R:517) y, con
  # muestra pequeña, sin Age Group (:512); media UE de data4web.
  data = function(ctx) {
    tbl <- get_results_table(ctx, target = "rp_cost", ctype = "full",
                             omit = c("Category", if (small_sample(ctx)) "Age Group"))
    eu <- eu_value(ctx, "data4web", "rp_cost")
    log_msg("fig 23: National = ", formatC(tbl$value[tbl$grouping == "National"], format = "f", digits = 2),
            " · UE data4web(rp_cost) = ", formatC(eu, format = "f", digits = 2), " · N/A = ", sum(is.na(tbl$value)))
    rows_grouped(tbl, eu = eu, decimals = 0)
  },

  # Dibujo: WJPr::wjp_groupbars() con sus argumentos (wjpr_grouped_bars, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_grouped(df, spec) de R/05_charts.R con metodo svg_r en la spec.
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
