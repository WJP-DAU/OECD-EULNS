## Figura 37 · Legal Documentation in {country}
## Fuente de datos: vulnerability1 (identity), vulnerability3 (work agreement), vulnerability2 (housing)
##   National + eu_est (F10.R:55); orden impreso 1, 3, 2; etiquetas de F10_seccion4
## Método UE: eu_est
## Código de origen: report_charts.R:247 chart_columns (ancho W_hbars = 172+12+300+34+6 pt;
##   standard_figures.R:474) ← paired_bars (F10.R:60)

# Documentos en el orden impreso (1, 3, 2) = variable. origen: F10.R:54 (docs)
fig37_docs <- c("Official proof of identity" = "vulnerability1", "Written work agreement" = "vulnerability3",
                "Proof of housing or land tenure" = "vulnerability2")

fig_37 <- list(

  # Spec: tipo y motor de dibujo, tamaño final (in), recuadro, columna de etiquetas y media UE (figures/_template.R)
  spec = list(tipo = "columns", metodo = "wjpr", w_in = 7.278, h_in = 3.34, callout = FALSE, label_col = "none",
              metodo_ue = "eu_est", eu_id = "vulnerability1;vulnerability3;vulnerability2",
              nota = "columnas verticales; mismo ancho que la 36"),

  # País = promedio nacional ponderado por NUTS (nat_est; n < 30 → N/A), UE = media simple de las estimaciones
  # nacionales (eu_est), como F10.R:55-59. Orden fijo del diseño (rows_columns, R/05_charts.R).
  data = function(ctx) {
    hu <- vapply(fig37_docs, \(v) nat_est(ctx, v)$value[[1]], numeric(1))
    eu <- vapply(fig37_docs, \(v) eu_est(ctx, v), numeric(1))
    log_msg("fig 37: ", paste(sprintf("%s %.2f / UE %.2f", names(hu), hu, eu), collapse = " · "))
    rows_columns(names(fig37_docs), hu, eu, decimals = 0)
  },

  # Dibujo: WJPr::wjp_bars() vertical con sus argumentos (wjpr_columns, R/09_wjpr.R; WJPr 1.2.0), del ancho de las
  # barras horizontales de la 36. Respaldo sin WJPr: chart_columns(df, spec) (sin portar: error legible, R/05_charts.R).
  plot = function(df, spec) wjpr_columns(df, spec)
)
