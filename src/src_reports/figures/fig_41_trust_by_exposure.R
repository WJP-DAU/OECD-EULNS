## Figura 41 · Public Trust in Justice Actors in {country}, by Exposure to the Justice System
## Fuente de datos: xt(ctx, a, "exposicion") para a = trust_TRT_judges, trust_TRT_prosecutors, trust_TRT_pda
##   (A04.R:16-21; nat_est); exposicion es la columna de add_report_vars() que crea exposure_ladder() (niveles
##   ladder_levels); exposure_ladder es la función, no una columna. ref = row (Without a non-trivial problem); orden
##   de filas publicado (standard_figures.R:195; no el de ladder_levels); text2 = delta de valores redondeados con
##   signo "+", "−" (U+2212) o "±" y sufijo " points" ("" en la tarjeta de referencia); lo/hi = min/max de
##   round(value) de las figuras 41 y 42 (escala común, publicado 29/55); 'Without a non-trivial problem' (relabel local)
## Método UE: none
## Código de origen: report_charts.R:393 chart_scorecards + standard_figures.R:192-197
##   scorecard_def('exposure') (A04.R:3-21)

fig_41 <- list(

  # Confianza (1-2) en cada actor por exposición al sistema (exposicion de add_report_vars, A04.R:3-12), sobre todos los encuestados del país (trust_ladder, R/03_estimators.R:
  # xt con el promedio NUTS del reporte; n < 30 → N/A), en el orden publicado de las filas; la primera fila («No
  # non-trivial problem», impresa «Without a non-trivial problem») es la de referencia. Escala de color común a la 41 y
  # la 42 (trust_cards_scale: rango de los valores redondeados de las dos; lo publicado, 29/55).
  data = function(ctx) {
    vals <- trust_ladder(ctx, "exposicion")
    rownames(vals)[rownames(vals) == "No non-trivial problem"] <- "Without a non-trivial problem"
    lohi <- trust_cards_scale(ctx)
    log_msg("fig 41: escala ", lohi[1], "-", lohi[2], " · ", paste(sprintf("%s %s", rownames(vals),
            apply(vals, 1, \(r) paste(sprintf("%.2f", r), collapse = "/"))), collapse = " · "))
    rows_cards(vals, ref = "row", lohi = lohi)
  },

  # Dibujo: WJPr::wjp_cards() con sus argumentos (wjpr_cards, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_scorecards(df, spec) (sin portar: error legible, R/05_charts.R).
  plot = function(df, spec) wjpr_cards(df, spec)
)
