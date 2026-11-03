## Figura 42 · Public Trust in Justice Actors in {country}, by Experience of Hardship
## Fuente de datos: xt(ctx, a, "escalera_hardship") para a = trust_TRT_judges, trust_TRT_prosecutors, trust_TRT_pda
##   (A05.R:11-13, 26-28; corte Escalera con hardship; nat_est); escalera_hardship es la columna de add_report_vars().
##   ref = row (Without a non-trivial problem); text2 como en la 41; lo/hi = min/max de round(value) de las figuras
##   41 y 42 (escala común, publicado 29/55: la tarjeta del 30 % tiene t = 0.1535, medido en el PNG publicado; la
##   referencia anterior traía 30/55); 'Without a non-trivial problem' (relabel local)
## Método UE: none
## Código de origen: report_charts.R:393 chart_scorecards + standard_figures.R:198-202
##   scorecard_def('hardship') (A05.R)

fig_42 <- list(

  # Spec: tipo y motor de dibujo, tamaño final (in), recuadro, columna de etiquetas y media UE (figures/_template.R)
  spec = list(tipo = "cards", metodo = "wjpr", w_in = 6.5, h_in = 2.756, callout = FALSE, label_col = "none",
              metodo_ue = "none",
              nota = paste("idem 41 (9 tarjetas; lo/hi 29/55, la escala común con la 41: la tarjeta del 30 % tiene t",
                           "= 0.1535 en la publicada; la referencia traía 30/55 hasta el 29-oct); referencia de",
                           "A05.csv confirmada con finales/datos")),

  # Confianza (1-2) en cada actor por experiencia de dificultades (escalera_hardship de add_report_vars, A05.R:11-13), sobre todos los encuestados del país (trust_ladder, R/03_estimators.R:
  # xt con el promedio NUTS del reporte; n < 30 → N/A), en el orden publicado de las filas; la primera fila («No
  # non-trivial problem», impresa «Without a non-trivial problem») es la de referencia. Escala de color común a la 41 y
  # la 42 (trust_cards_scale: rango de los valores redondeados de las dos; lo publicado, 29/55).
  data = function(ctx) {
    vals <- trust_ladder(ctx, "escalera_hardship")
    rownames(vals)[rownames(vals) == "No non-trivial problem"] <- "Without a non-trivial problem"
    lohi <- trust_cards_scale(ctx)
    log_msg("fig 42: escala ", lohi[1], "-", lohi[2], " · ", paste(sprintf("%s %s", rownames(vals),
            apply(vals, 1, \(r) paste(sprintf("%.2f", r), collapse = "/"))), collapse = " · "))
    rows_cards(vals, ref = "row", lohi = lohi)
  },

  # Dibujo: WJPr::wjp_cards() con sus argumentos (wjpr_cards, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_scorecards(df, spec) (sin portar: error legible, R/05_charts.R).
  plot = function(df, spec) wjpr_cards(df, spec)
)
