## Figura 15 · Access to Affordable Assistance and Representation in {country}: Perceptions and Experiences
## Fuente de datos: perception_experience(ctx, "JSE_access2assis", "rep_c_published") (F06.R:3-15, 21): percepción =
##   población general (JSE_access2assis con binary_agree); experiencia = con problema no trivial (rep_c_published,
##   journey_vars); 18-24 sin valor en ambas series → fila vacía (sin puntos ni texto)
## Método UE: none
## Código de origen: report_charts.R:182-217 chart_dumbbell ← viz_helpers.R:92 report_dumbbell (F06.R:27)

fig_15 <- list(

  # Estimación. perception_experience() devuelve la fila nacional y los cortes Gender, Age Group, Financial
  # Situation y Area of Residence (promedio nacional ponderado por NUTS; n < 30 → NA). La experiencia es la variante
  # publicada del acceso a asistencia (rep_c_published: sin "no lo necesitó" ni "razón desconocida", R03.R:19), que
  # perception_experience() añade con with_journey(). rows_dumbbell() fija el orden, kind y text/text2 con fmt_num()
  # ("" si el valor falta: en el país del reporte, 18-24 sin ninguno de los dos → solo la etiqueta).
  data = function(ctx) {
    tbl <- perception_experience(ctx, "JSE_access2assis", "rep_c_published")
    nat <- tbl[tbl$category == "National Average", ]
    log_msg("fig 15: National percepción = ", formatC(nat$perception, format = "f", digits = 2), " (n ", nat$n_p,
            ") · experiencia = ", formatC(nat$experience, format = "f", digits = 2), " (n ", nat$n_e, ")")
    rows_dumbbell(tbl, decimals = 0)
  },

  # Dibujo: WJPr::wjp_dumbbells() con sus argumentos (wjpr_dumbbell, R/09_wjpr.R; WJPr 1.2.0), con la leyenda fija de
  # dos líneas que trae por defecto (la misma de la 10). Respaldo sin WJPr: chart_dumbbell(df, spec, legend = <la
  # misma>) con metodo svg_r.
  plot = function(df, spec) wjpr_dumbbell(df, spec)
)
