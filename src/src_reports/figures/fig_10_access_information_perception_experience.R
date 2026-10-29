## Figura 10 · Access to Information and Advice in {country}: Perceptions and Experiences
## Fuente de datos: perception_experience(ctx, "JSE_access2info", "access2info") (F06.R:3-15, 20): percepción =
##   población general (JSE_access2info con binary_agree); experiencia = con problema no trivial (access2info)
## Método UE: none
## Código de origen: report_charts.R:182-217 chart_dumbbell ← viz_helpers.R:92 report_dumbbell (F06.R:26)

fig_10 <- list(

  # Estimación. perception_experience() devuelve la fila nacional y los cortes Gender, Age Group, Financial
  # Situation y Area of Residence (promedio nacional ponderado por NUTS; n < 30 → NA: 18-24 sin percepción).
  # rows_dumbbell() fija el orden (National Average y, dentro de cada corte, alfabético inverso), kind (avg |
  # header | row) y text/text2 con fmt_num() ("" si el valor falta: sin punto ni texto).
  data = function(ctx) {
    tbl <- perception_experience(ctx, "JSE_access2info", "access2info")
    rows_dumbbell(tbl, decimals = 0)
  },

  # Dibujo: WJPr::wjp_dumbbells() con sus argumentos (wjpr_dumbbell, R/09_wjpr.R; WJPr 1.2.0): percepción (color
  # principal) y experiencia (magenta) unidas por un segmento gris. La leyenda es texto fijo del diseño (la de
  # viz_helpers.R:106, impresa en la publicada) y es la que wjpr_dumbbell() trae por defecto («General population:
  # perception» / «People with a non-trivial problem: experience»). Respaldo sin WJPr: chart_dumbbell(df, spec,
  # legend = <la misma>) con metodo svg_r.
  plot = function(df, spec) wjpr_dumbbell(df, spec)
)
