## Figura 11 · Pathways to Accessing Appropriate Assistance and Representation in {country}
## Fuente de datos: journey(ctx) (R03.R:4-38): adv_contacted y adv_s_appropriate/nonappr/needed_no/noneed/unknown a 1
##   decimal; not_contacted = 100 − contacted; access = round(rep_c_published); no_access = 100 − access; excluded =
##   round(no_need_r1 + unknown_r1) = 33 (rows_sankey: método publicado, standard_figures.R:129-148)
## Método UE: none
## Código de origen: report_charts.R:311-382 chart_sankey (reescrito para df$text) + standard_figures.R:128-151
##   sankey_def('assistance') (diseño charts5.py:79 sankey)

fig_11 <- list(

  # Promedios nacionales ponderados por NUTS de las variables del recorrido (journey, memorizado por ctx). rows_sankey()
  # aplica el redondeo publicado y devuelve los 10 nodos (columna 1: contactó o no; 2: tipo de asesor o motivo; 3:
  # acceso entre quienes lo necesitaban y excluidos) con text = fmt_num(value, 0).
  data = function(ctx) rows_sankey(journey(ctx), "assistance"),

  # Dibujo: WJP_theme()/wjp_palette() + complementos (wjpr_sankey, R/09_wjpr.R; metodo wjpr: WJPr no tiene sankey),
  # con los rótulos y encabezados fijos de sankey_def("assistance"), que el adaptador deduce del df (sin el nodo
  # "turned" de la 16); ancho del manifiesto (8.4 in). Respaldo sin WJPr: chart_sankey(df, spec, "assistance") con
  # metodo svg_r.
  plot = function(df, spec) wjpr_sankey(df, spec)
)
