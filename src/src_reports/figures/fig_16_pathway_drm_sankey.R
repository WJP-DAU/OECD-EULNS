## Figura 16 · Pathways to Accessing DRMs in {country}
## Fuente de datos: journey(ctx) (R03.R:4-38): drm_s_turned/needed_no/noneed/unknown a 1 decimal; not_contacted = 100 −
##   contacted; access = round(drm_c_published) = 28; no_access = 100 − access; excluded = round(drm_no_need_r1 +
##   drm_unknown_r1) = round(47.1 + 8.4) = 56 (método publicado; sin redondear las partes daría 55)
## Método UE: none
## Código de origen: report_charts.R:311-382 chart_sankey + standard_figures.R:152-170 sankey_def('drm') +
##   PUBLISHED_SANKEY$drm (174-177); datos R03.R:4-38 journey_vars

fig_16 <- list(

  # Promedios nacionales ponderados por NUTS de las variables del recorrido (journey, memorizado por ctx).
  # rows_sankey(…, "drm") aplica el redondeo publicado y devuelve los 9 nodos (columna 1: contactó un DRM o no; 2:
  # acudió o motivo; 3: acceso entre quienes lo necesitaban y excluidos) con text = fmt_num(value, 0). En la columna 1
  # "contacted" = drm_s_turned (no hay nodo de DRM no apropiado). excluded suma las partes a 1 decimal (decisión del
  # usuario del 22-oct: el 56 % publicado; en el país del reporte las partes sin redondear suman 47.08 + 8.39 = 55.47
  # → 55). El log da las dos sumas.
  data = function(ctx) {
    j <- journey(ctx)
    f2 <- function(x) formatC(x, format = "f", digits = 2)
    log_msg("fig 16: excluded = round(", j$drm_no_need_r1, " + ", j$drm_unknown_r1, ") = ",
            fmt_num(j$drm_no_need_r1 + j$drm_unknown_r1, 0), " (partes sin redondear: ", f2(j$drm_s_noneed), " + ",
            f2(j$drm_s_unknown), " → ", fmt_num(j$drm_s_noneed + j$drm_s_unknown, 0), ") · access = round(",
            f2(j$drm_c_published), ") · contacted = ", f2(j$drm_s_turned))
    rows_sankey(j, "drm")
  },

  # Dibujo: WJPr::wjp_sankey() con sus argumentos (wjpr_sankey, R/09_wjpr.R; WJPr 1.2.0),
  # con los rótulos y encabezados fijos de sankey_def("drm"), que el adaptador deduce del nodo "turned" de la columna
  # 2; ancho del manifiesto (8.4 in). Respaldo sin WJPr: chart_sankey(df, spec, "drm") con metodo svg_r.
  plot = function(df, spec) wjpr_sankey(df, spec)
)
