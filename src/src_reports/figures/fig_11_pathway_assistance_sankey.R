## Figura 11 · Pathways to Accessing Appropriate Assistance and Representation in {country}
## Fuente de datos: journey(ctx) (R03.R:4-38): adv_contacted y
##   adv_s_appropriate/nonappr/needed_no/noneed/unknown a 1 decimal; not_contacted = 100 − c1; access =
##   round(rep_c_published); no_access = 100 − round; excluded = round(no_need_r1 + unknown_r1) = 33
## Método UE: none
## Código de origen: report_charts.R:315 chart_sankey (reescrito para df$text) + standard_figures.R:128-150
##   sankey_def('assistance') (diseño charts5.py:79 sankey)
## Estado: pendiente (esqueleto de la Etapa 1; contrato en figures/_template.R)

fig_11 <- list(
  data = function(ctx) stop("pendiente: fig_11 sin implementar", call. = FALSE),
  plot = function(df, spec) stop("pendiente: fig_11 sin implementar", call. = FALSE)
)
