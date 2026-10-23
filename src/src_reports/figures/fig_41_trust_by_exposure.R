## Figura 41 · Public Trust in Justice Actors in {country}, by Exposure to the Justice System
## Fuente de datos: xt(ctx, a, "exposicion") para a = trust_TRT_judges, trust_TRT_prosecutors, trust_TRT_pda
##   (A04.R:16-21; nat_est); exposicion es la columna de add_report_vars() que crea exposure_ladder() (niveles
##   ladder_levels); exposure_ladder es la función, no una columna. ref = row (Without a non-trivial problem); orden
##   de filas publicado (standard_figures.R:195; no el de ladder_levels); text2 = delta de valores redondeados con
##   signo "+", "−" (U+2212) o "±" y sufijo " points" ("" en la tarjeta de referencia); lo/hi = min/max de
##   round(value) de esta figura (publicado 29/55; no común con la 42); 'Without a non-trivial problem' (relabel local)
## Método UE: none
## Código de origen: report_charts.R:393 chart_scorecards + standard_figures.R:192-197
##   scorecard_def('exposure') (A04.R:3-21)
## Estado: pendiente (esqueleto de la Etapa 1; contrato en figures/_template.R)

fig_41 <- list(
  data = function(ctx) stop("pendiente: fig_41 sin implementar", call. = FALSE),
  plot = function(df, spec) stop("pendiente: fig_41 sin implementar", call. = FALSE)
)
