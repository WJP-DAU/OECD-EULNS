## Figura 42 · Public Trust in Justice Actors in {country}, by Experience of Hardship
## Fuente de datos: xt(ctx, a, "escalera_hardship") para a = trust_TRT_judges, trust_TRT_prosecutors, trust_TRT_pda
##   (A05.R:11-13, 26-28; corte Escalera con hardship; nat_est); escalera_hardship es la columna de add_report_vars().
##   ref = row (Without a non-trivial problem); text2 como en la 41; lo/hi = min/max de round(value) de esta figura
##   (publicado 30/55; no común con la 41); 'Without a non-trivial problem' (relabel local)
## Método UE: none
## Código de origen: report_charts.R:393 chart_scorecards + standard_figures.R:198-202
##   scorecard_def('hardship') (A05.R)
## Estado: pendiente (esqueleto de la Etapa 1; contrato en figures/_template.R)

fig_42 <- list(
  data = function(ctx) stop("pendiente: fig_42 sin implementar", call. = FALSE),
  plot = function(df, spec) stop("pendiente: fig_42 sin implementar", call. = FALSE)
)
