## Figura 32 · Hardships Experienced as a Result of Non-Trivial Justice Problems in {country}, by Outcome of the Problem
## Fuente de datos: xt(ctx, h, "resuelto", df = filter(ctx$national, has_nt)) para h = hardship, hardships_health,
##   hardships_income, hardships_emotional, hardships_drugs (A03.R:3-12; corte Resolución; nat_est); resuelto y has_nt
##   son columnas de add_report_vars(). ref = col (Fully resolved); text2 = delta de valores redondeados con signo
##   "+", "−" (U+2212) o "±" y sufijo " points" ("" y value2 NA en la tarjeta de referencia); lo/hi = min/max de
##   round(value) de esta figura (publicado 2/62); 'Alcohol or drug problems' (relabel local)
## Método UE: none
## Código de origen: report_charts.R:393 chart_scorecards (reescrito para df$text/text2/lo/hi) +
##   standard_figures.R:186-191 scorecard_def('hardship_outcome') (diseño charts5.py:150)
## Estado: pendiente (esqueleto de la Etapa 1; contrato en figures/_template.R)

fig_32 <- list(
  data = function(ctx) stop("pendiente: fig_32 sin implementar", call. = FALSE),
  plot = function(df, spec) stop("pendiente: fig_32 sin implementar", call. = FALSE)
)
