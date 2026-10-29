## Figura 32 · Hardships Experienced as a Result of Non-Trivial Justice Problems in {country}, by Outcome of the Problem
## Fuente de datos: xt(ctx, h, "resuelto", df = filter(ctx$national, has_nt)) para h = hardship, hardships_health,
##   hardships_income, hardships_emotional, hardships_drugs (A03.R:3-12; corte Resolución; nat_est); resuelto y has_nt
##   son columnas de add_report_vars(). ref = col (Fully resolved); text2 = delta de valores redondeados con signo
##   "+", "−" (U+2212) o "±" y sufijo " points" ("" y value2 NA en la tarjeta de referencia); lo/hi = min/max de
##   round(value) de esta figura (publicado 2/62); 'Alcohol or drug problems' (relabel local)
## Método UE: none
## Código de origen: report_charts.R:393 chart_scorecards (reescrito para df$text/text2/lo/hi) +
##   standard_figures.R:186-191 scorecard_def('hardship_outcome') (diseño charts5.py:150)

# Dificultades (fila impresa = variable) y desenlaces (columnas). origen: A03.R:5-6 (harms) y standard_figures.R:186-191
# (scorecard_def('hardship_outcome'): rótulos publicados, «Alcohol or drug problems» en vez de «Alcohol/Drug problems»)
fig32_harms <- c("At least one hardship" = "hardship", "Health issues" = "hardships_health",
                 "Economic issues" = "hardships_income", "Relationship breakdown" = "hardships_emotional",
                 "Alcohol or drug problems" = "hardships_drugs")
fig32_cols <- c("Fully resolved", "Not resolved")

fig_32 <- list(

  # Entre quienes tienen un problema no trivial (has_nt), % con cada dificultad por desenlace (resuelto, de
  # add_report_vars: resolved = rp_outcome), con el promedio NUTS del reporte (xt, A03.R:7-12; n < 30 → N/A). Tarjeta de
  # referencia = la primera columna (Fully resolved); escala de color = rango de los valores redondeados (rows_cards).
  data = function(ctx) {
    d <- ctx$national %>% filter(has_nt)
    vals <- t(vapply(fig32_harms, function(v) {
      x <- xt(ctx, v, "resuelto", df = d); x$pct[match(fig32_cols, x$grupo)]
    }, numeric(length(fig32_cols))))
    dimnames(vals) <- list(names(fig32_harms), fig32_cols)
    log_msg("fig 32: ", paste(sprintf("%s %.2f / %.2f", rownames(vals), vals[, 1], vals[, 2]), collapse = " · "))
    rows_cards(vals, ref = "col")
  },

  # Dibujo: WJPr::wjp_cards() con sus argumentos (wjpr_cards, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_scorecards(df, spec) (sin portar: error legible, R/05_charts.R).
  plot = function(df, spec) wjpr_cards(df, spec)
)
