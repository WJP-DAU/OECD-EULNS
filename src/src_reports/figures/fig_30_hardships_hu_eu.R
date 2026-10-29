## Figura 30 · Hardships Experienced as a Result of Non-Trivial Justice Problems in {country}
## Fuente de datos: get_results_table National de hardships_health/income/emotional/drugs + eu_est(ctx, …)
##   (F07.R); orden fijo; series {country} | EU Average; etiqueta 'Alcohol/Drug problems'
## Método UE: eu_est
## Código de origen: report_charts.R:247 chart_columns ← viz_helpers.R:126 paired_bars (F07.R:22)

# Las cuatro dificultades en el orden impreso (= variable). origen: F07.R:5-6 (hardship_vars)
fig30_hardships <- c("Health issues" = "hardships_health", "Economic issues" = "hardships_income",
                     "Relationship breakdown" = "hardships_emotional", "Alcohol/Drug problems" = "hardships_drugs")

fig_30 <- list(

  # País = promedio nacional ponderado por NUTS de cada dificultad (nat_est; n < 30 → N/A), UE = media simple de las
  # estimaciones nacionales (eu_est), como F07.R:19-23. Orden fijo del diseño (rows_columns, R/05_charts.R).
  data = function(ctx) {
    hu <- vapply(fig30_hardships, \(v) nat_est(ctx, v)$value[[1]], numeric(1))
    eu <- vapply(fig30_hardships, \(v) eu_est(ctx, v), numeric(1))
    log_msg("fig 30: ", paste(sprintf("%s %.2f / UE %.2f", names(hu), hu, eu), collapse = " · "))
    rows_columns(names(fig30_hardships), hu, eu, decimals = 0)
  },

  # Dibujo: WJPr::wjp_bars() vertical con sus argumentos (wjpr_columns, R/09_wjpr.R; WJPr 1.2.0): país y UE lado a lado,
  # alturas relativas a la columna más alta. Respaldo sin WJPr: chart_columns(df, spec) (sin portar: error legible, R/05_charts.R).
  plot = function(df, spec) wjpr_columns(df, spec)
)
