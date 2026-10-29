## Figura 07 · Severity of Justice Problems in {country}
## Fuente de datos: severity_distribution (F03.R:3-18): % de los problemas reportados (sev 0-10) en cada valor de
##   severidad, por país (nuts_mean_long: por región NUTS, n < 30 → NA, ponderado por población); UE = media simple
##   de las distribuciones nacionales de ctx$meta$countries con dato (F03.R:18; el país incluido)
## Método UE: eu_est_distribution
## Código de origen: F03.R:3-18 (severity_distribution, run_F03) y F03.R:32-48 sev_plot (geom_col dodge 0.8 width 0.75
##   + vline 3.5) → wjpr_severity_bars (R/09_wjpr.R)

# problems_long(): R/03_estimators.R (origen F02.R:3-12, literal; la comparte la fig 3)

# origen: F03.R:3-9 — adaptada: nuts_mean_long() con ctx (regions de ctx$regions). Para cada valor v = 0..10,
# proporción de los problemas con sev == v por país (× groups): media por región NUTS (regiones con menos de 30
# problemas → NA) y promedio ponderado por regionpoppct; pct = 100 · est. La supresión depende solo del número de
# problemas de la región: la distribución de cada país suma 100. La columna one no se usa (literal del original).
severity_distribution <- function(ctx, long, groups = NULL) {
  long <- long %>% mutate(one = 1)
  map_dfr(0:10, function(v) {
    nuts_mean_long(ctx, long %>% mutate(at_v = as.numeric(sev == v)), "at_v", groups) %>% mutate(sev = v)
  }) %>% mutate(pct = 100 * est)
}

fig_07 <- list(

  # Dos barras por valor de severidad (11 × 2): el país y la media UE. Como run_F03, la serie del país sale de la
  # misma tabla de países que la UE (ctx$master; = ctx$national). value = % de los problemas (sin redondear; NA si
  # todas las regiones del país quedan suprimidas); nada impreso. Orden: severidad 0-10 y, dentro, país antes que la
  # UE (dodge).
  data = function(ctx) {
    countries <- ctx$meta$countries %||% unique(ctx$master$country_name_ltn)
    es <- problems_long(ctx$master %>% filter(country_name_ltn %in% countries))                   # origen: F03.R:14
    dist_country <- severity_distribution(ctx, es)                                                 # origen: F03.R:16
    hu_dist <- dist_country %>% filter(country_name_ltn == ctx$country) %>%
      transmute(sev, pct, serie = ctx$country)                                                      # origen: F03.R:17
    if (nrow(hu_dist) == 0) stop("fig 07: ", ctx$country, " sin problemas con severidad 0-10", call. = FALSE)
    eu_dist <- dist_country %>% group_by(sev) %>% summarise(pct = mean(pct, na.rm = TRUE), .groups = "drop") %>%
      mutate(serie = "EU Average")                                                                  # origen: F03.R:18
    log_msg("fig 07: media UE = media de ", n_distinct(dist_country$country_name_ltn[!is.na(dist_country$pct)]),
            " distribuciones nacionales (de ", length(countries), " países)")
    bind_rows(hu_dist, eu_dist) %>%                                                                 # origen: F03.R:19
      arrange(sev, match(serie, c(ctx$country, "EU Average"))) %>%
      transmute(order = row_number(), kind = "bar", grouping = serie, label = as.character(sev), value = pct,
                value2 = NA_real_, text = "", text2 = "")
  },

  # Dibujo: WJPr::wjp_bars() vertical de WJPr 1.2.0 con sus argumentos, sin complementos (wjpr_severity_bars,
  # R/09_wjpr.R; metodo wjpr; revisión 5): series lado a lado (dodge 0.8, ancho 0.75), leyenda arriba ({country} | EU
  # Average), línea de corte discontinua en 3.5 (umbral de no trivial, ref_lines) y el tema de F03 en pt finales.
  plot = function(df, spec) wjpr_severity_bars(df, spec)
)
