## Figura 03 · Prevalence and Severity of Non-Trivial Justice Problems in {country}, by Area of Residence
## Fuente de datos: category_profile("residence") (F02.R:14-25): prevalencia = nat_est sobre problem_flags por área
##   (Rural / Urban); severidad media = nuts_mean_long sobre problems_long (todos los problemas, sev 0-10) por
##   categoría × área; n = encuestados con problema no trivial en la categoría (punto hueco si n < 30); destacadas
##   (rotuladas; series "destacada") = figures: fig03: highlight de config/<pais>.yml (lo publicado: top_cats de
##   F02.R:33); sin la clave, las 6 de mayor prevalencia no trivial nacional, con aviso en el log
## Método UE: none
## Código de origen: F02.R:3-25 (problems_long, category_profile) y F02.R:33-51 run_F02 (geom_point + lm discontinua +
##   ggrepel seed 7, rótulos top_cats) → wjpr_scatter (R/09_wjpr.R)

# problems_long(): R/03_estimators.R (origen F02.R:3-12, literal; la comparte la fig 7)

# origen: F02.R:14-25 — adaptada a ctx: hu → ctx$national; nat_est() y nuts_mean_long() con ctx (regions de
# ctx$regions); pct → value. Una fila por grupo × categoría: prevalencia (NUTS, n < 30 → NA), n_grupo,
# n_con_problema_nt, severidad_media (NA si todas las regiones del grupo × categoría tienen menos de 30 problemas) y
# n_problemas.
category_profile <- function(ctx, group_var) {
  hu <- ctx$national
  nt <- problem_flags(hu)
  flags <- bind_cols(hu %>% select(country_name_ltn, nuts_id, all_of(group_var)), nt %>% mutate(across(everything(), as.numeric)))
  prev <- map_dfr(categories, \(k) nat_est(ctx, k, by = group_var, df = flags) %>%
                    transmute(grupo = as.character(.data[[group_var]]), category = k, prevalencia = value, n_grupo = n))
  n_nt <- map_dfr(categories, \(k) flags %>% filter(.data[[k]] == 1, !is.na(.data[[group_var]])) %>%
                    count(grupo = as.character(.data[[group_var]]), name = "n_con_problema_nt") %>% mutate(category = k))
  sev <- nuts_mean_long(ctx, problems_long(hu, group_var) %>% filter(!is.na(.data[[group_var]])), "sev", c("category", group_var)) %>%
    transmute(grupo = .data[[group_var]], category, severidad_media = est, n_problemas = n)
  prev %>% left_join(n_nt, by = c("grupo", "category")) %>% left_join(sev, by = c("grupo", "category")) %>%
    mutate(n_con_problema_nt = coalesce(n_con_problema_nt, 0L), n_insuficiente = n_con_problema_nt < 30 | is.na(severidad_media))
}

# Categorías destacadas (rotuladas) de la figura: una elección editorial del reporte, no una regla de los datos (en
# lo publicado, top_cats de F02.R:33 = los puestos 1-4, 8 y 9 de prevalencia). Se leen de config/<pais>.yml (figures:
# fig03: highlight); sin la clave, las 6 de mayor prevalencia no trivial nacional (nat_est sobre cooc_flags(ctx), el
# orden de la fig 6), con aviso en el log; un nombre que no es categoría del reporte se avisa y se ignora.
# corregido: antes la lista publicada era el valor por defecto de wjpr_scatter y otro país rotulaba las mismas 6
# en silencio (revisión V2-03, V4-04)
fig03_highlight <- function(ctx) {
  h <- ctx$cfg$figures$fig03$highlight
  if (is.null(h)) {
    prev <- map_dbl(categories, \(k) nat_est(ctx, k, df = cooc_flags(ctx))$value) %>% setNames(categories)
    h <- head(names(sort(prev, decreasing = TRUE)), 6)
    log_msg("fig 03 aviso: config/", ctx$slug, ".yml sin figures: fig03: highlight; destacadas = las 6 de mayor ",
            "prevalencia no trivial nacional: ", paste(h, collapse = ", "))
    return(h)
  }
  h <- as.character(unlist(h))
  otras <- setdiff(h, categories)
  if (length(otras) > 0) {
    log_msg("fig 03 aviso: figures: fig03: highlight de config/", ctx$slug, ".yml trae nombres que no son categorías ",
            "(se ignoran): ", paste(otras, collapse = ", "))
  }
  intersect(h, categories)
}

fig_03 <- list(

  # Spec: tipo y motor de dibujo, tamaño final (in), recuadro, columna de etiquetas y media UE (figures/_template.R)
  spec = list(tipo = "scatter", metodo = "wjpr", w_in = 7.382, h_in = 3.445, callout = FALSE, label_col = "none",
              base_pt = 12, metodo_ue = "none",
              nota = "sin valores impresos: verify compara 24 puntos (x tol 0.6; y tol 0.06); compare/ obligatorio"),

  # Un punto por área × categoría (2 × 12): value = prevalencia no trivial (% de los encuestados del área),
  # value2 = severidad media 0-10 (NA = suprimida: el punto no se dibuja), n = encuestados con problema no trivial en
  # la categoría, series = "destacada" (rotulada; fig03_highlight) o "". Nada impreso (text = ""). Orden: facetas
  # Rural y Urban; dentro, categorías en orden alfabético (categories).
  # corregido (revisión 4): text2 = WJPR_SCATTER_HUECO ("n < 30") en los puntos dibujables (prevalencia y severidad)
  # con n < 30, los que el adaptador dibuja huecos (revisión 5: es la marca que lee, hollow de wjp_scatter, =
  # n_insuficiente de F02.R:24 en los puntos que se dibujan); no se imprime (compara = "text"): lo compara lo dibujado
  # por punto (wjpr_scatter_drawn), que antes no veía un punto hueco con n ≥ 30
  data = function(ctx) {
    dest <- fig03_highlight(ctx)
    category_profile(ctx, "residence") %>%
      arrange(grupo, match(category, categories), .locale = "C") %>%
      transmute(order = row_number(), kind = "point", grouping = grupo, label = category, value = prevalencia,
                value2 = severidad_media, text = "",
                text2 = if_else(is.finite(value) & is.finite(value2) & n_con_problema_nt < 30, WJPR_SCATTER_HUECO, ""),
                n = n_con_problema_nt, series = if_else(category %in% dest, "destacada", ""))
  },

  # Dibujo: WJPr::wjp_scatter() con sus argumentos (wjpr_scatter, R/09_wjpr.R; revisión 5, WJPr 1.2.0): tendencia
  # lineal discontinua por faceta, destacadas (series) con su rótulo (seed 7), huecos (text2 «n < 30»), ejes 0-50 % y
  # 4-8; tamaños sobre base_pt 12 de la spec. Los puntos suprimidos no se dibujan (cuántos por faceta, al log) y
  # una faceta con menos de 2 puntos no se dibuja (aviso en el log; complemento).
  plot = function(df, spec) wjpr_scatter(df, spec)
)
