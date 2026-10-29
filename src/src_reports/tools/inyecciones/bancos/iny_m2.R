# Inyecciones en el dibujo de las figuras de M2 con WJPr 1.2.0 (revisión 5): las de iny_g1.R (pb_* → 02, sev_* → 07)
# portadas a las capas de wjp_bars 1.2.0 y nuevas para hbars (hb_* → 13) y columns (col_* → 30). Se añade al final de
# R/09_wjpr.R de la copia M2 (iny_run_m2.sh, --stage all --strict; restaura con md5) o se carga en iny_m2_sim.R (13 y
# 30, sin data(): save_figure + verify por fila sobre la referencia). Cambian solo el dibujo (df intacto).
# INY = nombre de la inyección. Mismo error semántico que el banco G1: dos textos intercambiados, un valor oculto,
# una barra +5 pp, las series de una fila cambiadas, dos etiquetas intercambiadas, la leyenda intercambiada.
# Capas de wjp_bars 1.2.0: barras (geom_col sobre p$data; la pista, una función de p$data), valores (geom_text o, sin
# wjp_layout y con value_position "outside", geom_label; datos propios con labels_var y value_colour, mapeado con I())
# y, con wjp_layout en horizontal, la columna de etiquetas (geom_text con datos propios grouping_var + text).

.iny <- Sys.getenv("INY")
# Intercambia dos etiquetas impresas de una escala discreta (labels waiver → los niveles; vector con nombres → por
# nombre; función → su resultado); origen: iny_g1.R
.swap_scale_labels <- function(p, aes, a, b) {
  sc <- p$scales$get_scales(aes); f <- sc$labels
  if (is.character(f) && !is.null(names(f))) {
    i <- match(a, names(f)); j <- match(b, names(f)); f[c(i, j)] <- f[c(j, i)]; sc$labels <- f
  } else {
    sc$labels <- function(x) {
      y <- if (is.function(f)) f(x) else as.character(x)
      i <- which(x == a); j <- which(x == b)
      if (length(i) && length(j)) y[c(i, j)] <- y[c(j, i)]
      y
    }
  }
  p
}
# Rehace (sin p$layers[[k]] <- …) las capas que cumplen pred con data f(data)
.edit_layers <- function(p, pred, f) {
  p$layers <- lapply(p$layers, function(l) {
    if (!isTRUE(pred(l)) || !is.data.frame(l$data)) return(l)
    l2 <- ggplot2::ggproto(NULL, l); l2$data <- f(l$data); l2
  })
  p
}
.is_values <- function(l) inherits(l$geom, c("GeomText", "GeomLabel")) && is.data.frame(l$data) && "labels_var" %in% names(l$data)
.is_labcol <- function(l) inherits(l$geom, "GeomText") && is.data.frame(l$data) && all(c("grouping_var", "text") %in% names(l$data)) &&
  !"labels_var" %in% names(l$data)
.sw <- function(v, i, j) { v[c(i, j)] <- v[c(j, i)]; v }
.at <- function(d, cat, ser) which(as.character(d$grouping_var) == cat & as.character(d$colors_var) %in% ser)
# Las inyecciones comunes de barras: textos de dos categorías, valor oculto, barra +5 pp, series de una categoría,
# series con sus textos, etiquetas de la columna o del eje, leyenda
.iny_bars <- function(p, pre, cats, ser, eje_x = FALSE) {
  i1 <- cats[1]; i2 <- cats[2]; i3 <- cats[3]
  if (.iny == paste0(pre, "textos")) p <- .edit_layers(p, .is_values, function(d) {
    d$labels_var <- .sw(d$labels_var, .at(d, i1, ser[1]), .at(d, i2, ser[1])); d })
  if (.iny == paste0(pre, "oculto")) p <- .edit_layers(p, .is_values, function(d) {
    k <- .at(d, i2, ser[length(ser)]); d$value_colour[k] <- "transparent"; d })
  d <- p$data
  if (.iny == paste0(pre, "barra")) { k <- .at(d, i3, ser[1]); d$target_var[k] <- d$target_var[k] + 5 }
  if (.iny %in% paste0(pre, c("series", "series_todo")) && length(ser) > 1) {
    d$target_var <- .sw(d$target_var, .at(d, i1, ser[1]), .at(d, i1, ser[2]))
  }
  p$data <- d
  if (.iny == paste0(pre, "series_todo") && length(ser) > 1) p <- .edit_layers(p, .is_values, function(d) {
    d$labels_var <- .sw(d$labels_var, .at(d, i1, ser[1]), .at(d, i1, ser[2])); d })
  if (.iny == paste0(pre, "etiquetas")) {
    p <- if (eje_x) .swap_scale_labels(p, "x", i1, i2) else .edit_layers(p, .is_labcol, function(d) {
      d$text <- .sw(d$text, which(as.character(d$grouping_var) == i1), which(as.character(d$grouping_var) == i2)); d })
  }
  if (.iny == paste0(pre, "leyenda") && length(ser) > 1) p <- .swap_scale_labels(p, "fill", ser[1], ser[2])
  p
}

# 02 · paired_bars (pb_*: las de iny_g1.R con sus categorías; pb_oculto nueva) --------------------------------------
.pb_orig <- wjpr_paired_bars
wjpr_paired_bars <- function(df, spec, ...) {
  p <- .pb_orig(df, spec, ...); a <- attr(p, "wjpr"); ctry <- spec$country
  if (.iny == "pb_textos") p <- .edit_layers(p, .is_values, function(d) {   # texto del país: Public Services ↔ Housing
    d$labels_var <- .sw(d$labels_var, .at(d, "Public Services", ctry), .at(d, "Housing", ctry)); d })
  if (.iny == "pb_oculto") p <- .edit_layers(p, .is_values, function(d) {   # valor UE de Consumer transparente
    k <- .at(d, "Consumer", "EU Average"); d$value_colour[k] <- "transparent"; d })
  d <- p$data
  if (.iny == "pb_barra") { k <- .at(d, "Community", ctry); d$target_var[k] <- d$target_var[k] + 5 }   # país +5 pp
  if (.iny %in% c("pb_series", "pb_series_todo")) {                       # Community: barras de las dos series
    d$target_var <- .sw(d$target_var, .at(d, "Community", ctry), .at(d, "Community", "EU Average"))
  }
  p$data <- d
  if (.iny == "pb_series_todo") p <- .edit_layers(p, .is_values, function(d) {   # … y sus textos
    d$labels_var <- .sw(d$labels_var, .at(d, "Community", ctry), .at(d, "Community", "EU Average")); d })
  if (.iny == "pb_etiquetas") p <- .edit_layers(p, .is_labcol, function(d) {     # columna de etiquetas
    d$text <- .sw(d$text, which(as.character(d$grouping_var) == "Community"), which(as.character(d$grouping_var) == "Money & Debt")); d })
  if (.iny == "pb_leyenda") p <- .swap_scale_labels(p, "fill", ctry, "EU Average")
  attr(p, "wjpr") <- a
  p
}

# 07 · severity_bars (sev_*: las de iny_g1.R sin cambios; la línea de corte es ahora la geom_vline de ref_lines) -------
.sev_orig <- wjpr_severity_bars
wjpr_severity_bars <- function(df, spec, ...) {
  p <- .sev_orig(df, spec, ...); a <- attr(p, "wjpr")
  if (.iny == "sev_barra") {            # barra del país en severidad 5 +1 pp
    d <- p$data; k <- d$grouping_var == "5" & d$colors_var == spec$country; d$target_var[k] <- d$target_var[k] + 1; p$data <- d
  }
  if (.iny == "sev_leyenda") p <- .swap_scale_labels(p, "fill", spec$country, "EU Average")
  if (.iny == "sev_ejes") p <- .swap_scale_labels(p, "x", "3", "4")
  if (.iny == "sev_corte") p$layers <- Filter(function(l) !inherits(l$geom, "GeomVline"), p$layers)
  attr(p, "wjpr") <- a
  p
}

# 13 (y las demás hbars) · hb_*: textos de las filas 1 ↔ 2, valor de la fila 2 oculto, barra de la fila 3 +5 pp,
# etiquetas de las filas 1 ↔ 2 en la columna de etiquetas ---------------------------------------------------------------
.hb_orig <- wjpr_hbars
wjpr_hbars <- function(df, spec, ...) {
  p <- .hb_orig(df, spec, ...); a <- attr(p, "wjpr")
  if (startsWith(.iny, "hb_")) {
    cats <- df$label[order(df$order)][df$kind[order(df$order)] != "header"]
    p <- .iny_bars(p, "hb_", cats[1:3], "main")
  }
  attr(p, "wjpr") <- a
  p
}

# 30 (y 37) · col_*: textos del país de las categorías 1 ↔ 2, valor UE de la 2 oculto, barra del país de la 3 +5 pp,
# series de la 1 (barras; series_todo: con sus textos), etiquetas del eje 1 ↔ 2, leyenda --------------------------------
.col_orig <- wjpr_columns
wjpr_columns <- function(df, spec, ...) {
  p <- .col_orig(df, spec, ...); a <- attr(p, "wjpr")
  if (startsWith(.iny, "col_")) {
    cats <- df$label[order(df$order)]
    p <- .iny_bars(p, "col_", cats[1:3], c(spec$country, "EU Average"), eje_x = TRUE)
  }
  attr(p, "wjpr") <- a
  p
}
