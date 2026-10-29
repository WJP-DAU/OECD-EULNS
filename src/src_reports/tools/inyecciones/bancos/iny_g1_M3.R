# Inyecciones db_* de bancos/iny_g1.R (figura 10, dumbbell) portadas a la estructura de capas de wjp_dumbbells 1.2.0
# con layout: 1 blank (líneas: group, pos, start, end), 2 segment, 3-4 point (y = start | end), 5 text de valores
# (datos con series, label, pos, y_out), 6 text de rótulos de línea (datos con kind, label, pos). Mismo error semántico
# que en el banco original; cambian solo el dibujo (df intacto). Se añade al final de R/09_wjpr.R (iny_run_M3.sh).
# INY = db_textos | db_etiquetas | db_punto (las del banco) | db_oculto | db_leyenda (extra).

.iny <- Sys.getenv("INY")
# Rehace (sin p$layers[[k]] <- …, que desborda la pila) las capas que cumplen pick(l) con data f(data)
.edit_layers_data <- function(p, pick, f) {
  p$layers <- lapply(p$layers, function(l) {
    if (!is.data.frame(l$data) || !isTRUE(pick(l))) return(l)
    l2 <- ggplot2::ggproto(NULL, l); l2$data <- f(l$data); l2
  })
  p
}
.is_values <- function(l) inherits(l$geom, "GeomText") && "series" %in% names(l$data)
.is_labels <- function(l) inherits(l$geom, "GeomText") && "kind" %in% names(l$data)
.is_point_y <- function(ycol) function(l) inherits(l$geom, "GeomPoint") && identical(rlang::as_label(l$mapping$y), ycol)

.db_orig <- wjpr_dumbbell
wjpr_dumbbell <- function(df, spec, ...) {
  p <- .db_orig(df, spec, ...); a <- attr(p, "wjpr")
  blank <- p$layers[[1]]$data                                   # una fila por categoría: group (rótulo) y pos
  pos_of <- function(lab) blank$pos[match(lab, blank$group)]
  s1 <- unique(blank$start_group)[1]
  if (.iny == "db_textos") {            # texto de percepción de Male (29%) ↔ el de Female (25%)
    p <- .edit_layers_data(p, .is_values, function(d) {
      i <- which(d$pos == pos_of("Male") & d$series == s1); j <- which(d$pos == pos_of("Female") & d$series == s1)
      d$label[c(i, j)] <- d$label[c(j, i)]; d
    })
  }
  if (.iny == "db_etiquetas") {         # rótulos impresos Urban ↔ Rural
    p <- .edit_layers_data(p, .is_labels, function(d) {
      i <- which(d$label == "Urban"); j <- which(d$label == "Rural"); d$label[c(i, j)] <- d$label[c(j, i)]; d
    })
  }
  if (.iny == "db_punto") {             # punto de percepción de Male +5 pp (texto y segmento en su sitio)
    p <- .edit_layers_data(p, .is_point_y("start"), function(d) {
      k <- d$group == "Male"; d$start[k] <- d$start[k] + 5; d
    })
  }
  if (.iny == "db_oculto") {            # extra: valor de experiencia de Female oculto (blanco sobre blanco)
    p <- .edit_layers_data(p, .is_values, function(d) {
      k <- d$pos == pos_of("Female") & d$series != s1; d$colour[k] <- "#FFFFFF"; d
    })
  }
  if (.iny == "db_leyenda") {           # extra: textos de la leyenda intercambiados (colores en su sitio)
    sc <- p$scales$get_scales("colour"); lb <- sc$labels
    sc$labels <- if (is.character(lb)) rev(lb) else rev(sc$breaks)
  }
  attr(p, "wjpr") <- a
  p
}
