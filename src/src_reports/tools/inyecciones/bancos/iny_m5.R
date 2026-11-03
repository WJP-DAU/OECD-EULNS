# Inyecciones de M5 en el sankey (fig 11; sk_*, portadas de bancos/iny_g3.R a la estructura de capas de wjp_sankey) y en
# el UpSet (fig 29; up_*, nuevas): se añade al final de R/09_wjpr.R de la copia M5 y se quita después (iny_run_m5.sh).
# Cambian solo el dibujo (df intacto): la capa se rehace con ggplot2::layer() con otra data (como en iny_g3.R). Las capas
# se buscan por estructura (wjpr_sk_layers, wjpr_up_layers), no por número. INY = nombre de la inyección.

.iny <- Sys.getenv("INY")
.iny_data <- function(p, k, f) {
  l <- p$layers[[k]]
  pr <- c(l$geom_params, l$stat_params, l$aes_params); pr <- pr[!duplicated(names(pr))]
  p$layers[[k]] <- ggplot2::layer(geom = l$geom, stat = l$stat, position = l$position, data = f(l$data),
                                  mapping = ggplot2::aes(!!!as.list(l$mapping)), params = pr,
                                  inherit.aes = l$inherit.aes, show.legend = FALSE)
  p
}
.iny_param <- function(p, k, ...) {
  l <- p$layers[[k]]
  pr <- c(l$geom_params, l$stat_params, l$aes_params); pr <- pr[!duplicated(names(pr))]; pr <- utils::modifyList(pr, list(...))
  p$layers[[k]] <- ggplot2::layer(geom = l$geom, stat = l$stat, position = l$position, data = l$data,
                                  mapping = ggplot2::aes(!!!as.list(l$mapping)), params = pr,
                                  inherit.aes = l$inherit.aes, show.legend = FALSE)
  p
}
.iny_swap <- function(d, col, i, j) { v <- d[[col]]; v[c(i, j)] <- v[c(j, i)]; d[[col]] <- v; d }

.sk_orig <- wjpr_sankey
wjpr_sankey <- function(df, spec, ...) {
  p <- .sk_orig(df, spec, ...); a <- attr(p, "wjpr"); ly <- wjpr_sk_layers(p$layers)
  # barras (x, y0, y1, fill; filas = columna 1, 2, 3 de arriba abajo), etiquetas (una fila por línea), valores (una por
  # caja, en el orden de las barras), encabezados (una fila por línea: x, y, label)
  if (.iny == "sk_valores") p <- .iny_data(p, ly$val, function(d) .iny_swap(d, "label", 3, 5))          # appropriate 21% ↔ needed_no 38%
  if (.iny == "sk_etiquetas") p <- .iny_data(p, ly$lab, function(d) .iny_swap(d, "label", which(d$label == "Appropriate adviser:"),
                                                                                which(d$label == "Non-appropriate adviser:")))
  if (.iny == "sk_alto") p <- .iny_data(p, ly$bars, function(d) { d$y1[3] <- d$y0[3] + (d$y1[5] - d$y0[5]); d })   # appropriate con el alto de needed_no
  # nuevas (1.2.0: los encabezados son una capa de texto y no el eje x)
  if (.iny == "sk_encabezados") p <- .iny_data(p, ly$head, function(d) {           # encabezados de las columnas 1 y 2 intercambiados
    x <- sort(unique(d$x)); i <- d$x == x[1]; j <- d$x == x[2]; d$x[i] <- x[2]; d$x[j] <- x[1]; d
  })
  if (.iny == "sk_oculto") p <- .iny_data(p, ly$val, function(d) { d$colour[4] <- "#F2F3F3"; d })   # 8 % (non_appropriate) del color de la caja
  attr(p, "wjpr") <- a
  p
}

.up_orig <- wjpr_upset
wjpr_upset <- function(df, spec, ...) {
  p <- .up_orig(df, spec, ...); a <- attr(p, "wjpr"); ly <- wjpr_up_layers(p$layers)
  # barras de intersección (xmin…ymax en pt, y hacia abajo; filas de izquierda a derecha), puntos (x, y, on), barras de
  # conjunto (de arriba abajo), nombres (label), valores (text, una fila por barra), tamaños (text)
  if (.iny == "up_valores") p <- .iny_data(p, ly$val, function(d) .iny_swap(d, "text", 1, 3))       # 16.9% ↔ 12.5%
  if (.iny == "up_nombres") p <- .iny_data(p, ly$names, function(d) .iny_swap(d, "label", 1, 2))    # Problem Resolved ↔ Resolution Process
  if (.iny == "up_alto") {                                                                          # 2.ª intersección +5 pp
    vmax <- max(df$value[df$kind == "row"]); d0 <- p$layers[[ly$bars]]$data; bh <- max(d0$ymax) - 2 * 5.48 / 1.024
    p <- .iny_data(p, ly$bars, function(d) { d$ymin[2] <- d$ymin[2] - 5 * bh / (vmax * 1.25); d })
  }
  if (.iny == "up_conjunto") p <- .iny_data(p, ly$sets, function(d) { d$xmin[1] <- d$xmax[1] - 1.2 * (d$xmax[1] - d$xmin[1]); d })  # tamaño 1 ×1.2
  if (.iny == "up_punto") p <- .iny_data(p, ly$dots, function(d) {                                  # columna 3: los puntos rotan una fila (el activo de abajo pasa arriba)
    x3 <- sort(unique(d$x))[3]; k <- which(d$x == x3); on <- d$on[k]
    d$on[k] <- on[c(length(on), seq_len(length(on) - 1))]
    d
  })
  if (.iny == "up_oculto") p <- .iny_param(p, ly$val, alpha = 0)                                   # valores de las intersecciones invisibles
  attr(p, "wjpr") <- a
  p
}
