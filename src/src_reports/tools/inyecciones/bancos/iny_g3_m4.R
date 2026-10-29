# Inyecciones de la red (fig 05) portadas a la estructura de capas de WJPr::wjp_network (revisión 5, M4): mismo error
# semántico que bancos/iny_g3.R (red_*). Capas de wjp_network: 1 aristas (geom_segment: x, y, xend, yend, w, width,
# alpha, lw), 2 nodos (geom_polygon: x, y, id, fill), 3 un texto por nodo (geom_text: x, y, text = "nombre\nvalor",
# ink). La capa k se rehace con ggplot2::layer() con otra data. Se añade al final de R/09_wjpr.R de la copia y se quita
# después (iny_run_m4.sh). Cambian solo el dibujo (df intacto). INY = nombre de la inyección.

.iny <- Sys.getenv("INY")
.iny_data <- function(p, k, f) {
  l <- p$layers[[k]]
  pr <- c(l$geom_params, l$stat_params, l$aes_params); pr <- pr[!duplicated(names(pr))]
  p$layers[[k]] <- ggplot2::layer(geom = l$geom, stat = l$stat, position = l$position, data = f(l$data),
                                  mapping = ggplot2::aes(!!!as.list(l$mapping)), params = pr,
                                  inherit.aes = l$inherit.aes, show.legend = FALSE)
  p
}
.layer_of <- function(p, cls) which(vapply(p$layers, function(l) inherits(l$geom, cls), logical(1)))[1]

.net_orig <- wjpr_network
wjpr_network <- function(df, spec, ...) {
  p <- .net_orig(df, spec, ...); a <- attr(p, "wjpr")
  nd <- df[df$kind == "node", ]; nd <- nd[order(nd$order), ]
  at <- function(d, lab) which(abs(d$x - nd$x[nd$label == lab]) < 1e-9 & abs(d$y - nd$y[nd$label == lab]) < 1e-9)
  ls <- .layer_of(p, "GeomSegment"); lp <- .layer_of(p, "GeomPolygon"); lt <- .layer_of(p, "GeomText")
  # partes del texto de un nodo: nombre (todas las líneas menos la última) y valor (la última)
  nm_of <- function(s) sub("\n[^\n]*$", "", s); val_of <- function(s) sub("^.*\n", "", s)
  if (.iny == "red_nombres") p <- .iny_data(p, lt, function(d) {        # nombres Consumer ↔ Land
    i <- at(d, "Consumer"); j <- at(d, "Land")
    ti <- d$text[i]; tj <- d$text[j]
    d$text[i] <- paste(nm_of(tj), val_of(ti), sep = "\n"); d$text[j] <- paste(nm_of(ti), val_of(tj), sep = "\n"); d
  })
  if (.iny == "red_porcentajes") p <- .iny_data(p, lt, function(d) {    # porcentajes Consumer ↔ Land
    i <- at(d, "Consumer"); j <- at(d, "Land")
    ti <- d$text[i]; tj <- d$text[j]
    d$text[i] <- paste(nm_of(ti), val_of(tj), sep = "\n"); d$text[j] <- paste(nm_of(tj), val_of(ti), sep = "\n"); d
  })
  if (.iny == "red_gruesa") p <- .iny_data(p, ls, function(d) d[-which.max(d$w), ])
  if (.iny == "red_fina") p <- .iny_data(p, ls, function(d) d[-which.min(d$w), ])
  if (.iny == "red_nodo") {                        # Education 0.06 unidades de layout (≈ 24 pt) a la derecha, con sus aristas
    k <- which(nd$label == "Education"); dx <- 0.06; ex <- nd$x[k]; ey <- nd$y[k]
    p <- .iny_data(p, lp, function(d) { d$x[d$id == k] <- d$x[d$id == k] + dx; d })
    p <- .iny_data(p, lt, function(d) { i <- at(d, "Education"); d$x[i] <- d$x[i] + dx; d })
    p <- .iny_data(p, ls, function(d) {
      a0 <- abs(d$x - ex) < 1e-9 & abs(d$y - ey) < 1e-9; a1 <- abs(d$xend - ex) < 1e-9 & abs(d$yend - ey) < 1e-9
      d$x[a0] <- d$x[a0] + dx; d$xend[a1] <- d$xend[a1] + dx; d
    })
  }
  attr(p, "wjpr") <- a
  p
}
