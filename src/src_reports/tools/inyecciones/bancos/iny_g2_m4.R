# Inyecciones de la matriz (06) y la dispersión (03) portadas a la estructura de capas de WJPr 1.2.0 (revisión 5,
# M4): mismo error semántico que bancos/iny_g2.R, sobre los datos de wjp_heatmap (x, y, text, fill_value, fill_colour,
# label_colour; filas = y, columnas = x) y de wjp_scatter (.x, .y, .label, panel, .hollow, .shape; rótulos en la capa
# geom_text_repel con sus datos). Se añade al final de R/09_wjpr.R de la copia y se quita después (iny_run_m4.sh).
# Cambian solo el dibujo (df intacto). INY = nombre de la inyección.

.iny <- Sys.getenv("INY")
.rebuild <- function(l, data) {
  pr <- c(l$geom_params, l$stat_params, l$aes_params); pr <- pr[!duplicated(names(pr))]
  ggplot2::layer(geom = l$geom, stat = l$stat, position = l$position, data = data, mapping = l$mapping,
                 params = pr, inherit.aes = l$inherit.aes, show.legend = FALSE)
}
.swap_axis_labels <- function(p, ax, a, b) {
  sc <- p$scales$get_scales(ax); f <- if (is.null(sc)) NULL else sc$labels
  g <- function(x) {
    y <- if (is.function(f)) f(x) else as.character(x)
    i <- which(x == a); j <- which(x == b)
    if (length(i) && length(j)) y[c(i, j)] <- y[c(j, i)]
    y
  }
  if (is.null(sc)) return(p + (if (ax == "y") ggplot2::scale_y_discrete(labels = g) else ggplot2::scale_x_discrete(labels = g)))
  sc$labels <- g
  p
}

.mx_orig <- wjpr_matrix
wjpr_matrix <- function(df, spec, ...) {
  p <- .mx_orig(df, spec, ...); a <- attr(p, "wjpr")
  d <- p$data
  cell <- function(r, c) which(d$y == r & d$x == c)
  if (.iny == "mx_textos") {                    # dos textos de celda intercambiados (misma fila)
    i <- cell("Public Services", "Consumer"); j <- cell("Public Services", "Community")
    d$text[c(i, j)] <- d$text[c(j, i)]
  }
  if (.iny == "mx_filas") p <- .swap_axis_labels(p, "y", "Consumer", "Community")
  if (.iny == "mx_traspuesta") {                # cada celda con el texto, la banda y el color de texto de su simétrica
    k <- match(paste(d$x, d$y), paste(d$y, d$x))
    d$text <- d$text[k]; d$fill_value <- d$fill_value[k]; d$fill_colour <- d$fill_colour[k]
    d$label_colour <- d$label_colour[k]
  }
  if (.iny == "mx_banda") {                     # una celda con el color de otra banda (y el texto que wjp_heatmap le da)
    i <- cell("Public Services", "Consumer"); j <- which(as.character(d$fill_value) == "25% to 50%")[1]
    d$fill_value[i] <- d$fill_value[j]; d$fill_colour[i] <- d$fill_colour[j]; d$label_colour[i] <- d$label_colour[j]
  }
  p$data <- d
  attr(p, "wjpr") <- a
  p
}

.sc_orig <- wjpr_scatter
wjpr_scatter <- function(df, spec, ...) {
  p <- .sc_orig(df, spec, ...); a <- attr(p, "wjpr")
  d <- p$data
  pt <- function(g, l) which(as.character(d$panel) == g & d$.label == l)
  rep_k <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomTextRepel"), logical(1)))
  if (.iny == "sc_x3") { i <- pt("Rural", "Accidental"); d$.x[i] <- d$.x[i] + 3 }        # un punto +3 pp en x
  if (.iny == "sc_hueco") { i <- pt("Rural", "Consumer"); d$.hollow[i] <- TRUE; d$.shape[i] <- 1 }   # hueco con n ≥ 30
  if (.iny %in% c("sc_rotulos", "sc_rotulo_otro")) {
    l <- p$layers[[rep_k]]; ld <- l$data
    r <- function(g, lab) which(as.character(ld$panel) == g & ld$.label == lab)
    if (.iny == "sc_rotulos") {                  # dos rótulos intercambiados
      i <- r("Rural", "Community"); j <- r("Rural", "Consumer"); ld$.label[c(i, j)] <- ld$.label[c(j, i)]
    } else {                                     # el rótulo de Community junto al punto de Accidental
      i <- r("Rural", "Community"); s <- pt("Rural", "Accidental")
      ld$.x[i] <- d$.x[s]; ld$.y[i] <- d$.y[s]
    }
    p$layers[[rep_k]] <- .rebuild(l, ld)
  }
  p$data <- d
  attr(p, "wjpr") <- a
  p
}
