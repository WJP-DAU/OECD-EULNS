## 09_wjpr.R — figuras con WJPr + complementos de código ---------------------------------------------------------
## Origen: nuevo (2026-10-23, pedido del usuario: usar WJPr en el mayor número de figuras y complementarlo con código
## hasta el diseño publicado). Para cada tipo de figura, wjpr_<tipo>(df, spec, ...) parte de la función de WJPr del
## tipo (o de WJP_theme()/wjp_palette() cuando WJPr no tiene una) y la complementa hasta acercarse a lo publicado.
## Recibe el df del contrato (§4.4: order, kind, grouping, label, value, value2, text, text2 + extras) y la spec,
## igual que chart_<tipo>(), y devuelve un ggplot con attr(p, "wjpr") = list(base, complementos, compara, drawn,
## rotulos) (wjpr_meta). La exporta y la mide tools/wjpr_gallery.R; desde el 23-oct es el motor de main.R (metodo
## wjpr: plot = wjpr_<tipo>(df, spec)). Verificación del dibujo (R/07_verify.R): wjpr_similarity() (parecido global y
## local con la publicada) y, en los adaptadores que la exponen, lo dibujado por fila (attr(p, "wjpr")$drawn,
## wjpr_drawn(): WJPR_DRAWN_COLS + las opcionales WJPR_DRAWN_OPT, leídas del dibujo con wjpr_slots(), wjpr_slot_of() y
## wjpr_drawn_texts()).
## Cada complemento lleva "# complemento: <qué le falta a WJPr>": es la fuente del documento de brechas.
## Solo funciones: nada se ejecuta al cargar; WJPr solo se pide al llamar (deps.csv lo declara opcional).

WJPR_FAMILY <- "inter"   # familia registrada por setup_fonts() (Inter Tight); la de las figuras publicadas


## Utilidades comunes ----------------------------------------------------------------------------------------------

# Interruptor único de los complementos: options(wjpr.complementos = FALSE) dibuja cada figura con WJPr sola (la línea
# de base que mide la galería con --sin-complementos: lo que el paquete logra sin ayuda)
wjpr_complementos <- function() isTRUE(getOption("wjpr.complementos", TRUE))

wjpr_require <- function() {
  if (!requireNamespace("WJPr", quietly = TRUE)) stop("WJPr no está instalado: es el motor de dibujo (metodo wjpr, deps.csv)", call. = FALSE)
  if (wjpr_has_fun("wjp_font_family")) options(wjpr.family = WJPR_FAMILY)   # main: fuente nativa (sobra wjpr_font)
  invisible(TRUE)
}

# Capacidades de la versión de WJPr cargada: cada adaptador usa lo nativo cuando existe y el complemento cuando no,
# así una versión nueva del paquete reduce los complementos sin tocar este archivo (salvo lo que cambie de nombre)
wjpr_has_fun <- function(fun) requireNamespace("WJPr", quietly = TRUE) && exists(fun, envir = asNamespace("WJPr"), inherits = FALSE)
wjpr_has_arg <- function(fun, arg) wjpr_has_fun(fun) && arg %in% names(formals(get(fun, envir = asNamespace("WJPr"))))
wjpr_native_font <- function() wjpr_has_fun("wjp_font_family")
# Versión y commit de la build cargada ("1.1.0@382aee9"; "local" si se instaló desde un clon): dos builds con el mismo
# número de versión tienen código distinto (1.1.0 del 13-jul frente a main)
wjpr_build <- function() {
  if (!requireNamespace("WJPr", quietly = TRUE)) return(NA_character_)
  d <- utils::packageDescription("WJPr")
  paste0(d$Version, "@", substr(d$RemoteSha %||% d$GithubSHA1 %||% "local", 1, 7))
}

# Marca el ggplot con la función de WJPr de partida y los complementos aplicados (los lee la galería)
# compara = columnas del df cuyos textos se imprimen tal cual (la galería los busca en el dibujo); p. ej. en la matriz
# text2 es la banda de color, no un texto
# corregido (V3-02, V3-07): drawn y rotulos, para que verify mire el dibujo fila a fila y no solo su parecido global.
# drawn = function(b) que, con b = ggplot_build() del gráfico final, devuelve lo dibujado por fila (WJPR_DRAWN_COLS +
# WJPR_DRAWN_OPT, el drawn de chart_out en svg_r; wjpr_drawn()): figures lo escribe en tables/<stem>.drawn.csv y
# compara el texto, el nombre impreso y el valor de cada fila con los de df; verify compara frac/frac2, el texto, el
# nombre impreso, el valor y el orden con la referencia. Lo exponen (revisión 4) wjpr_grouped_bars (la implementación
# de referencia, wjpr_gb_drawn), wjpr_paired_bars, wjpr_severity_bars, wjpr_dumbbell, wjpr_matrix, wjpr_scatter,
# wjpr_network y wjpr_sankey (29 figuras); faltan hbars, columns, funnel, cards y upset. Otro adaptador lo añade con
# wjpr_meta(…, drawn = ), y wjpr_slots()/wjpr_slot_of() (o wjpr_pos_slots()/wjpr_pos_slot_of() con un eje mapeado) le
# dan la fila de cada barra o texto y wjpr_drawn_texts() lo impreso en cada posición. NULL: el dibujo solo lo miden el
# parecido y el parecido local. rotulos = rótulos fijos
# del diseño que el adaptador dibuja y df no trae (nombres de nodo, categorías destacadas): entran con los de compara
# en la comparación de textos (drawn_text_diff_wjpr, 06_export.R); los declaran wjpr_scatter (categorías rotuladas,
# 3) y wjpr_network (nombres de nodo, 5).
# corregido (revisión 4): marcas = function(b) con las marcas fijas del diseño que no tienen fila en df y faltan en el
# dibujo (character(), vacío si están todas; p. ej. la línea de corte de la 7, wjpr_sev_cut_missing): figures da
# estado "dibujo" si devuelve alguna. NULL: el adaptador no tiene marcas fijas que comprobar.
wjpr_meta <- function(p, base, complementos = character(), compara = c("text", "text2"), drawn = NULL,
                      rotulos = character(), marcas = NULL) {
  if (!is.null(drawn) && !is.function(drawn)) stop("wjpr_meta: drawn debe ser una función de ggplot_build()", call. = FALSE)
  if (!is.null(marcas) && !is.function(marcas)) stop("wjpr_meta: marcas debe ser una función de ggplot_build()", call. = FALSE)
  attr(p, "wjpr") <- list(base = base, complementos = unique(complementos), compara = compara, drawn = drawn,
                          rotulos = as.character(rotulos), marcas = marcas)
  p
}

# complemento: WJPr 1.1.0 fija family = "Lato Full" dentro de las capas (geom_text, ggtext::geom_richtext) y de los
# elementos de WJP_theme() y de sus temas internos, sin argumento para cambiarla; sin esa fuente, svglite usa una de
# reemplazo con otras métricas. Se reemplaza en capas (copias, sin tocar el objeto de WJPr) y en el tema.
wjpr_font <- function(p, family = WJPR_FAMILY) {
  p$layers <- lapply(p$layers, function(l) {
    l2 <- ggplot2::ggproto(NULL, l)
    if (!is.null(l2$aes_params$family)) l2$aes_params$family <- family
    if (!is.null(l2$geom_params$family)) l2$geom_params$family <- family
    if ("family" %in% names(l2$mapping)) {             # familia mapeada en aes() (geom_richtext de wjp_edgebars)
      l2$mapping$family <- NULL; l2$aes_params$family <- family
    }
    l2
  })
  is_text <- vapply(p$theme, function(e) inherits(e, c("element_text", "ggplot2::element_text")), logical(1))
  for (nm in unique(c("text", names(p$theme)[is_text]))) {
    el <- p$theme[[nm]]
    add <- do.call(ggplot2::theme, stats::setNames(list(ggplot2::element_text(family = family)), nm))
    # element_markdown (ggtext, S3) no se fusiona con element_text en ggplot2 4 ("Can't merge"): se reemplaza en el
    # tema (%+replace% entre temas; el gráfico es S7, p@theme) una copia del mismo elemento con la familia nueva
    p <- if (inherits(el, "element_markdown") && is.list(el)) {
      el$family <- family; p@theme <- p@theme %+replace% do.call(ggplot2::theme, stats::setNames(list(el), nm)); p
    } else tryCatch(p + add, error = function(e) {
      if (!is.list(el)) stop(e)
      el$family <- family; p@theme <- p@theme %+replace% do.call(ggplot2::theme, stats::setNames(list(el), nm)); p
    })
  }
  p
}

# Tema de partida: WJP_theme() con Inter Tight (por ptheme, el argumento documentado de las funciones de WJPr)
wjpr_theme <- function(family = WJPR_FAMILY) {
  wjpr_require()
  e <- ggplot2::element_text(family = family)
  WJPr::WJP_theme() + ggplot2::theme(text = e, axis.text = e, axis.text.x = e, axis.text.y = e, axis.title = e,
                                     axis.title.x = e, axis.title.y = e, strip.text = e, legend.text = e,
                                     legend.title = e, plot.title = e)
}

## Lo impreso en el dibujo (wjpr_drawn_texts) -------------------------------------------------------------------
## complemento: WJPr 1.1.0 y main no dicen qué texto imprime un gráfico ni dónde; WJPr::wjp_drawn(what = "text") (en
## preparación en el clon del paquete, R/drawn.R: aún en ninguna build) lo hará. wjpr_drawn_texts(b) devuelve lo mismo
## con su forma (source "layer" | "axis-x" | "axis-y" | "strip", layer, PANEL, label sin markup, x, y en coordenadas
## de datos — las etiquetas de eje en la posición de su break —, colour) y además descarta lo que no se ve: un texto
## con alfa 0, del color de lo que tiene debajo (el fondo del panel o del gráfico, blanco por defecto, o el relleno de
## una barra, celda o polígono de una capa anterior: un texto blanco sobre una celda oscura sí se ve), tapado por una
## marca opaca de una capa posterior, de tamaño 0, o cuyo elemento del tema es element_blank (ejes, strips). Es lo que
## leen los adaptadores para saber qué etiqueta está impresa en cada posición (label_drawn, grouping_drawn) y lo que
## cuenta wjpr_texts(). Cuando WJPr::wjp_drawn exista en la build fijada, esta función la llama y conserva el filtro.

WJPR_TEXT_GEOMS <- c("GeomText", "GeomLabel", "GeomRichText", "GeomTextBox", "GeomTextRepel", "GeomLabelRepel")
WJPR_VISIBLE_MIN <- 0.02   # |Δ| RGB (0-1) mínima entre el texto (con su alfa) y lo que tiene debajo para contar como impreso

# Texto de una marca: sin etiquetas HTML (<br> → salto de línea, como wjp_drawn) y, si se dibuja como Markdown
# (ggtext: GeomRichText/GeomTextBox, element_markdown/element_textbox), sin escapes de Markdown ("\*", "1\.") y con
# las entidades HTML resueltas (&amp; → &): lo que se lee en la figura
wjpr_plain_text <- function(x, markdown = FALSE) {
  x <- as.character(x)
  x <- gsub("<br\\s*/?>", "\n", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("<[^>]+>", "", x, perl = TRUE)
  if (!markdown) return(trimws(x))
  x <- gsub("\\\\([[:punct:]])", "\\1", x, perl = TRUE)
  ent <- c("&lt;" = "<", "&gt;" = ">", "&quot;" = "\"", "&#39;" = "'", "&apos;" = "'", "&nbsp;" = " ")
  for (k in names(ent)) x <- gsub(k, ent[[k]], x, fixed = TRUE)
  num <- function(m, base) vapply(m, function(s) intToUtf8(strtoi(s, base)), "")
  for (re in c("&#([0-9]+);", "&#[xX]([0-9A-Fa-f]+);")) {
    m <- regmatches(x, gregexpr(re, x, perl = TRUE))
    regmatches(x, gregexpr(re, x, perl = TRUE)) <- lapply(m, function(v) num(sub(re, "\\1", v, perl = TRUE), if (grepl("x", re)) 16L else 10L))
  }
  trimws(gsub("&amp;", "&", x, fixed = TRUE))
}

# Nombre impreso normalizado para comparar (espacios, tabuladores y saltos de línea → un espacio, sin bordes)
wjpr_norm_text <- function(x) trimws(gsub("[[:space:] ]+", " ", as.character(x), perl = TRUE))

# Color de un texto con markup: el primer color: de un style (el de la caja que lo pinta en ggtext); NA si no trae
wjpr_markup_colour <- function(x) {
  m <- regmatches(x, regexpr("color\\s*:\\s*[#A-Za-z0-9]+", x, perl = TRUE, ignore.case = TRUE))
  out <- rep(NA_character_, length(x))
  out[grepl("color\\s*:", x, perl = TRUE, ignore.case = TRUE)] <- sub("^color\\s*:\\s*", "", m, ignore.case = TRUE)
  out
}

# RGB (0-1) y alfa de colores (NA o "transparent" → alfa 0)
wjpr_rgba <- function(col) {
  col <- as.character(col); ok <- !is.na(col) & nzchar(col)
  m <- matrix(c(1, 1, 1, 0), 4, length(col))
  if (any(ok)) m[, ok] <- tryCatch(grDevices::col2rgb(col[ok], alpha = TRUE) / 255, error = function(e) m[, ok])
  m
}

# Color de fondo (RGB 0-1) de un elemento del tema (element_rect con fill) sobre el de abajo; sin relleno, el de abajo
wjpr_theme_fill <- function(th, name, under = c(1, 1, 1)) {
  el <- tryCatch(ggplot2::calc_element(name, th), error = function(e) NULL)
  if (is.null(el) || inherits(el, "element_blank") || is.null(el$fill)) return(under)
  f <- wjpr_rgba(el$fill)[, 1]
  f[1:3] * f[4] + under * (1 - f[4])
}

# ¿Contiene la marca la posición (x, y)? Rectángulos (xmin..ymax: barras, celdas, rect) y polígonos (por group, regla
# par-impar). Devuelve una matriz lógica textos × filas (rect) o textos × grupos (polígono) y el relleno de cada marca
wjpr_marks_at <- function(d, geom, px, py) {
  has <- function(...) all(c(...) %in% names(d))
  rgba <- function(fill, alpha) { m <- wjpr_rgba(fill); a <- suppressWarnings(as.numeric(alpha)); m[4, ] <- m[4, ] * coalesce(a, 1); m }
  if (has("xmin", "xmax", "ymin", "ymax", "fill")) {
    x0 <- pmin(d$xmin, d$xmax); x1 <- pmax(d$xmin, d$xmax); y0 <- pmin(d$ymin, d$ymax); y1 <- pmax(d$ymin, d$ymax)
    inside <- outer(px, x0, ">=") & outer(px, x1, "<=") & outer(py, y0, ">=") & outer(py, y1, "<=")
    inside[is.na(inside)] <- FALSE
    return(list(inside = inside, rgba = rgba(d$fill, if ("alpha" %in% names(d)) d$alpha else NA)))
  }
  if (inherits(geom, "GeomPolygon") && has("x", "y", "fill")) {
    g <- if ("group" %in% names(d)) d$group else rep(1L, nrow(d))
    ids <- unique(g)
    inside <- vapply(ids, function(id) {
      vx <- d$x[g == id]; vy <- d$y[g == id]; n <- length(vx); j <- c(n, seq_len(n - 1))
      vapply(seq_along(px), function(k) {
        cr <- ((vy > py[k]) != (vy[j] > py[k])) & (px[k] < (vx[j] - vx) * (py[k] - vy) / (vy[j] - vy) + vx)
        sum(cr, na.rm = TRUE) %% 2 == 1
      }, logical(1))
    }, logical(length(px)))
    inside <- matrix(inside, nrow = length(px))
    first <- match(ids, g)
    return(list(inside = inside, rgba = rgba(d$fill[first], if ("alpha" %in% names(d)) d$alpha[first] else NA)))
  }
  NULL
}

# ¿Se ven los textos de la capa i (posiciones px, py del panel pan, color col, alfa a)? Fondo = el del panel con las
# marcas rellenas de las capas anteriores que contienen la posición compuestas encima (en orden); tapado = una marca
# opaca de una capa posterior en esa posición. Visible si alfa > 0 y |texto − fondo| > WJPR_VISIBLE_MIN en algún canal
# corregido (revisión 4, integración): punto de prueba de un texto = su ancla desplazada un épsilon hacia donde se
# extiende el texto (hjust, vjust, angle; con coord_flip los ejes de datos se intercambian). Un valor anclado en el
# extremo de su barra y dibujado hacia fuera (la 29: hjust -0.15 a 90°, del color de la barra) contaba como dentro
# de ella, del mismo color, y no visible (15 textos «faltan» en la galería). Solo cambia el resultado cuando el ancla
# cae justo en el borde de una marca; just no numérico («inward», …) = centrado
wjpr_text_probe <- function(x, y, hjust, vjust, angle, flip = FALSE) {
  jn <- function(v) {
    v <- as.character(v); n <- suppressWarnings(as.numeric(v))
    n[is.na(n) & v %in% c("left", "bottom")] <- 0; n[is.na(n) & v %in% c("right", "top")] <- 1
    coalesce(n, 0.5)
  }
  n <- length(x); ux <- 0.5 - rep_len(jn(hjust), n); uy <- 0.5 - rep_len(jn(vjust), n)
  a <- coalesce(suppressWarnings(as.numeric(rep_len(angle, n))), 0) * pi / 180
  dx <- cos(a) * ux - sin(a) * uy; dy <- sin(a) * ux + cos(a) * uy
  if (flip) { t <- dx; dx <- dy; dy <- t }
  eps <- function(v) 1e-7 * pmax(1, abs(v))
  list(x = x + sign(round(dx, 9)) * eps(x), y = y + sign(round(dy, 9)) * eps(y))
}

wjpr_text_visible <- function(b, i, pan, px, py, col, a, bg_panel, box = NULL) {
  n <- length(px); if (!n) return(logical())
  bg <- matrix(bg_panel, 3, n)
  covered <- rep(FALSE, n)
  for (j in seq_along(b$data)) {
    if (j == i || inherits(b$plot$layers[[j]]$geom, WJPR_TEXT_GEOMS)) next
    d <- b$data[[j]]
    if (!nrow(d) || !"PANEL" %in% names(d)) next
    for (pn in unique(pan)) {
      k <- which(pan == pn); dj <- d[as.character(d$PANEL) == pn, , drop = FALSE]
      if (!nrow(dj)) next
      mk <- wjpr_marks_at(dj, b$plot$layers[[j]]$geom, px[k], py[k])
      if (is.null(mk)) next
      for (m in seq_len(ncol(mk$inside))) {
        hit <- k[mk$inside[, m]]
        if (!length(hit) || mk$rgba[4, m] <= 0) next
        if (j < i) bg[, hit] <- mk$rgba[1:3, m] * mk$rgba[4, m] + bg[, hit, drop = FALSE] * (1 - mk$rgba[4, m])
        else if (mk$rgba[4, m] >= 0.999) covered[hit] <- TRUE
      }
    }
  }
  if (!is.null(box)) {                                  # geom_label: la caja de la etiqueta (fill) va bajo su texto
    bx <- wjpr_rgba(box); bg <- bx[1:3, , drop = FALSE] * rep(bx[4, ], each = 3) + bg * rep(1 - bx[4, ], each = 3)
  }
  tc <- wjpr_rgba(col); ta <- tc[4, ] * coalesce(suppressWarnings(as.numeric(a)), 1)
  delta <- apply(abs(tc[1:3, , drop = FALSE] - bg), 2, max) * ta
  !covered & ta > 0 & delta > WJPR_VISIBLE_MIN
}

# Paneles que dibujan el eje (lado side) en el layout: facet_grid solo en la columna o fila del borde (salvo axes =
# "all"); facet_wrap en todos si la escala es libre, si no en el borde (o sin vecino de ese lado); sin facetas, todos
wjpr_axis_panels <- function(b, ax, side) {
  lay <- b$layout$layout; fp <- b$layout$facet$params %||% list()
  all_ax <- isTRUE(fp$draw_axes[[ax]]) || inherits(b$layout$facet, "FacetNull")
  if (inherits(b$layout$facet, "FacetWrap") && isTRUE(fp$free[[ax]])) all_ax <- TRUE
  if (all_ax) return(as.character(lay$PANEL))
  edge <- switch(side, left = lay$COL == min(lay$COL), right = lay$COL == max(lay$COL),
                 top = lay$ROW == min(lay$ROW), bottom = lay$ROW == max(lay$ROW), rep(TRUE, nrow(lay)))
  if (inherits(b$layout$facet, "FacetWrap")) {            # sin vecino del lado del eje (fila incompleta)
    nb <- switch(side, left = paste(lay$ROW, lay$COL - 1), right = paste(lay$ROW, lay$COL + 1),
                 top = paste(lay$ROW - 1, lay$COL), bottom = paste(lay$ROW + 1, lay$COL))
    edge <- edge | !(nb %in% paste(lay$ROW, lay$COL))
  }
  as.character(lay$PANEL[edge])
}

wjpr_drawn_texts <- function(b) {
  th <- ggplot2::complete_theme(b$plot$theme)
  bg_plot <- wjpr_theme_fill(th, "plot.background")
  bg_panel <- wjpr_theme_fill(th, "panel.background", bg_plot)
  rows <- list()
  # Capas de texto (columnas por nombre exacto: d$x de un data.frame casaría con xmin)
  for (i in seq_along(b$data)) {
    l <- b$plot$layers[[i]]
    if (!inherits(l$geom, WJPR_TEXT_GEOMS)) next
    d <- as.data.frame(b$data[[i]])
    if (!nrow(d) || !"label" %in% names(d)) next
    cl <- function(nm, default) if (nm %in% names(d)) d[[nm]] else rep(default, nrow(d))
    md <- inherits(l$geom, c("GeomRichText", "GeomTextBox"))
    raw <- as.character(d[["label"]])
    x <- suppressWarnings(as.numeric(cl("x", NA))); y <- suppressWarnings(as.numeric(cl("y", NA)))
    lab <- wjpr_plain_text(raw, markdown = md)
    size <- suppressWarnings(as.numeric(cl("size", 1)))
    ok <- !is.na(raw) & !is.na(lab) & nzchar(lab) & is.finite(x) & is.finite(y) & coalesce(size, 1) > 0
    if (!any(ok)) next
    col <- as.character(cl("colour", l$aes_params$colour %||% "black"))
    if (md) col <- coalesce(wjpr_markup_colour(raw), col)
    pan <- as.character(cl("PANEL", "1"))
    box <- if (inherits(l$geom, c("GeomLabel", "GeomLabelRepel"))) as.character(cl("fill", "white"))[ok] else NULL
    pr <- wjpr_text_probe(x[ok], y[ok], cl("hjust", 0.5)[ok], cl("vjust", 0.5)[ok], cl("angle", 0)[ok],
                          inherits(b$layout$coord, "CoordFlip"))
    vis <- wjpr_text_visible(b, i, pan[ok], pr$x, pr$y, col[ok], cl("alpha", NA)[ok], bg_panel, box)
    k <- which(ok)[vis]
    if (length(k)) rows[[length(rows) + 1]] <- tibble(source = "layer", layer = i, PANEL = pan[k], label = lab[k],
                                                       x = x[k], y = y[k], colour = col[k])
  }
  # Etiquetas de eje: en cada panel que dibuja el eje, las de sus breaks (posición = la del break en datos)
  flipped <- inherits(b$layout$coord, "CoordFlip")
  gd <- b$plot$guides
  for (k in seq_along(b$layout$panel_params)) {
    pp <- b$layout$panel_params[[k]]; pan <- as.character(b$layout$layout$PANEL[k])
    for (ax in c("x", "y")) {
      sc <- pp[[ax]]
      if (is.null(sc) || !is.function(sc$get_labels)) next
      side <- sc$position %||% if (ax == "x") "bottom" else "left"
      guide <- sc$guide
      g_plot <- tryCatch(if (inherits(gd, "Guides")) gd$guides[[match(ax, gd$aesthetics)]] else NULL, error = function(e) NULL)
      if (identical(guide, "none") || inherits(guide, "GuideNone") || identical(g_plot, "none") || inherits(g_plot, "GuideNone")) next
      el <- tryCatch(ggplot2::calc_element(paste0("axis.text.", ax, ".", side), th), error = function(e) NULL)
      if (is.null(el) || inherits(el, "element_blank")) next
      if (!pan %in% wjpr_axis_panels(b, ax, side)) next
      brk <- tryCatch(sc$get_breaks(), error = function(e) NULL); labs <- tryCatch(sc$get_labels(), error = function(e) NULL)
      if (!length(brk) || length(labs) != length(brk)) next
      pos <- if (isTRUE(sc$scale_is_discrete)) suppressWarnings(as.numeric(sc$map(brk))) else suppressWarnings(as.numeric(brk))
      raw <- vapply(labs, function(v) paste(as.character(v), collapse = " "), "")
      md <- inherits(el, c("element_markdown", "element_textbox"))
      lab <- wjpr_plain_text(raw, markdown = md)
      # corregido (revisión 4, G1): el color y el tamaño del elemento pueden ser un vector por break (las 10 y 15): rep()
      # daba n² colores y n² − n filas con label NA; rep_len los alinea con las etiquetas
      cl <- rep_len(as.character(el$colour %||% "black"), length(raw))
      col <- if (md) coalesce(wjpr_markup_colour(raw), cl) else cl
      tc <- wjpr_rgba(col)
      vis <- !is.na(brk) & is.finite(pos) & !is.na(lab) & nzchar(lab) & rep_len(el$size %||% 1, length(raw)) > 0 &
        apply(abs(tc[1:3, , drop = FALSE] - bg_plot), 2, max) * tc[4, ] > WJPR_VISIBLE_MIN
      if (!any(vis)) next
      s2 <- if (flipped) setdiff(c("x", "y"), ax) else ax
      rows[[length(rows) + 1]] <- tibble(source = paste0("axis-", s2), layer = NA_integer_, PANEL = pan, label = lab[vis],
                                         x = if (s2 == "x") pos[vis] else NA_real_, y = if (s2 == "y") pos[vis] else NA_real_,
                                         colour = col[vis])
    }
  }
  # Strips de faceta: el valor de la faceta en el panel del borde donde se dibuja (sin el labeller), si su elemento se ve
  lay <- b$layout$layout; fp <- b$layout$facet$params %||% list()
  strip_set <- if (inherits(b$layout$facet, "FacetGrid")) {
    sw <- fp$switch %||% ""
    list(list(vars = names(fp$rows), el = if (sw %in% c("y", "both")) "strip.text.y.left" else "strip.text.y.right",
              edge = if (sw %in% c("y", "both")) lay$COL == min(lay$COL) else lay$COL == max(lay$COL)),
         list(vars = names(fp$cols), el = if (sw %in% c("x", "both")) "strip.text.x.bottom" else "strip.text.x.top",
              edge = if (sw %in% c("x", "both")) lay$ROW == max(lay$ROW) else lay$ROW == min(lay$ROW)))
  } else if (inherits(b$layout$facet, "FacetWrap")) {
    sp <- fp$strip.position %||% "top"
    list(list(vars = names(fp$facets), el = paste0("strip.text.", if (sp %in% c("left", "right")) "y." else "x.", sp),
              edge = rep(TRUE, nrow(lay))))
  } else list()
  for (s in strip_set) {
    if (!length(s$vars)) next
    el <- tryCatch(ggplot2::calc_element(s$el, th), error = function(e) NULL)
    if (is.null(el) || inherits(el, "element_blank")) next
    bg_s <- wjpr_theme_fill(th, sub("^strip\\.text", "strip.background", s$el), bg_plot)
    for (v in intersect(s$vars, names(lay))) {
      raw <- as.character(lay[[v]][s$edge]); pan <- as.character(lay$PANEL[s$edge])
      md <- inherits(el, c("element_markdown", "element_textbox"))
      lab <- wjpr_plain_text(raw, markdown = md); col <- rep(el$colour %||% "black", length(raw))
      tc <- wjpr_rgba(col)
      vis <- !is.na(lab) & nzchar(lab) & apply(abs(tc[1:3, , drop = FALSE] - bg_s), 2, max) * tc[4, ] > WJPR_VISIBLE_MIN
      if (any(vis)) rows[[length(rows) + 1]] <- tibble(source = "strip", layer = NA_integer_, PANEL = pan[vis], label = lab[vis],
                                                       x = NA_real_, y = NA_real_, colour = col[vis])
    }
  }
  out <- bind_rows(tibble(source = character(), layer = integer(), PANEL = character(), label = character(),
                          x = numeric(), y = numeric(), colour = character()), rows)
  out
}

# Textos efectivamente dibujados por las capas de texto (posición finita), sin marcas de texto enriquecido. b =
# ggplot_build(p) si ya está construido (figures lo construye una vez para los textos y para lo dibujado por fila)
# corregido (revisión 4): solo los que se ven (wjpr_drawn_texts: alfa 0, del color de lo que tienen debajo o tapados
# no cuentan); antes un valor oculto (alfa 0, blanco sobre blanco) contaba como dibujado y el multiconjunto daba
# «iguales». Con Markdown (ggtext) el texto es el que se lee (&amp; → &, sin escapes)
wjpr_texts <- function(p, b = NULL) {
  if (is.null(b)) b <- suppressWarnings(ggplot2::ggplot_build(p))
  tx <- wjpr_drawn_texts(b)
  tx$label[tx$source == "layer"]
}

# Multiconjunto de textos: lo que falta (esperado y no dibujado) y lo que sobra (dibujado y no esperado)
wjpr_text_diff <- function(drawn, expected) {
  expected <- expected[!is.na(expected) & nzchar(expected)]
  k <- union(drawn, expected)
  n1 <- as.integer(table(factor(drawn, levels = k))); n2 <- as.integer(table(factor(expected, levels = k)))
  list(faltan = rep(k, pmax(n2 - n1, 0)), sobran = rep(k, pmax(n1 - n2, 0)))
}

# Lo dibujado por fila: attr(p, "wjpr")$drawn(b) con b = ggplot_build(p) (o el ya construido) → tibble con
# WJPR_DRAWN_COLS: order = posición visual (1 = arriba o a la izquierda), kind/grouping/label = la fila del df que ocupa
# esa posición SEGÚN EL DIBUJO (el nivel del eje o la geometría en esa posición, nunca df copiado en orden),
# frac/frac2 = fracción dibujada (como el drawn de svg_r: valor / escala de la pista; NA sin barra) y text/text2 = lo
# impreso en esa posición ("" si nada). NULL si el adaptador no lo expone; error legible si la función no cumple el
# contrato (la figura queda en error: un drawn roto no se ignora).
# corregido (revisión 4): las columnas opcionales WJPR_DRAWN_OPT (contrato extendido), porque con solo las de arriba
# un error que conserva el conjunto de textos pasaba (etiquetas Male/Female intercambiadas, un encabezado cambiado):
# label_drawn y grouping_drawn (character) = el nombre IMPRESO en la posición de la marca de esa fila (etiqueta del eje,
# encabezado de grupo bajo el que queda, rótulo junto al punto, nombre dentro del nodo; wjpr_drawn_texts), NA = el
# adaptador no lo expone, "" = no hay nada impreso; val y val2 (numeric) = el valor que codifica la geometría dibujada,
# decodificado a las unidades de df$value y df$value2 (largo de barra, x del punto, alto de caja), NA = no lo codifica o
# no se expone. Un adaptador que no las devuelve las tiene NA (nada que comparar). figures (06) compara label_drawn y
# grouping_drawn con df (estado "texto") y val/val2 con df$value/value2 (estado "dibujo"); verify (07), con la referencia.
WJPR_DRAWN_COLS <- c("order", "kind", "grouping", "label", "frac", "frac2", "text", "text2")
WJPR_DRAWN_OPT  <- c("label_drawn", "grouping_drawn", "val", "val2")
# Las columnas del contrato en orden y con su tipo (las que falten: "" o NA en las fijas, NA en las opcionales); las
# demás columnas se conservan detrás. La usan wjpr_drawn() y save_figure() (06_export.R) para el drawn de svg_r
wjpr_drawn_complete <- function(d) {
  n <- nrow(d); get <- function(nm, na) if (nm %in% names(d)) d[[nm]] else rep(na, n)
  chr <- function(x) coalesce(as.character(x), "")
  num <- function(x) suppressWarnings(as.numeric(x))
  out <- tibble(order = as.integer(get("order", NA)), kind = chr(get("kind", "")), grouping = chr(get("grouping", "")),
                label = chr(get("label", "")), frac = num(get("frac", NA)), frac2 = num(get("frac2", NA)),
                text = chr(get("text", "")), text2 = chr(get("text2", "")),
                label_drawn = as.character(get("label_drawn", NA_character_)),
                grouping_drawn = as.character(get("grouping_drawn", NA_character_)),
                val = num(get("val", NA)), val2 = num(get("val2", NA)))
  bind_cols(out, as_tibble(d)[, setdiff(names(d), names(out)), drop = FALSE])
}
wjpr_drawn <- function(p, b = NULL) {
  fn <- attr(p, "wjpr")$drawn
  if (!is.function(fn)) return(NULL)
  if (is.null(b)) b <- suppressWarnings(ggplot2::ggplot_build(p))
  d <- fn(b)
  miss <- if (is.data.frame(d)) setdiff(WJPR_DRAWN_COLS, names(d)) else WJPR_DRAWN_COLS
  if (length(miss)) stop("attr(p, \"wjpr\")$drawn no devolvió las columnas ", paste(miss, collapse = ", "), call. = FALSE)
  wjpr_drawn_complete(d)[, c(WJPR_DRAWN_COLS, WJPR_DRAWN_OPT)]
}

# Posiciones de un eje discreto en un gráfico construido (b = ggplot_build): una fila por nivel de cada panel con PANEL,
# pos (entero de la escala del panel), level (su nivel) y slot (orden visual: en el eje y, paneles de arriba abajo y,
# dentro de cada uno, la y mayor arriba; en el x, de izquierda a derecha; coord con reverse lo invierte). Es la base de
# un drawn por fila: una barra o un texto de una capa construida está en el slot de su PANEL y de round(y) (o x).
wjpr_slots <- function(b, axis = c("y", "x")) {
  axis <- match.arg(axis)
  lay <- b$layout$layout
  scales <- if (axis == "y") b$layout$panel_scales_y else b$layout$panel_scales_x
  sc_col <- if (axis == "y") "SCALE_Y" else "SCALE_X"
  out <- bind_rows(lapply(seq_len(nrow(lay)), function(i) {
    s <- scales[[lay[[sc_col]][i]]]
    if (is.null(s) || !s$is_discrete()) stop("wjpr_slots: el eje ", axis, " del panel ", lay$PANEL[i], " no es discreto", call. = FALSE)
    lev <- as.character(s$get_limits())
    rv <- b$layout$panel_params[[i]]$reverse
    flip <- is.character(rv) && length(rv) == 1 && rv %in% c(axis, "xy")
    up <- (axis == "y") != flip                         # TRUE: la posición mayor va primero (arriba)
    tibble(PANEL = as.integer(as.character(lay$PANEL[i])), ROW = as.integer(lay$ROW[i]), COL = as.integer(lay$COL[i]),
           pos = seq_along(lev), level = lev, .k = if (up) -seq_along(lev) else seq_along(lev))
  }))
  out <- if (axis == "y") arrange(out, ROW, COL, .k) else arrange(out, COL, ROW, .k)
  mutate(select(out, -.k), slot = row_number())
}

# Slot (wjpr_slots) de cada fila de una capa construida d: su PANEL y round() de su coordenada en el eje discreto; NA
# si no es finita (barra de un valor suprimido) o no corresponde a ningún nivel
wjpr_slot_of <- function(d, slots, axis = "y") {
  if (!nrow(d) || is.null(d[[axis]]) || !"PANEL" %in% names(d)) return(rep(NA_integer_, nrow(d)))
  v <- suppressWarnings(round(as.numeric(d[[axis]])))
  i <- match(paste(as.integer(as.character(d$PANEL)), v), paste(slots$PANEL, slots$pos))
  ifelse(is.finite(v), slots$slot[i], NA_integer_)
}

# ¿Mismo color que ref (hex o nombre, alfa incluido)? NA → FALSE
wjpr_same_color <- function(x, ref) {
  x <- as.character(x); out <- rep(FALSE, length(x)); ok <- !is.na(x)
  if (any(ok)) out[ok] <- colSums(abs(grDevices::col2rgb(x[ok], alpha = TRUE) - as.vector(grDevices::col2rgb(ref, alpha = TRUE)))) == 0
  out
}

# Exporta como las figuras ggplot: svglite al tamaño del manifiesto (los tamaños de WJPr ya están en pt reales:
# 3.514598 mm = 10 pt) → texto en trazos → tamaño exacto → PNG a cfg$layout$dpi
wjpr_render <- function(p, spec, cfg, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  svg <- file.path(out_dir, paste0(figure_stem(spec), ".svg")); png <- sub("\\.svg$", ".png", svg)
  tmp <- tempfile(fileext = ".svg")
  if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_auto(FALSE)
  suppressWarnings(ggplot2::ggsave(tmp, p, device = svglite::svglite, width = spec$w_in, height = spec$h_in, units = "in"))
  lg <- tryCatch(outline_svg_text(tmp), error = function(e) e)
  if (inherits(lg, "error")) { unlink(tmp); stop("texto en trazos: ", conditionMessage(lg), call. = FALSE) }
  set_size(tmp, spec$w_in, spec$h_in)
  file.copy(tmp, svg, overwrite = TRUE); unlink(tmp)
  px <- tryCatch(chrome_png(svg, png, spec$w_in, spec$h_in, dpi = cfg$layout$dpi, chrome = cfg$paths$chrome),
                 error = function(e) NA_integer_)
  list(svg = svg, png = if (file.exists(png)) png else NA_character_, png_px = paste(px, collapse = "x"),
       doubtful = if (is.data.frame(lg)) as.integer(sum(lg$doubtful)) else NA_integer_,
       families = if (is.data.frame(lg) && nrow(lg)) paste(unique(lg$family), collapse = ", ") else "",
       n_text = count_live_text(svg))
}

# Parecido visual con la figura publicada: ambas a 96 dpi en el mismo lienzo; "tinta" = píxel con gris < 0.75.
# precision = tinta nueva a ≤ r px de tinta publicada; recall = tinta publicada a ≤ r px de tinta nueva; f1 su media
# armónica (1 = mismo dibujo). mad = diferencia media absoluta de gris. Mide disposición, no colores exactos.
# corregido (V3-01): el f1 es global y binario, y no ve un error localizado: una barra 10 pp más larga cambia menos del
# 0.5 % de la tinta, y la pista, la banda < 25 % y los colores claros no son tinta. Además del f1 devuelve local (la
# peor ventana de block × block px, wjpr_local_diff: 0 = igual, 1 = ventana entera distinta), local_x_in y
# local_y_in (centro de esa ventana, en pulgadas desde arriba a la izquierda) y local_px (píxeles distintos).
# local = FALSE: solo el f1. Los umbrales los fija verify (config/<pais>.yml: min_f1, max_local, local_*).
wjpr_similarity <- function(png_new, png_pub, r = 2L, dpi_new = NA, local = TRUE, block = 12L, delta = 0.2,
                            delta_flat = 0.05, shift = 1L) {
  a <- read_png_rgb(png_pub); b <- read_png_rgb(png_new)
  bi <- resample_to_dpi(b$img, if (is.na(dpi_new)) (if (is.na(b$dpi)) 300 else b$dpi) else dpi_new)
  h <- min(dim(a$img)[1], dim(bi)[1]); w <- min(dim(a$img)[2], dim(bi)[2])
  gray <- function(img) 0.299 * img[seq_len(h), seq_len(w), 1] + 0.587 * img[seq_len(h), seq_len(w), 2] + 0.114 * img[seq_len(h), seq_len(w), 3]
  ga <- gray(a$img); gb <- gray(bi)
  ia <- ga < 0.75; ib <- gb < 0.75
  dilate <- function(m) {
    out <- m
    for (dy in -r:r) for (dx in -r:r) {
      ys <- seq_len(h) + dy; xs <- seq_len(w) + dx
      okY <- ys >= 1 & ys <= h; okX <- xs >= 1 & xs <= w
      out[okY, okX] <- out[okY, okX] | m[ys[okY], xs[okX]]
    }
    out
  }
  prec <- if (sum(ib)) sum(ib & dilate(ia)) / sum(ib) else NA_real_
  rec <- if (sum(ia)) sum(ia & dilate(ib)) / sum(ia) else NA_real_
  out <- list(precision = prec, recall = rec, f1 = if (is.na(prec) || is.na(rec) || prec + rec == 0) NA_real_ else 2 * prec * rec / (prec + rec),
              mad = mean(abs(ga - gb)))
  if (!isTRUE(local)) return(out)
  c(out, wjpr_local_diff(a$img[seq_len(h), seq_len(w), 1:3, drop = FALSE], bi[seq_len(h), seq_len(w), 1:3, drop = FALSE],
                         block = block, delta = delta, delta_flat = delta_flat, shift = shift))
}

# Diferencia LOCAL entre dos imágenes RGB del mismo tamaño (alto × ancho × 3, en 0-1; la publicada y la nueva a 96 dpi).
# Un píxel es distinto si, en algún canal, su valor cae a más de delta fuera del rango [mín, máx] de ese canal en la
# vecindad de (2·shift + 1)² px de la otra imagen, en los dos sentidos (tolera el antialias y desplazamientos de shift
# px; por canal ve también un cambio de tono con el mismo gris), o si las dos imágenes son planas en esa vecindad (rango
# < 0.03 por canal) y difieren allí en más de delta_flat (colores claros que delta no separa: pista #E5E8E8 frente a
# blanco = 0.10, UE #A8A8C8 frente a la pista = 0.24, tonos de relleno). local = máximo, sobre todas las ventanas de
# block × block px, de la fracción de píxeles distintos. Calibración (23-oct): config/hungary.yml, verify$max_local.
wjpr_local_diff <- function(A, B, block = 12L, delta = 0.2, delta_flat = 0.05, shift = 1L, dpi = COMPARE_DPI) {
  h <- dim(A)[1]; w <- dim(A)[2]
  if (!identical(dim(A), dim(B)) || length(dim(A)) != 3 || dim(A)[3] != 3) stop("wjpr_local_diff: A y B deben ser alto × ancho × 3 del mismo tamaño", call. = FALSE)
  rs <- function(M, d) M[pmin(pmax(seq_len(h) + d, 1L), h), , drop = FALSE]
  cs <- function(M, d) M[, pmin(pmax(seq_len(w) + d, 1L), w), drop = FALSE]
  box <- function(M) {                           # mín y máx en la vecindad (separable: filas, luego columnas)
    hi <- M; lo <- M
    for (d in setdiff(-shift:shift, 0L)) { S <- rs(M, d); hi <- pmax(hi, S); lo <- pmin(lo, S) }
    hi2 <- hi; lo2 <- lo
    for (d in setdiff(-shift:shift, 0L)) { hi2 <- pmax(hi2, cs(hi, d)); lo2 <- pmin(lo2, cs(lo, d)) }
    list(hi = hi2, lo = lo2)
  }
  out_a <- matrix(0, h, w); out_b <- out_a; flat_a <- matrix(TRUE, h, w); flat_b <- flat_a; raw <- out_a
  for (k in 1:3) {
    a <- matrix(A[, , k], h, w); b <- matrix(B[, , k], h, w); ba <- box(a); bb <- box(b)
    out_a <- pmax(out_a, bb$lo - a, a - bb$hi)   # A fuera del rango de B en su vecindad
    out_b <- pmax(out_b, ba$lo - b, b - ba$hi)   # y al revés
    flat_a <- flat_a & (ba$hi - ba$lo) < 0.03; flat_b <- flat_b & (bb$hi - bb$lo) < 0.03
    raw <- pmax(raw, abs(a - b))
  }
  bad <- pmax(out_a, out_b) > delta | (flat_a & flat_b & raw > delta_flat)
  k <- as.integer(min(block, h, w))
  S <- matrix(0, h + 1, w + 1); S[-1, -1] <- t(apply(apply(bad * 1, 2, cumsum), 1, cumsum))   # imagen integral
  i <- seq_len(h - k + 1); j <- seq_len(w - k + 1)
  s <- S[i + k, j + k, drop = FALSE] - S[i, j + k, drop = FALSE] - S[i + k, j, drop = FALSE] + S[i, j, drop = FALSE]
  m <- which.max(s) - 1L
  list(local = max(s) / k^2, local_x_in = (m %/% length(i) + k / 2) / dpi, local_y_in = (m %% length(i) + k / 2) / dpi,
       local_px = as.integer(sum(bad)))
}

# df del contrato desde la referencia publicada, para dibujar figuras cuyo data() aún no existe. En los tipos con
# barra relativa a 100 % (agrupadas, dumbbell, hbars, pareadas) value/value2 = frac·100 (la barra publicada exacta);
# en columns frac = value/vmax y frac·100 da las mismas alturas relativas; en el resto, value/value2 tal cual.
# Extras (x, y, n, lo, hi) se conservan.
wjpr_ref_df <- function(spec, cfg) {
  f <- file.path(cfg$paths$reference, paste0(figure_stem(spec), ".csv"))
  if (!file.exists(f)) stop("sin referencia: ", basename(f), call. = FALSE)
  d <- read_csv(f, col_types = cols(.default = col_character()), na = character())
  num <- intersect(c("order", "value", "value2", "frac", "frac2", "x", "y", "n", "lo", "hi"), names(d))
  d <- d %>% mutate(across(all_of(num), ~ suppressWarnings(as.numeric(.x))))
  if (spec$tipo %in% c("grouped_bars", "dumbbell", "hbars", "paired_bars", "columns")) {   # columns: frac = value/vmax
    scale <- if (spec$tipo == "grouped_bars" && !any(grepl("%", d$text))) 10 else 100      # fig 4: media 0-10
    d <- d %>% mutate(value = coalesce(frac * scale, value), value2 = coalesce(frac2 * scale, value2))
  }
  # corregido (revisión 4): la referencia del scatter (3) no trae series y wjpr_scatter la usa para las destacadas
  # (rotuladas): sin ella la galería sin caché dibujaba la 3 sin rótulos. Se pone como en data(): "destacada" si la
  # categoría está en figures: figNN: highlight de config/<pais>.yml (las de lo publicado), "" si no
  if (identical(spec$tipo, "scatter") && !"series" %in% names(d)) {
    hl <- as.character(unlist(cfg$figures[[sprintf("fig%02d", as.integer(spec$fig))]]$highlight))
    d$series <- if_else(d$label %in% hl, "destacada", "")
  }
  # corregido (revisión 4): tampoco trae text2 = WJPR_SCATTER_HUECO (punto dibujable con n < 30, la marca que pone
  # data() y que compara lo dibujado por punto); se pone como en data() desde la columna n de la referencia
  if (identical(spec$tipo, "scatter") && "n" %in% names(d) && !any(d$text2 == WJPR_SCATTER_HUECO)) {
    d$text2 <- if_else(is.finite(d$value) & is.finite(d$value2) & coalesce(d$n < 30, FALSE), WJPR_SCATTER_HUECO, d$text2)
  }
  d %>% select(-any_of(c("frac", "frac2", "tol", "tol2", "tol_xy"))) %>% mutate(order = as.integer(order))
}

## Prueba de la verificación del dibujo (revisión 3, 2026-10-23, F1: V3-01, V3-02): SCRATCH/fix3/F1/pruebas/
## p10_calibra_final.R con wjpr_similarity() y los parámetros de config/hungary.yml sobre las 42 de tools/wjpr_gallery.R
## (1.1.0@382aee9 y main, mismos valores) y las perturbaciones de la revisión V3 (rev3_V3/perturb): local 0.000-0.444
## en 41 correctas (0.444 la 06, claves de leyenda de 15.2 pt; 0.389 la 29, título del eje y desplazado ~2 px y gris
## #E5E8E8 en lugar de #D9D9D9 en la matriz; 0.243 la 16; 0.153 la 03, rótulos de ggrepel; 0.125 la 19; 0.118 la 07) y
## 0.715 en la 42 (tarjeta 42 % con texto negro; publicada, blanco: diferencia real del bloque cards); errores: barra
## +10 pp en la 1 → 1.000 (f1 0.9954), Public Services −5 pp en la 2 → 0.833 (0.9951), Male +1.0 en la 4 → 0.924
## (0.9957), colores de banda invertidos en la 6 → 1.000 (0.9985), UE severidad 5 −1 pp en la 7 → 1.000 (0.9973),
## lift máximo × 2 en la 5 → 1.000 (0.9995); +1 pp en la 1 → 0.333 y +2 pp → 0.583 (las barras agrupadas los ven por
## fila); textos de dos filas intercambiados → 0.028-0.194 (los ve solo lo dibujado por fila). Tiempo: 0.05-0.17 s por
## figura (la tolerancia de 1 px como mínimo/máximo separable 3 × 3; la versión con 9 desplazamientos tardaba 0.2-0.5 s
## y daba lo mismo salvo en bordes de texto: 03 0.201 → 0.153, 29 0.424 → 0.389). p11_unit.R: idénticas → 0, barra 12 px
## más larga → 0.917, todo desplazado 1 px → 0, pista #E5E8E8 → blanco (Δ 0.10) → 1, tono de la barra → 0.917;
## wjpr_meta(drawn = "x") y un drawn sin columnas → error legible; sin drawn → NULL.

## Lo dibujado por fila con dos series por posición (revisión 4, G1) ------------------------------------------------
## Auxiliares de los bloques dumbbell, paired_bars y severity_bars (un solo panel con un eje discreto); escritos en
## el bloque dumbbell y movidos aquí porque los usan tres bloques.
# wjpr_pos_slots(b, axis): una fila por nivel del eje discreto con level, pos = su posición MAPEADA (sc$map, la paleta
# de la escala: el dumbbell pone cada línea en pt con scale_x_discrete(palette = ), no en 1..n como supone
# wjpr_slots()) y slot = orden visual: con coord_flip el x de datos va de abajo arriba (la posición mayor arriba = 1),
# sin flip el x de izquierda a derecha y el y de arriba abajo
wjpr_pos_slots <- function(b, axis = c("x", "y")) {
  axis <- match.arg(axis)
  if (nrow(b$layout$layout) != 1) stop("wjpr_pos_slots: se esperaba un solo panel (", nrow(b$layout$layout), ")", call. = FALSE)
  sc <- (if (axis == "y") b$layout$panel_scales_y else b$layout$panel_scales_x)[[1]]
  if (is.null(sc) || !sc$is_discrete()) stop("wjpr_pos_slots: el eje ", axis, " no es discreto", call. = FALSE)
  lev <- as.character(sc$get_limits())
  pos <- suppressWarnings(as.numeric(sc$map(lev)))
  up <- (axis == "y") != inherits(b$layout$coord, "CoordFlip")      # TRUE: la posición mayor va primero
  tibble(level = lev, pos = pos, slot = as.integer(rank(if (up) -pos else pos, ties.method = "first")))
}
# Slot de cada coordenada v del eje discreto: el del nivel más cercano, a menos de media separación entre niveles
# (una barra esquivada o un texto de la línea caen en su nivel); NA si no es finita o no cae en ninguno
wjpr_pos_slot_of <- function(v, slots) {
  v <- suppressWarnings(as.numeric(v))
  half <- if (nrow(slots) > 1) min(diff(sort(slots$pos))) / 2 else 0.5
  vapply(v, function(x) {
    if (!is.finite(x)) return(NA_integer_)
    d <- abs(slots$pos - x); j <- which.min(d)
    if (d[j] < half) slots$slot[j] else NA_integer_
  }, NA_integer_)
}
# Lo impreso en la leyenda: una fila por clave con aes ("fill" | "colour"), value (el color de la clave) y label (su
# texto sin markup). complemento: wjpr_drawn_texts() no lee la leyenda (WJPr::wjp_drawn tampoco, en preparación): se
# lee de las guías que ggplot_build() ya entrenó (b$plot$guides: key con .label y el color de cada clave). Vacía si no
# se dibuja: legend.position "none", legend.text en blanco, de tamaño 0 o del color del fondo, o sin guía de leyenda
# (una capa con show.legend = FALSE no la crea)
wjpr_legend_drawn <- function(b) {
  empty <- tibble(aes = character(), value = character(), label = character())
  g <- b$plot$guides
  if (!inherits(g, "Guides") || !length(g$params)) return(empty)
  th <- ggplot2::complete_theme(b$plot$theme)
  pos <- tryCatch(ggplot2::calc_element("legend.position", th), error = function(e) NULL) %||% "right"
  if (identical(as.character(pos)[1], "none")) return(empty)
  el <- tryCatch(ggplot2::calc_element("legend.text", th), error = function(e) NULL)
  if (is.null(el) || inherits(el, "element_blank") || !isTRUE(all((el$size %||% 1) > 0))) return(empty)
  bg <- wjpr_theme_fill(th, "legend.background", wjpr_theme_fill(th, "plot.background"))
  tc <- wjpr_rgba(el$colour %||% "black")[, 1]
  if (max(abs(tc[1:3] - bg)) * tc[4] <= WJPR_VISIBLE_MIN) return(empty)
  md <- inherits(el, c("element_markdown", "element_textbox"))
  out <- lapply(seq_along(g$params), function(k) {
    key <- g$params[[k]]$key
    if (!inherits(g$guides[[k]], "GuideLegend") || !is.data.frame(key) || !nrow(key) || !".label" %in% names(key)) return(NULL)
    lab <- wjpr_plain_text(vapply(key$.label, function(v) paste(as.character(v), collapse = " "), ""), markdown = md)
    bind_rows(lapply(intersect(c("fill", "colour"), names(key)), function(a)
      tibble(aes = a, value = as.character(key[[a]]), label = lab)))
  })
  out <- bind_rows(empty, out)
  out[!is.na(out$label) & nzchar(out$label), , drop = FALSE]
}
# Serie de cada color col: la que la leyenda impresa (wjpr_legend_drawn, guía del aes) da a ese color; si no hay
# leyenda de ese aes, la del color que el adaptador da a cada serie (series ↔ cols; NULL: NA). NA si ninguna
wjpr_series_of <- function(col, legend, aes, series = NULL, cols = NULL) {
  lg <- legend[legend$aes == aes, , drop = FALSE]
  ref <- if (nrow(lg)) lg$value else cols; nm <- if (nrow(lg)) lg$label else series
  vapply(as.character(col), function(cl) {
    if (is.na(cl) || !length(ref)) return(NA_character_)
    k <- which(wjpr_same_color(ref, cl)); if (length(k)) nm[k[1]] else NA_character_
  }, NA_character_, USE.NAMES = FALSE)
}


## Bloques por tipo ---------------------------------------------------------------------------------------------------
## (cada bloque: encabezado "## Bloque wjpr <tipo> — figuras …", la función wjpr_<tipo>(df, spec, ...) y sus
## auxiliares; se añaden debajo)

## Bloque wjpr grouped_bars — figuras 1, 4, 9, 12, 17, 20-28, 31, 33, 35, 38-40 --------------------------------------
## Base: WJPr::wjp_groupbars() con argumentos documentados: colors (principal y pista; en la 4 la pista transparente),
## labels (= df$text, impreso tal cual), group_order, level_order (las filas en el orden del df, en el sentido en que lo
## lee la build: main de arriba abajo, 1.1.0 de abajo arriba; wjpr_gb_top_down), show_national + national_style "bar" +
## national_position "top" + national_label + national_value (fila National Average; la 27 no la lleva),
## national_group_label, label_position "end", bar_width (barra / paso de fila del diseño), show_axis FALSE,
## strip_position "left" y ptheme = wjpr_theme(). Fuente: en main la nativa (options(wjpr.family = ), que fija
## wjpr_require()); en 1.1.0, el complemento wjpr_font(). Se conservan sus capas (geom_col apilado = barra + pista
## 0-100 %, geom_text de los valores), su faceta (paneles de alto libre) y su barra nacional; los complementos van
## marcados "# complemento:". Medidas del diseño publicado: STD (04_svg.R; la 27, ST_A01 con la pista que llena los
## 6.5 in, como chart_grouped(fill_w = spec$w_in * 72)) y la columna de etiquetas LABEL_COL[["std"]]. Variantes por
## figura (spec$fig): 4 = media 0-10 sin pista y con el valor tras la barra (chart_grouped(track = FALSE, value_at_end =
## TRUE)); 27 = ST_A01 sin filas de promedio. complementos = FALSE devuelve la llamada a WJPr sola (la línea de base con
## la que se mide el efecto de los complementos). Unidades: las medidas del diseño son puntos de 1/72 in (los de svglite
## y de 04_svg.R) = "bigpts" de grid; el "pt" de grid/ggplot2::margin() es 1/72.27 in (0.4 % menor).
## Builds probadas: 1.1.0@382aee9 (13-jul) y main a46c831. Cada "propuesta:" de un complemento es el argumento de
## wjp_groupbars que lo sustituiría: cuando una build lo traiga, entra en la llamada con wjpr_has_arg("wjp_groupbars",
## "<arg>") y el complemento queda en la otra rama (no se escriben ramas sin una build que las pruebe).

WJPR_GB_NAT <- " "   # faceta de los promedios = national_group_label por defecto de WJPr (strip en blanco)
WJPR_GB_HDR <- "\u00b6 "   # prefijo de la faceta de encabezado de cada grupo (no se ve: los strips van ocultos)

# Escapa lo que element_markdown leería como Markdown o HTML en una etiqueta ("Money & Debt", "1. …", "*")
wjpr_gb_md_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("([*_`\\\\\\[\\]#])", "\\\\\\1", x, perl = TRUE)
  sub("^([0-9]+)([.)])", "\\1\\\\\\2", x)
}

# Copia de las capas de una clase de geom con aes_params cambiados (sin tocar el objeto de WJPr)
wjpr_gb_set_layer <- function(p, geom, ...) {
  vals <- list(...)
  p$layers <- lapply(p$layers, function(l) {
    if (!inherits(l$geom, geom)) return(l)
    l2 <- ggplot2::ggproto(NULL, l)
    for (k in names(vals)) l2$aes_params[[k]] <- vals[[k]]
    l2
  })
  p
}

# Ancho (pt de 1/72 in) de lo que el gtable pone a la izquierda del primer panel (margen, etiquetas, strips), medido en
# un dispositivo svglite como el de la exportación (gridtext mide el Markdown con las métricas del dispositivo); deja
# activo el dispositivo que lo estaba
wjpr_gb_left_of_panel <- function(p, w_in, h_in) {
  tf <- tempfile(fileext = ".svg"); old <- grDevices::dev.cur()
  svglite::svglite(tf, width = w_in, height = h_in); dev <- grDevices::dev.cur()
  on.exit({ grDevices::dev.off(dev); if (old > 1) grDevices::dev.set(old); unlink(tf) }, add = TRUE)
  gt <- suppressWarnings(ggplot2::ggplotGrob(p))
  l <- min(gt$layout$l[grepl("^panel", gt$layout$name)])
  if (l <= 1) return(0)
  sum(grid::convertWidth(gt$widths[seq_len(l - 1)], "bigpts", valueOnly = TRUE))
}

# Medidas del diseño publicado (pt): st, columna de etiquetas L, inicio de la barra x0, valor de la barra llena y
# variante media 0-10 (la 4, o textos sin "%": escala 10, sin pista y con el valor tras la barra)
wjpr_gb_design <- function(df, spec) {
  fig <- as.integer(spec$fig)
  st <- if (identical(fig, 27L)) ST_A01 else STD
  W <- spec$w_in * 72; L <- LABEL_COL[["std"]]
  if (identical(fig, 27L)) st$track <- W - L - st$gap_label - st$gap_value - st$value_w   # la pista llena la figura
  media <- identical(fig, 4L) || !any(grepl("%", df$text, fixed = TRUE))
  list(st = st, W = W, L = L, x0 = L + st$gap_label, scale = if (media) 10 else 100, track = !media, value_at_end = media)
}

# Sentido en que la build cargada lee level_order: main (f4f3b8f) lo documenta y lo aplica de arriba abajo; 1.1.0 lo
# pasa tal cual a ggplot2, que pone el primer nivel abajo (su ayuda lo advierte). Se mira lo que hace con dos filas y
# no el número de versión (las dos builds dicen 1.1.0); no es un complemento: es el argumento documentado de cada una
wjpr_gb_top_down <- function() {
  p <- WJPr::wjp_groupbars(data.frame(g = "a", l = c("x", "y"), v = c(10, 20)), target = "v", grouping = "g",
                           levels = "l", level_order = list(a = c("x", "y")), ptheme = ggplot2::theme())
  lim <- ggplot2::layer_scales(p)$y$get_limits()        # niveles del eje y de abajo arriba
  grepl("y$", lim[1])                                   # abajo el último dado → los lee de arriba abajo
}

# Entrada de wjp_groupbars: una fila por categoría (grp, lvl, v = barra 0-100, lab = df$text, kind). Con complementos,
# la fila EU Average en la faceta de los promedios y, por grupo, una faceta de encabezado (una fila sin barra) antes
# de la de sus categorías. Devuelve d, el orden de facetas (group_order), level_order (las filas en el orden del df,
# de arriba abajo, en el sentido de la build: top_down), el tipo de cada faceta y keys: la fila del df (order, kind,
# grouping, label) de cada nivel del eje y de WJPr (y_id = "<faceta> | <categoría>", el que lab_fun rotula), para
# lo dibujado por fila (wjpr_gb_drawn).
wjpr_gb_input <- function(df, g, complementos = TRUE, top_down = wjpr_gb_top_down()) {
  lo <- function(x) if (top_down) x else rev(x)
  avg <- df[df$kind == "avg", ]; body <- df[df$kind %in% c("header", "row"), ]
  # complemento: WJPr solo acepta proporciones (0-1) o porcentajes (0-100) y leería la media 0-10 de la 4 como %
  # (propuesta: scale_max = 10): la barra entra como value / scale · 100 y el texto, de df$text
  pct <- function(v) as.numeric(v) / g$scale * 100
  rows <- body[body$kind == "row", ]
  groups <- unique(as.character(body$grouping))
  row_d <- function(r, grp) tibble(grp = grp, lvl = r$label, v = pct(r$value), lab = coalesce(r$text, ""), kind = r$kind)
  key_d <- function(r, grp) tibble(y_id = paste(grp, r$label, sep = " | "), order = as.integer(r$order), kind = r$kind,
                                   grouping = r$grouping, label = r$label)
  nat_key <- if (nrow(avg)) key_d(avg[1, ], WJPR_GB_NAT) else key_d(avg, character())   # barra nacional de WJPr
  if (!complementos) {
    d <- row_d(rows, as.character(rows$grouping))
    lev <- setNames(lapply(groups, function(gr) lo(d$lvl[d$grp == gr])), groups)
    return(list(d = d, order = groups, lev = lev, panels = rep("rows", length(groups)), avg = avg,
                keys = bind_rows(nat_key, key_d(rows, as.character(rows$grouping)))))
  }
  extra <- if (nrow(avg)) avg[-1, ] else avg
  d <- row_d(extra, rep(WJPR_GB_NAT, nrow(extra))) %>% mutate(kind = "avg")
  keys <- bind_rows(nat_key, key_d(extra, rep(WJPR_GB_NAT, nrow(extra))))
  order <- character(); panels <- character(); lev <- list()
  if (nrow(avg)) { order <- WJPR_GB_NAT; panels <- "avg"; lev[[WJPR_GB_NAT]] <- lo(avg$label) }
  for (gr in groups) {
    b <- body[as.character(body$grouping) == gr, ]
    h <- b[b$kind == "header", ]; r <- b[b$kind == "row", ]
    if (nrow(h)) {
      key <- paste0(WJPR_GB_HDR, gr)
      d <- bind_rows(d, row_d(h, rep(key, nrow(h))) %>% mutate(lab = ""))
      keys <- bind_rows(keys, key_d(h, rep(key, nrow(h))))
      order <- c(order, key); panels <- c(panels, "header"); lev[[key]] <- lo(h$label)
    }
    if (nrow(r)) {
      d <- bind_rows(d, row_d(r, rep(gr, nrow(r))))
      keys <- bind_rows(keys, key_d(r, rep(gr, nrow(r))))
      order <- c(order, gr); panels <- c(panels, "rows"); lev[[gr]] <- lo(r$label)
    }
  }
  list(d = d, order = order, lev = lev, panels = panels, avg = avg, keys = keys)
}

# Lo dibujado por fila (contrato drawn de wjpr_meta; V3-01, V3-02 y contrato extendido de la revisión 4), leído de b =
# ggplot_build() del gráfico final: en cada posición del eje y de cada panel (wjpr_slots, de arriba abajo = order), la
# fila del df cuyo nivel y_id ocupa esa posición (sin equivalente: kind "" y el nivel como label, una fila sobrante),
# frac = largo de las barras del color principal en esa posición / 100 (la pista es 0-100 en la escala de WJPr; la 4
# entra como value / scale · 100, así que frac = value / scale, el frac de la referencia), val = frac · scale (el largo
# en las unidades de df$value: % o, en la 4, media 0-10) y text = lo que la capa de texto imprime y se ve en esa
# posición (wjpr_drawn_texts: un valor con alfa 0 o blanco sobre blanco no cuenta). label_drawn = la etiqueta del eje
# impresa en esa posición (la que lab_fun rotula; "" si ninguna) y grouping_drawn = el encabezado impreso bajo el que
# queda la fila: el strip visible de su panel si lo hay (WJPr sin complementos) y si no la etiqueta impresa de la
# última fila de encabezado por encima (la propia en un encabezado; "" en los promedios, que van antes del primero).
# Una barra o un texto en la fila equivocada, una barra más larga, un orden distinto, dos etiquetas intercambiadas, un
# encabezado cambiado o un valor oculto se ven aquí aunque el multiconjunto de textos y el parecido global sean iguales.
wjpr_gb_drawn <- function(b, keys, bar_fill, scale = 100) {
  slots <- wjpr_slots(b, "y")
  layer_data <- function(geoms) b$data[which(vapply(b$plot$layers, function(l) inherits(l$geom, geoms), logical(1)))]
  has <- function(d, ...) all(c(...) %in% names(d))
  bars <- bind_rows(lapply(layer_data(c("GeomCol", "GeomBar", "GeomRect")), function(d)
    tibble(slot = wjpr_slot_of(d, slots), main = if (has(d, "fill")) wjpr_same_color(d$fill, bar_fill) else FALSE,
           len = if (has(d, "xmin", "xmax")) as.numeric(d$xmax) - as.numeric(d$xmin) else NA_real_)))
  bars <- bars[!is.na(bars$slot) & bars$main & is.finite(bars$len), , drop = FALSE]
  tx <- wjpr_drawn_texts(b)                            # solo lo que se ve
  at <- function(src) { t <- tx[tx$source == src, , drop = FALSE]; t$slot <- wjpr_slot_of(t, slots); t[!is.na(t$slot), , drop = FALSE] }
  txt <- at("layer"); txt <- txt[order(txt$slot, txt$x), , drop = FALSE]
  ax <- at("axis-y")
  strips <- tx[tx$source == "strip", , drop = FALSE]
  k <- match(slots$level, keys$y_id)
  kind <- coalesce(keys$kind[k], "")
  lab_drawn <- vapply(slots$slot, function(s) paste(ax$label[ax$slot == s], collapse = " "), "")
  strip_of <- strips$label[match(as.character(slots$PANEL), strips$PANEL)]
  hdr <- ""; grp_drawn <- character(nrow(slots))
  for (i in seq_len(nrow(slots))) {                     # slots ya va en orden visual (de arriba abajo)
    if (kind[i] == "header") hdr <- lab_drawn[i]
    grp_drawn[i] <- if (!is.na(strip_of[i]) && nzchar(strip_of[i])) strip_of[i] else hdr
  }
  frac <- vapply(slots$slot, function(s) { l <- bars$len[bars$slot == s]; if (length(l)) sum(l) / 100 else NA_real_ }, 0)
  tibble(order = slots$slot, kind = kind, grouping = coalesce(keys$grouping[k], ""),
         label = coalesce(keys$label[k], slots$level), frac = frac, frac2 = NA_real_,
         text = vapply(slots$slot, function(s) paste(txt$label[txt$slot == s], collapse = " "), ""), text2 = "",
         label_drawn = lab_drawn, grouping_drawn = grp_drawn, val = frac * scale, val2 = NA_real_)
}
wjpr_gb_drawn_fun <- function(keys, bar_fill, scale = 100) {
  force(keys); force(bar_fill); force(scale)
  function(b) wjpr_gb_drawn(b, keys, bar_fill, scale)
}

wjpr_grouped_bars <- function(df, spec, complementos = wjpr_complementos(), ...) {
  wjpr_require()
  df <- df[order(df$order), ]
  g <- wjpr_gb_design(df, spec); st <- g$st
  inp <- wjpr_gb_input(df, g, complementos); d <- inp$d; avg <- inp$avg
  has_nat <- nrow(avg) > 0
  # corregido (revisión 4): con el promedio nacional suprimido (NA) WJPr se detenía con «national_value must be a
  # single proportion», sin decir qué figura ni por qué; ahora el error es legible y sale antes de llamar a WJPr
  if (has_nat && !is.finite(suppressWarnings(as.numeric(avg$value[1])))) {
    stop(sprintf("fig %02d: sin promedio nacional (%s con value NA: suprimido por n < 30 o sin datos; wjp_groupbars necesita national_value)",
                 as.integer(spec$fig), avg$label[1]), call. = FALSE)
  }
  nat_v <- if (has_nat) as.numeric(avg$value[1]) / g$scale * 100 else NULL
  p <- WJPr::wjp_groupbars(
    data = d, target = "v", grouping = "grp", levels = "lvl",
    colors = c(st$col$main, if (g$track) st$col$track else "transparent"),   # 4: sin pista (complemento transparente)
    labels = "lab", group_order = inp$order[inp$order != WJPR_GB_NAT], level_order = inp$lev,
    show_national = has_nat, national_value = nat_v, national_style = "bar", national_position = "top",
    national_label = if (has_nat) avg$label[1] else NULL, national_group_label = WJPR_GB_NAT,
    label_position = "end", bar_width = st$bar / st$pitch, show_axis = FALSE, strip_position = "left",
    ptheme = wjpr_theme())
  # lo dibujado por fila (V3-02; revisión 4: label_drawn, grouping_drawn y val en unidades de df$value), también sin
  # complementos
  drawn <- wjpr_gb_drawn_fun(inp$keys, st$col$main, g$scale)
  if (!complementos) return(wjpr_meta(p, base = "WJPr::wjp_groupbars", complementos = character(), drawn = drawn))

  # complemento (solo 1.1.0): WJPr fija family = "Lato Full" en capas y tema (wjpr_font); main la toma de
  # options(wjpr.family = ), que fija wjpr_require(), y el complemento sobra
  native_font <- wjpr_native_font()
  if (!native_font) p <- wjpr_font(p)
  comp <- c(if (!native_font) "fuente Inter Tight en capas y tema (wjpr_font)",
            if (g$scale != 100) "media 0-10 a barra 0-100")
  # complemento: WJPr no admite una segunda fila de promedio (national_value es un escalar; propuesta: national_value
  # con nombres, p. ej. c("National Average" = 61, "EU Average" = 61)): la fila UE entra como dato en la faceta del
  # promedio nacional (national_group_label), debajo de él (wjpr_gb_input).
  if (sum(d$kind == "avg")) comp <- c(comp, "fila EU Average")
  # complemento: WJPr solo dibuja los grupos como strips de faceta, a la izquierda de las etiquetas y sin alinear con
  # ellas (propuesta: group_header = "row"): cada encabezado es una faceta de una fila sin barra, en la columna de
  # etiquetas y alineado a la derecha como las categorías (wjpr_gb_input), y los strips se ocultan (tema, abajo).
  comp <- c(comp, "encabezados en la columna de etiquetas")

  # complemento: WJPr pone cada valor tras su barra (value + 1 unidades) y solo los alinea en columna con intervalos
  # de confianza (label_after_ci, a 101); falta una columna fija sin intervalos y la separación en pt (propuesta:
  # label_position = "column", label_gap): la columna publicada empieza gap_value pt tras la pista (la 4: tras la barra).
  # complemento: WJPr no dibuja las celdas suprimidas (su etiqueta queda en x = NA aunque labels traiga "N/A";
  # propuesta: na_label = "N/A"): con la columna fija la etiqueta de df$text tiene posición.
  # complemento: WJPr imprime la barra nacional como round(national_value) + "%" sin poder cambiarlo (propuesta:
  # national_value_label): se toma de df$text (la 4 imprime "4.9" sobre una barra de 49 en la escala 0-100 de WJPr).
  gap <- st$gap_value / st$track * 100
  nat_id <- paste(WJPR_GB_NAT, if (has_nat) avg$label[1] else "", sep = " | ")
  nat_txt <- if (has_nat) as.character(avg$text[1]) else NA_character_
  p$data <- p$data %>% mutate(
    label_value = if_else(color_type == "primary" & as.character(y_id) == nat_id & has_nat, nat_txt, label_value),
    label_x = if_else(color_type == "primary" & !is.na(label_value) & nzchar(label_value),
                      if (g$value_at_end) if_else(is.na(value), 100 + gap, value + gap) else 100 + gap, NA_real_))
  comp <- c(comp, if (g$value_at_end) "valor tras la barra a gap_value pt" else "columna de valores fija",
            if (any(d$lab == "N/A")) "N/A en celdas suprimidas",
            if (has_nat && !identical(nat_txt, paste0(round(nat_v), "%"))) "texto del valor nacional = df$text")

  # complemento: WJPr fija los valores en 5 mm = 14.2 pt negrita (propuesta: label_size, label_fontface): 9 pt semibold
  # cursiva (bold.italic de "inter" = SemiBoldItalic, como registra setup_fonts)
  p <- wjpr_gb_set_layer(p, "GeomText", size = st$size_value / ggplot2::.pt, fontface = "bold.italic")
  comp <- c(comp, "valores 9 pt semibold cursiva")

  # complemento: WJPr fija el eje en 0-115 (0-100 con eje) y el ancho de la pista sale de lo que sobra (propuesta:
  # xlim o value_col_width): la pista publicada (st$track pt) va de x0 al valor 100 y el panel acaba en el borde de la
  # figura, tras la columna de valores.
  xmax <- (g$W - g$x0) / st$track * 100
  # complemento: WJPr fija las etiquetas en 13 pt y solo destaca la barra nacional, en negrita (propuesta: label_size,
  # highlight_levels con su estilo): categorías 10 pt regular #1A1A1A; promedios y encabezados semibold cursiva del
  # color principal; una fila = st$pitch (expansión de media fila, WJPr: 0.6)
  kinds <- setNames(c(if (has_nat) "avg", d$kind), c(if (has_nat) nat_id, paste(d$grp, d$lvl, sep = " | ")))
  lab_fun <- function(x) {
    txt <- wjpr_gb_md_escape(sub("^.* \\| ", "", x))
    ifelse(kinds[x] %in% c("avg", "header"), sprintf("<b><i><span style='color:%s'>%s</span></i></b>", st$col$main, txt), txt)
  }
  p <- suppressMessages(p +
    ggplot2::scale_x_continuous(limits = c(0, xmax), expand = c(0, 0), oob = scales::oob_keep, position = "top") +
    ggplot2::scale_y_discrete(labels = lab_fun, expand = ggplot2::expansion(add = 0.5)))
  comp <- c(comp, "pista 0-100 % del ancho publicado", "etiquetas 10 pt, promedios y encabezados semibold cursiva")

  # complemento: WJPr fija 8 mm entre grupos y el margen 10/35/10/10 pt después de ptheme (ptheme no los puede
  # cambiar), y la columna de etiquetas mide la más larga (propuesta: row_height, group_spacing, label_width y aplicar
  # ptheme al final). Una fila = st$pitch; el bloque de encabezado (st$header) = s1 + fila de encabezado + s2, con s1
  # tal que la línea base del encabezado cae en st$header_base (panel.spacing es un vector: s1 antes de cada
  # encabezado, s2 después); la columna de etiquetas acaba en L y la barra empieza en x0: margen izquierdo = x0 − lo
  # que el gtable pone a la izquierda del panel (etiquetas + su margen), medido con svglite (wjpr_gb_left_of_panel).
  s1 <- st$header_base - 0.36 * st$size_label - st$pitch / 2
  s2 <- st$header - st$pitch - s1
  pn <- inp$panels
  spacing <- if (length(pn) > 1) vapply(seq_len(length(pn) - 1), function(i) {
    if (pn[i + 1] == "header") s1 else if (pn[i] == "header") s2 else s1 + s2
  }, 0) else 0
  top <- 2 + (if (length(pn) && pn[1] == "header") s1 else 0)
  n_units <- (if (has_nat) 1 else 0) + nrow(d)
  used <- top + n_units * st$pitch + sum(spacing) + 4
  # complemento: element_markdown (el de WJPr para la barra nacional) centra la caja de gridtext, con descendentes, y
  # no la altura de mayúsculas como el texto normal: la etiqueta queda 0.104 em por encima de su barra; un margen
  # superior de 0.208 em la baja a la línea base publicada. Con la barra nacional, el axis.text.y de WJPr es
  # ggtext::element_markdown y ggplot2 4 solo le suma otro element_markdown (theme(axis.text.y = element_text(…)) da
  # "Can't merge"): el de las etiquetas, que hace falta igual para los promedios y encabezados en Markdown, se suma
  # sin tocar p@theme (propuesta: estilo de las etiquetas por argumento, sin Markdown en el tema; con ella sobra el
  # margen)
  p <- p + ggplot2::theme(
    axis.text.y = ggtext::element_markdown(family = WJPR_FAMILY, size = st$size_label, colour = st$col$text, hjust = 1,
                                           margin = ggplot2::margin(t = 0.208 * st$size_label, r = st$gap_label, unit = "bigpts")))
  p <- p + ggplot2::theme(
    axis.ticks = ggplot2::element_blank(), axis.ticks.length = grid::unit(0, "bigpts"),
    strip.text = ggplot2::element_blank(), strip.text.y.left = ggplot2::element_blank(),
    strip.switch.pad.grid = grid::unit(0, "bigpts"),
    panel.spacing = grid::unit(spacing, "bigpts"),
    plot.margin = ggplot2::margin(top, 0, 4 + (spec$h_in * 72 - used), 0, unit = "bigpts"))
  left <- wjpr_gb_left_of_panel(p, spec$w_in, spec$h_in)
  if (left > g$x0) warning("wjpr_grouped_bars: las etiquetas miden más que la columna publicada (", round(left, 1), " pt)", call. = FALSE)
  p <- p + ggplot2::theme(plot.margin = ggplot2::margin(top, 0, 4 + (spec$h_in * 72 - used), max(g$x0 - left, 0), unit = "bigpts"))
  comp <- c(comp, "línea base de las etiquetas Markdown", "alto de fila y bloque de encabezado del diseño",
            "márgenes y columna de etiquetas fijos")
  wjpr_meta(p, base = "WJPr::wjp_groupbars", complementos = comp, drawn = drawn)
}
## Prueba del bloque (2026-10-23): Rscript tools/wjpr_gallery.R --figs 1,4,9,12,17,20,21,22,23,24,25,26,27,28,31,33,35,
## 38,39,40, con 1.1.0@382aee9 y con main a46c831 (SRC_REPORTS_LIB) → 20/20 ok en las dos, textos iguales, 0 dudosos,
## f1_publicada 1.000 (r = 0 px: 0.996-0.999; mad 0.0005-0.0011); los 20 SVG son idénticos byte a byte entre builds y
## con los de antes de adaptar main. Complementos: 9-11 con 1.1.0, 8-10 con main (sin wjpr_font). Con main antes de
## esto (level_order al revés): f1 0.72-0.97. complementos = FALSE (WJPr solo): f1 0.17-0.79 (1.1.0; mediana 0.64) y
## 0.14-0.79 (main; mediana 0.62: Inter Tight nativa, misma geometría). Posiciones en el SVG: barra en x0 = 155,
## etiquetas a 149, valores a 410 (27: 441), filas y líneas base de encabezados a ±0.05 pt de STD/ST_A01; escape
## Markdown ("a*b_c*", "1. …", "&").
## Lo dibujado por fila (revisión 3, 2026-10-23, F1; SCRATCH/fix3/F1/pruebas/p08_drawn.R y tools/wjpr_gallery.R): 01, 04 y
## 09 con data() y 27 desde la referencia, con 1.1.0@382aee9 y con main → 18, 17, 36 y 9 filas, las de la referencia en
## el mismo orden visual, |Δfrac| ≤ 0.0028 y textos iguales; galería: 20/20 "n filas ok" en las dos builds; los SVG no
## cambian (md5); figures construye el ggplot una sola vez para wjpr_texts() y wjpr_drawn().
## Contrato extendido (revisión 4, 2026-10-26/27; SCRATCH/fix4/I0/pruebas/iny_gb.sh): label_drawn, grouping_drawn y val
## en las 18/17/36 filas de 01/04/09; cada inyección, una a la vez y solo en el dibujo, con --stage all --figs 01 --strict
## → exit 1: Male/Female, 65-100/18-24 y Urban/Rural intercambiadas (texto · label_drawn), encabezado Gender → Sex
## (texto · label_drawn + grouping_drawn de sus filas), valor de Male con alfa 0 (texto · text Male: 65%→), barra de
## Male +5 pp (dibujo · val 65.35→70.35), Helvetica en los valores (texto · 14 dudosos); en verify, «fallo · figures:
## <motivo>». Sin inyección, 10/10 en las dos builds con los SVG y PNG idénticos (md5) a los de antes; Polonia y Malta
## (01, 04, 09) ok en figures; National Average NA → «fig 01: sin promedio nacional (…)» antes de llamar a WJPr.


## Bloque wjpr dumbbell — figuras 10, 15 ----------------------------------------------------------------------------
## Base: WJPr::wjp_dumbbells() con sus argumentos documentados: data larga (dos filas por línea de la figura, una por
## serie, también para los encabezados, con valor NA), target, grouping (clave única por línea: dos categorías con el
## mismo nombre no se funden), cgroups (= los textos de la leyenda), colors, labels (= df$text / df$text2 tal cual),
## cvec (colores del reporte), order (el de df), ptheme = wjpr_theme() y, según la build cargada: labpos (posición de
## cada valor, a dot_r + 3 pt del punto y por fuera del par; la automática de 1.1.0, ±7 recortada a 3-97, y el
## label_offset de main dan NA en una fila con un solo valor: la 18-24 de la 10 perdería su 68 %) o label_offset (main,
## cuando da la misma posición: la 15), y show_legend = TRUE (main: puntos con el color mapeado y leyenda rotulada con
## cgroups; 1.1.0 no tiene leyenda y el color se mapea como complemento). Se conservan sus 7 capas (blank, ribbon de
## franjas, segment, 2 point, 2 text): los complementos cambian parámetros y mapeos de esas capas, sus escalas, la guía,
## el clip y el tema (ninguna capa nueva). Medidas: ST_DUMB y LABEL_COL[["dumb"]], las de chart_dumbbell()
## (05_charts.R): leyenda con líneas base 10 y 24 pt, rótulos del eje con base 44, línea del eje en 49, filas desde 52;
## la figura 15 (18-24 sin ningún valor) sale como fila vacía sin rama propia.
## complementos = FALSE devuelve WJPr sin complementos (como el piloto: sin encabezados), para medir la brecha.
## Lo dibujado por fila (revisión 4, G1; contrato drawn de wjpr_meta, con y sin complementos): wjpr_dumbbell_drawn,
## una fila por línea con val/val2 = punto de cada serie (por su color y la leyenda impresa), frac = val/100,
## text/text2, label_drawn y grouping_drawn (último encabezado impreso). Las líneas sin valor no pasan a las capas de
## segmento, punto y texto (wjpr_drop_na_rows: sin «Removed N rows» en el log, mismo dibujo).
## Prueba (2026-10-23, tools/wjpr_gallery.R --figs 10,15, con y sin SRC_REPORTS_LIB): textos iguales, 0 dudosos, f1
## 1.000 en las dos con 1.1.0@382aee9 (13 complementos) y con main a46c831 (12 en la 10, que sigue con labpos por su
## 18-24 de un solo valor; 11 en la 15: sobran wjpr_font y labpos). Sin complementos: 0.128 / 0.170 con 1.1.0 (y
## Noto Sans Syriac en lugar de la fuente: 24 textos dudosos) y 0.185 / 0.221 con main (fuente nativa, 0 dudosos).
## Prueba de lo dibujado por fila (revisión 4, 2026-10-27, G1: SCRATCH/fix4/G1/pruebas/iny_g1.sh, una inyección a la
## vez en el dibujo con --stage all --figs 10 --strict → exit 1): texto de percepción Male ↔ Female (texto · text),
## etiquetas Urban ↔ Rural (texto · label_drawn), punto de Male +5 pp (dibujo · val 29.46→34.46; verify frac y val);
## sin inyección 17 filas sin diferencias con df ni con la referencia (85/85 en drawn_ref_rows) y sin complementos 13
## (lo que le falta a WJPr: encabezados y el 68 % de 18-24). Sin inyección: Hungría --stage all --figs 01-07,09-11
## --strict 10/10 con 1.1.0@382aee9 (SVG y PNG = ROOT) y con main a46c831 (la 10 = el código anterior con main), 0
## «Removed N rows» en el log (antes 5); Polonia y Malta ok en figures (Malta: 4 celdas suprimidas, val NA); galería
## --figs 2,7,10,15: «17 filas ok» en la 10 y en la 15 (desde la referencia), SVG de la 15 = el del código anterior.
## Auxiliares de este bloque (los usa también el bloque hbars): unidades del diseño → ggplot2 y edición de capas;
## los de lo dibujado por fila (wjpr_pos_slots, wjpr_pos_slot_of, wjpr_legend_drawn, wjpr_series_of) los usan también
## los bloques paired_bars y severity_bars.

# Conversión de las medidas del diseño (pt = 1/72 in, la unidad del SVG) a ggplot2, comprobada en el SVG de svglite:
# linewidth L → trazo de L·.pt·0.75 pt (lwd 1 = 1/96 in); punto de forma 19 con stroke 0 → radio 0.375·size·.pt;
# texto: size = pt / .pt (los 3.514598 mm de WJPr = 10 pt). margin() y unit() van en "bigpts" (1/72 in): el "pt" de
# grid es 1/72.27 in y correría 0.4 % cada medida (0.7 pt a 184 pt).
wjpr_pt2lw <- function(pt) pt / (ggplot2::.pt * 0.75)
wjpr_pt2size <- function(r_pt) r_pt / (0.375 * ggplot2::.pt)
wjpr_pt2txt <- function(pt) pt / ggplot2::.pt
wjpr_unit_bp <- function(x) grid::unit(x, "bigpts")
wjpr_margin_bp <- function(t = 0, r = 0, b = 0, l = 0) ggplot2::margin(t, r, b, l, unit = "bigpts")
# Alto de la caja de una línea de texto de ggplot2 (ascenso de "M" + descenso de "gjpqyQ") con Inter Tight, en em:
# medido con ggplotGrob() en svglite (8.496 pt a 9 pt, 9.439 a 10; no depende del texto). Con vjust = 0 la línea base
# queda en el borde inferior de la caja.
WJPR_TEXT_BOX_EM <- 0.944

# Índices de las capas de p con una geom dada, en el orden en que la función de WJPr las añade
wjpr_which_layers <- function(p, geom) which(vapply(p$layers, function(l) inherits(l$geom, geom), logical(1)))

# Copia las capas `which` (sin tocar el objeto de WJPr) y cambia sus parámetros fijos (params; un NULL quita el
# parámetro), los de su geom (geom_params) y su mapeo (mapping: aes() que añade o reemplaza aesthetics). ggplot2 4 no
# admite p$layers[[i]] <- capa (desborda la pila al construir) ni un mapeo que sea lista: se rehacen la lista y el
# aes().
wjpr_edit_layers <- function(p, which, params = list(), mapping = NULL, geom_params = list()) {
  p$layers <- lapply(seq_along(p$layers), function(i) {
    l <- p$layers[[i]]
    if (!i %in% which) return(l)
    l2 <- ggplot2::ggproto(NULL, l)
    ap <- l2$aes_params
    for (k in names(params)) if (is.null(params[[k]])) ap[[k]] <- NULL else ap[[k]] <- params[[k]]
    l2$aes_params <- ap
    gp <- l2$geom_params
    for (k in names(geom_params)) if (is.null(geom_params[[k]])) gp[[k]] <- NULL else gp[[k]] <- geom_params[[k]]
    l2$geom_params <- gp
    if (!is.null(mapping)) {
      m <- l2$mapping
      l2$mapping <- ggplot2::aes(!!!c(as.list(m)[setdiff(names(m), names(mapping))], as.list(mapping)))
    }
    l2
  })
  p
}

# Ancho (pt) de la etiqueta más ancha de la columna, como la dibuja el reporte: filas en regular, encabezados y
# promedios en 600 cursiva (label_column(), 04_svg.R, sin su redondeo); text_width() coincide con svglite (±0.05 pt)
wjpr_labcol_w <- function(label, kind, size) {
  if (!length(label)) return(0)
  max(mapply(function(l, k) if (identical(k, "row")) text_width(l, size) else text_width(l, size, 600, TRUE),
             label, kind))
}

# Lo dibujado por fila del dumbbell (contrato drawn de wjpr_meta, revisión 4), leído de b = ggplot_build() del gráfico
# final: una fila por nivel del eje (una línea de la figura: promedio, encabezado o categoría) en orden visual de
# arriba abajo (wjpr_pos_slots), con kind/grouping/label = la fila del df cuya clave ocupa esa posición (keys: level =
# clave del nivel); val/val2 = la posición (0-100 %) del punto de cada serie en esa línea y frac/frac2 = val/100 (el
# frac de la referencia), con la serie de cada punto según su color y la leyenda impresa (series[1] = value,
# series[2] = value2; sin leyenda, según cols); text/text2 = los valores impresos en la línea, cada uno de la serie del
# punto más cercano (van por fuera del par, a dot_r + 3 pt de su punto; sin punto en la línea, la de su color);
# label_drawn = la etiqueta del eje impresa en esa posición y grouping_drawn = la del último encabezado impreso por
# encima (la propia en un encabezado; "" en el promedio, antes del primero). Dos textos o dos etiquetas
# intercambiados, un punto desplazado o un encabezado cambiado se ven aquí aunque el multiconjunto de textos sea igual.
wjpr_dumbbell_drawn <- function(b, keys, series, cols) {
  slots <- wjpr_pos_slots(b, "x")
  lg <- wjpr_legend_drawn(b)
  num <- function(d, nm, na = NA_real_) if (nm %in% names(d)) suppressWarnings(as.numeric(d[[nm]])) else rep(na, nrow(d))
  # unname: ggplot2 4 nombra las capas y bind_rows leería una lista con nombre como columnas
  ip <- unname(which(vapply(b$plot$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))))
  pts <- bind_rows(tibble(slot = integer(), y = numeric(), serie = character()), lapply(ip, function(i) {
    d <- as.data.frame(b$data[[i]])
    ok <- is.finite(num(d, "x")) & is.finite(num(d, "y")) & coalesce(num(d, "size"), 1) > 0 & coalesce(num(d, "alpha"), 1) > 0
    d <- d[ok, , drop = FALSE]
    tibble(slot = wjpr_pos_slot_of(d$x, slots), y = as.numeric(d$y),
           serie = wjpr_series_of(d$colour, lg, "colour", series, cols))
  }))
  pts <- pts[!is.na(pts$slot) & !is.na(pts$serie), , drop = FALSE]
  tx <- wjpr_drawn_texts(b)
  tx <- tx[!is.na(tx$label), , drop = FALSE]               # solo lo que se ve (y sin las filas NA de un color vectorizado)
  txt <- tx[tx$source == "layer", , drop = FALSE]
  txt$slot <- wjpr_pos_slot_of(txt$x, slots); txt <- txt[!is.na(txt$slot), , drop = FALSE]
  txt$serie <- vapply(seq_len(nrow(txt)), function(j) {
    p <- pts[pts$slot == txt$slot[j], , drop = FALSE]
    if (nrow(p)) p$serie[which.min(abs(p$y - txt$y[j]))] else wjpr_series_of(txt$colour[j], lg, "colour", series, cols)
  }, NA_character_)
  ax <- tx[tx$source == "axis-x", , drop = FALSE]; ax$slot <- wjpr_pos_slot_of(ax$x, slots)
  k <- match(slots$level, keys$level)
  kind <- coalesce(keys$kind[k], "")
  lab_drawn <- vapply(slots$slot, function(s) paste(ax$label[ax$slot %in% s], collapse = " "), "")
  hdr <- ""; grp_drawn <- character(nrow(slots))
  for (i in order(slots$slot)) {                           # de arriba abajo
    if (kind[i] == "header") hdr <- lab_drawn[i]
    grp_drawn[i] <- hdr
  }
  val_of <- function(s, se) { y <- pts$y[pts$slot == s & pts$serie %in% se]; if (length(y)) y[1] else NA_real_ }
  txt_of <- function(s, se) paste(txt$label[txt$slot == s & txt$serie %in% se], collapse = " ")
  v1 <- vapply(slots$slot, val_of, 0, se = series[1]); v2 <- vapply(slots$slot, val_of, 0, se = series[2])
  out <- tibble(order = slots$slot, kind = kind, grouping = coalesce(keys$grouping[k], ""),
                label = coalesce(keys$label[k], slots$level), frac = v1 / 100, frac2 = v2 / 100,
                text = vapply(slots$slot, txt_of, "", se = series[1]), text2 = vapply(slots$slot, txt_of, "", se = series[2]),
                label_drawn = lab_drawn, grouping_drawn = grp_drawn, val = v1, val2 = v2)
  out[order(out$order), , drop = FALSE]
}
wjpr_dumbbell_drawn_fun <- function(keys, series, cols) {
  force(keys); force(series); force(cols)
  function(b) wjpr_dumbbell_drawn(b, keys, series, cols)
}

# Copia las capas `which` sin las filas de su data cuya posición en el eje de valores (los aesthetics y e yend de su
# mapping) es NA: no se dibujan y ggplot2 las quitaba con el aviso «Removed N rows containing missing values»
wjpr_drop_na_rows <- function(p, which) {
  p$layers <- lapply(seq_along(p$layers), function(i) {
    l <- p$layers[[i]]
    if (!i %in% which || !is.data.frame(l$data)) return(l)
    keep <- rep(TRUE, nrow(l$data))
    for (a in intersect(c("y", "yend"), names(l$mapping))) keep <- keep & !is.na(rlang::eval_tidy(l$mapping[[a]], l$data))
    if (all(keep)) return(l)
    l2 <- ggplot2::ggproto(NULL, l)
    l2$data <- l$data[keep, , drop = FALSE]
    l2
  })
  p
}

wjpr_dumbbell <- function(df, spec, legend = c("General population: perception",
                                               "People with a non-trivial problem: experience"),
                          st = ST_DUMB, complementos = wjpr_complementos()) {
  wjpr_require()
  df <- df[order(df$order), , drop = FALSE]
  cg <- legend                                     # series = sus textos de leyenda: main rotula su leyenda con cgroups
  cvec <- stats::setNames(c(st$col$main, st$col$exp), cg)
  v1 <- as.numeric(df$value); v2 <- as.numeric(df$value2)
  t1 <- coalesce(as.character(df$text), ""); t2 <- coalesce(as.character(df$text2), "")
  if (!isTRUE(complementos)) {
    r <- df$kind != "header"
    long <- bind_rows(tibble(cat = df$label[r], serie = cg[1], v = v1[r], lab = t1[r]),
                      tibble(cat = df$label[r], serie = cg[2], v = v2[r], lab = t2[r]))
    p <- WJPr::wjp_dumbbells(data = long, target = "v", grouping = "cat", cgroups = cg, colors = "serie",
                             labels = "lab", cvec = cvec, order = setNames(df$order[r], df$label[r]),
                             ptheme = wjpr_theme())
    # corregido (revisión 4): lo dibujado por fila también sin complementos (nivel del eje = df$label; sin encabezados)
    keys0 <- tibble(level = df$label[r], kind = df$kind[r], grouping = df$grouping[r], label = df$label[r])
    return(wjpr_meta(p, base = "WJPr::wjp_dumbbells", drawn = wjpr_dumbbell_drawn_fun(keys0, cg, unname(cvec))))
  }
  geo <- list(leg_base = c(10, 24), leg_dot = 3.4, ax_base = 44, ax_line = 49, rows0 = 52)  # chart_dumbbell()
  W <- spec$w_in * 72; H <- spec$h_in * 72
  L <- label_col_pt(spec, tibble(kind = df$kind, label = df$label), st)
  track <- W - L - st$gap_label - st$gap_value - st$value_w
  x0 <- L + st$gap_label
  pct <- function(pt) pt / track * 100                                         # pt → unidades de la escala 0-100
  # centro (pt desde arriba) de cada línea: fila = mitad de su paso; encabezado = centro de su rótulo (línea base
  # header_base dentro de su bloque; ggplot2 centra el texto en la altura de mayúsculas, 0.36 em sobre la base)
  ctr <- numeric(nrow(df)); y <- geo$rows0
  for (i in seq_len(nrow(df))) {
    if (df$kind[i] == "header") { ctr[i] <- y + st$header_base - 0.36 * st$size_label; y <- y + st$header }
    else { ctr[i] <- y + st$pitch / 2; y <- y + st$pitch }
  }
  key <- sprintf("r%03d", seq_len(nrow(df)))
  gap <- pct(st$dot_r + 3)
  two <- !is.na(v1) & !is.na(v2)
  left1 <- two & v1 <= v2; left2 <- two & v2 < v1                              # el menor del par va a la izquierda
  pos1 <- if_else(left1, v1 - gap, v1 + gap); pos2 <- if_else(left2, v2 - gap, v2 + gap)
  long <- bind_rows(tibble(key = key, serie = cg[1], v = v1, lab = t1, pos = pos1),
                    tibble(key = key, serie = cg[2], v = v2, lab = t2, pos = pos2))
  # Lo que trae la build cargada: main dibuja la leyenda (show_legend; puntos con el color mapeado y escala con los
  # cgroups) y coloca los valores con label_offset (por fuera del par, a label_offset del punto); 1.1.0 no tiene ninguno
  leg_native <- wjpr_has_arg("wjp_dumbbells", "show_legend")
  pp <- c(pos1[two], pos2[two])
  off_native <- wjpr_has_arg("wjp_dumbbells", "label_offset") && !any(xor(is.na(v1), is.na(v2))) &&
    all(pp >= gap & pp <= 100 - gap)
  args <- list(data = long, target = "v", grouping = "key", cgroups = cg, colors = "serie", labels = "lab",
               cvec = cvec, order = setNames(seq_along(key), key), ptheme = wjpr_theme())
  if (leg_native) args$show_legend <- TRUE
  # complemento: posición de los valores (labpos, a dot_r + 3 pt del punto y por fuera del par) donde label_offset no
  # llega: en una fila con un solo valor da NA (if_else(start <= end) con start NA: la 18-24 de la 10 perdería su
  # 68 %) y recorta a [offset, 100 − offset]; 1.1.0 no lo tiene (su automática: ±7, recortada a 3-97). Con un
  # label_offset que coloque el valor solitario, el complemento sobraría en todas
  args <- c(args, if (off_native) list(label_offset = gap) else list(labpos = "pos"))
  p <- do.call(WJPr::wjp_dumbbells, args)
  comp <- character()
  if (!wjpr_native_font()) { p <- wjpr_font(p); comp <- "fuente (wjpr_font)" }  # main: options(wjpr.family)
  ip <- wjpr_which_layers(p, "GeomPoint"); it <- wjpr_which_layers(p, "GeomText")
  is <- wjpr_which_layers(p, "GeomSegment")
  # complemento: tamaño de punto (r = dot_r, sin trazo): wjp_dumbbells fija size = 4.5 (r ≈ 5.5 pt), sin argumento
  if (leg_native) {
    p <- wjpr_edit_layers(p, ip, params = list(size = wjpr_pt2size(st$dot_r), stroke = 0))
  } else {
    # complemento: leyenda (1.1.0): wjp_dumbbells da a los puntos un color fijo y no dibuja leyenda; se mapea el
    # color de cada serie para que la guía de ggplot2 la dibuje con los textos de `legend`
    for (j in 1:2) p <- wjpr_edit_layers(p, ip[j], params = list(size = wjpr_pt2size(st$dot_r), stroke = 0, colour = NULL),
                                         mapping = ggplot2::aes(colour = !!cg[j]))
  }
  # complemento: segmento del color y grosor del reporte (wjp_dumbbells: #BFBFBF, linewidth 2.5, sin argumento)
  p <- wjpr_edit_layers(p, is, params = list(colour = st$col$track, linewidth = wjpr_pt2lw(st$segment)))
  # complemento: valores en semibold cursiva de size_value pt, anclados por fuera del punto (wjp_dumbbells: negrita de
  # 10 pt centrada en su posición, sin argumento de cara, tamaño ni alineación)
  p <- wjpr_edit_layers(p, it[1], params = list(fontface = "bold.italic", size = wjpr_pt2txt(st$size_value)),
                        mapping = ggplot2::aes(hjust = as.numeric(labp0 < start)))
  p <- wjpr_edit_layers(p, it[2], params = list(fontface = "bold.italic", size = wjpr_pt2txt(st$size_value)),
                        mapping = ggplot2::aes(hjust = as.numeric(labp1 < end)))
  lv <- rev(key)                                   # niveles de WJPr: reorder(group, -order) → de abajo arriba
  posv <- H - ctr                                  # posición de cada línea en pt desde el borde inferior
  head_lv <- (df$kind != "row")[match(lv, key)]
  # leyenda: la clave de un punto mide size/10 cm (ggplot2); se corre a la izquierda para que el círculo quede en
  # x0 + dot_r y el texto en x0 + 2·dot_r + 5, como en el reporte
  key_w <- wjpr_pt2size(st$dot_r) / 10 / 2.54 * 72
  key_h <- diff(geo$leg_base)
  shift <- key_w / 2 - st$dot_r
  top <- geo$leg_base[1] - geo$leg_dot - key_h / 2                            # borde superior de la leyenda
  ax_box <- WJPR_TEXT_BOX_EM * st$size_axis
  # corregido (revisión 4): las líneas sin valor (encabezados, y la serie que falta en una fila de un solo valor: 18-24
  # de la 10) no pasan a las capas de segmento, punto y texto; ggplot2 las quitaba con «Removed N rows containing
  # missing values» (5 avisos por figura en el log de la 10). geom_blank conserva todos los niveles: el dibujo no cambia
  p <- wjpr_drop_na_rows(p, c(is, ip, it))
  # corregido (revisión 4): sin los mensajes «Scale for … is already present» / «Coordinate system already present»
  # en el log: las escalas y la coord de abajo reemplazan las de WJPr a propósito (complementos)
  p <- suppressMessages(p +
    # complemento: encabezados de grupo y alturas del reporte (fila 18 pt, encabezado 22): posición de cada nivel
    # del eje discreto; wjp_dumbbells no admite encabezados ni alturas distintas, y rotula con `grouping`
    ggplot2::scale_x_discrete(palette = function(n) unname(posv[match(lv, key)]), labels = setNames(df$label, key),
                              expand = ggplot2::expansion(add = c(min(posv), H - geo$ax_line - max(posv)))) +
    # complemento: eje 0-100 % cada 25 (wjp_dumbbells fija breaks = seq(0, 100, 20)) sin expansión: la pista del
    # diseño (track pt) es exactamente 0-100 %; oob_keep conserva los valores impresos más allá de 100 %
    ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"),
                                expand = c(0, 0), oob = scales::oob_keep, position = "right") +
    # complemento: sin franjas alternas (bgcolor solo cambia las blancas; las grises son #EBEBEB fijas)
    ggplot2::scale_fill_manual(values = c(grey = "transparent", white = "transparent"), guide = "none") +
    # leyenda: 1.1.0, escala del color mapeado; main, complemento: una serie por línea (su guía fija direction =
    # "horizontal", nrow = 1 y override.aes size = 4.5; la guía por defecto toma la dirección del tema y el punto).
    # Con un argumento de dirección o columnas de la leyenda en wjp_dumbbells, la guía sobraría
    (if (leg_native) ggplot2::guides(colour = ggplot2::guide_legend()) else
      ggplot2::scale_colour_manual(values = cvec, breaks = cg, name = NULL)) +
    # complemento: clip = "off": un valor junto a 0 o 100 % se imprime fuera de la pista (coord_flip() de WJPr recorta)
    ggplot2::coord_flip(clip = "off") +
    ggplot2::theme(
      # complemento: sin rejilla (WJPr: discontinua cada 20 % y encima de las capas por panel.ontop = TRUE)
      panel.grid.major = ggplot2::element_blank(),
      # complemento: columna de etiquetas alineada a la derecha, encabezados y promedio en semibold cursiva del color
      # principal (WJPr: hjust = 0 y un solo estilo, #524F4C, puestos después de ptheme). Color y cara van
      # vectorizados por nivel: ggplot2 4.0.3 lo aplica bien pero avisa al crear el elemento que no es oficial (se
      # silencia ese aviso); ggtext::element_markdown() también sirve, pero centra la caja de la línea y no la altura
      # de mayúsculas (0.108 em más arriba) y parte cada etiqueta en palabras
      axis.text.y = suppressWarnings(ggplot2::element_text(hjust = 1, size = st$size_label,
                                                           margin = wjpr_margin_bp(r = st$gap_label),
                                                           colour = ifelse(head_lv, st$col$main, st$col$text),
                                                           face = ifelse(head_lv, "bold.italic", "plain"))),
      # complemento: rótulos del eje de size_axis pt sobre una línea de eje (WJPr: 10 pt #524F4C, sin línea)
      axis.text.x.top = ggplot2::element_text(size = st$size_axis, colour = st$col$text, vjust = 0,
                                              margin = wjpr_margin_bp(b = geo$ax_line - geo$ax_base)),
      axis.line.x.top = ggplot2::element_line(colour = st$col$axis, linewidth = wjpr_pt2lw(0.75)),
      axis.ticks.length = wjpr_unit_bp(0),
      # complemento: leyenda arriba a la izquierda, una serie por línea, texto regular de size_legend pt y claves del
      # tamaño del punto (main, wjp_legend_theme: horizontal, 11 pt negrita, clave de 1.4 cm y márgenes 4 / 20 pt;
      # sin argumentos de tamaño, cara ni márgenes, que entrarían en wjp_legend_theme y en cada función)
      legend.position = "top", legend.justification = "left", legend.direction = "vertical",
      legend.margin = wjpr_margin_bp(l = -shift), legend.box.margin = wjpr_margin_bp(), legend.key = ggplot2::element_blank(),
      legend.key.spacing.y = wjpr_unit_bp(0), legend.key.width = wjpr_unit_bp(2 * st$dot_r),
      legend.key.height = wjpr_unit_bp(key_h),
      legend.box.spacing = wjpr_unit_bp(geo$ax_base - ax_box - (top + 2 * key_h)),
      legend.text = ggplot2::element_text(size = st$size_legend, colour = st$col$text, face = "plain",
                                          margin = wjpr_margin_bp(l = 5 - shift)),
      # complemento: márgenes que ponen la pista en x0..x0 + track y el panel desde la línea del eje hasta abajo
      plot.margin = wjpr_margin_bp(t = top, r = W - x0 - track, b = 0,
                                   l = x0 - st$gap_label - wjpr_labcol_w(df$label, df$kind, st$size_label))))
  comp <- c(comp, "tamaño de punto",
            if (leg_native) "leyenda: una serie por línea, texto y claves (guía + tema)" else
              "leyenda (color mapeado + escala + tema)",
            "segmento: color y grosor", "valores: cara, tamaño y anclaje",
            if (!off_native) "posición de los valores (labpos)", "encabezados y alturas de fila (escala discreta)",
            "eje cada 25 % sin expansión", "sin franjas alternas", "clip off", "sin rejilla",
            "columna de etiquetas: alineación y estilos", "eje: tamaño, línea y márgenes")
  # corregido (revisión 4): lo dibujado por fila (wjpr_dumbbell_drawn: punto, texto y etiqueta impresos en cada línea);
  # antes el dumbbell solo lo medían el parecido y el multiconjunto de textos, que no ven dos textos o dos etiquetas
  # intercambiados ni un punto desplazado
  keys <- tibble(level = key, kind = df$kind, grouping = df$grouping, label = df$label)
  wjpr_meta(p, base = "WJPr::wjp_dumbbells", complementos = comp, drawn = wjpr_dumbbell_drawn_fun(keys, cg, unname(cvec)))
}


## Bloque wjpr hbars — figuras 13, 14, 18, 19, 36 -------------------------------------------------------------------
## Base: WJPr::wjp_bars(direction = "horizontal") con sus argumentos documentados: data, target, grouping (la
## categoría), labels (= df$text tal cual), colors + cvec (una serie en el color principal), stacked = FALSE, lab_pos
## (el valor más gap_value pt: el texto empieza tras el final de la barra), order (el de df), width (barra de st$bar pt
## en un paso de st$pitch) y ptheme = wjpr_theme(). Se conservan sus 2 capas (col, text), su eje arriba y su rejilla
## discontinua cada 20 % (#D1CFD1, 0.53 pt; publicada #D0D1D3, 0.6 pt: no se toca). Se probaron las tres bases que
## pide el encargo, todas con complementos hasta f1 1.000 (galería 23-oct): wjp_bars (7 complementos), wjp_lollipops
## (8: la línea gruesa hace de barra y los puntos se ocultan con point_size = 0) y wjp_edgebars (10: rehace el eje y
## la rejilla que quita, lleva los rótulos de encima de la barra a la columna con un y_lab_pos negativo y dibuja
## df$text en vez de su round()); queda wjp_bars por defecto (menos complementos y capas con su significado: barras =
## geom_col, etiquetas = eje). base = "wjp_lollipops" | "wjp_edgebars" las reproduce. Usa los auxiliares del bloque
## dumbbell. Medidas: ST_HBARS y LABEL_COL[["hbars"]], las de chart_hbars_axis (report_charts.R:224-242): etiquetas
## desde x = 2, barras desde L + 12, pista track pt, rótulos del eje con base 12 pt, filas desde 20, rejilla desde 18.
## complementos = FALSE devuelve la función de WJPr sin complementos.
## Prueba (2026-10-23, tools/wjpr_gallery.R --figs 13,14,18,19,36, con y sin SRC_REPORTS_LIB): textos iguales, 0
## dudosos, f1 1.000 en las cinco con 1.1.0@382aee9 (7 complementos) y con main a46c831 (6: la fuente sale de
## options(wjpr.family)); sin complementos: 0.554, 0.680, 0.709, 0.862, 0.267 (1.1.0) y 0.557, 0.685, 0.710, 0.863,
## 0.270 (main). Bases alternativas (mismas cinco): lollipops 1.000 en las dos builds (8 / 7 complementos; sin ellos
## 0.26-0.49); edgebars 1.000 con 1.1.0 (10; sin ellos 0.31-0.43) y con main se detiene: la rehízo (rectángulos sobre
## una y numérica, rótulo encima de la barra, bar_width ≤ 0.8) y sus complementos no le corresponden.
wjpr_hbars <- function(df, spec, base = c("wjp_bars", "wjp_lollipops", "wjp_edgebars"), st = ST_HBARS,
                       complementos = wjpr_complementos()) {
  wjpr_require()
  base <- match.arg(base)
  df <- df[order(df$order), , drop = FALSE]
  df <- df[df$kind != "header", , drop = FALSE]
  W <- spec$w_in * 72; H <- spec$h_in * 72
  L <- label_col_pt(spec, tibble(kind = df$kind, label = df$label), st)
  x0 <- L + 12; track <- st$track
  geo <- list(lab_x = 2, ax_base = 12, rows0 = 20, grid0 = 18)               # chart_hbars_axis()
  d <- tibble(order = df$order, label = df$label, value = as.numeric(df$value),
              text = coalesce(as.character(df$text), ""), serie = "main",
              pos = as.numeric(df$value) + st$gap_value / track * 100)
  n <- nrow(d)
  lab_lo <- (geo$lab_x - x0) / track * 100                                     # x = 2 pt en la escala (edgebars)
  bare <- !isTRUE(complementos)
  # main rehízo wjp_edgebars (rectángulos sobre una y numérica, rótulo encima de la barra, sin eje ni coord_flip, y
  # bar_width ≤ 0.8): los complementos de abajo son los de su API de 1.1.0 y no se aplican a esa versión
  if (!bare && base == "wjp_edgebars" && wjpr_has_arg("wjp_edgebars", "label_start"))
    stop("base = \"wjp_edgebars\": complementos escritos para la API de WJPr 1.1.0; con esta build usa wjp_bars o ",
         "wjp_lollipops", call. = FALSE)
  p <- switch(base,
    wjp_bars = if (bare) {
      WJPr::wjp_bars(data = d, target = "value", grouping = "label", labels = "text", colors = "serie",
                     cvec = c(main = st$col$main), direction = "horizontal", order = "order", ptheme = wjpr_theme())
    } else {
      WJPr::wjp_bars(data = d, target = "value", grouping = "label", labels = "text", colors = "serie",
                     cvec = c(main = st$col$main), direction = "horizontal", stacked = FALSE, lab_pos = "pos",
                     order = "order", width = st$bar / st$pitch, ptheme = wjpr_theme())
    },
    wjp_lollipops = if (bare) {
      WJPr::wjp_lollipops(data = d, target = "value", grouping = "label", labels = "text", order = "order",
                          ptheme = wjpr_theme())
    } else {
      WJPr::wjp_lollipops(data = d, target = "value", grouping = "label", labels = "text", order = "order",
                          line_size = wjpr_pt2lw(st$bar), point_size = 0, line_color = st$col$main,
                          point_color = st$col$main, ptheme = wjpr_theme())
    },
    wjp_edgebars = if (bare) {
      WJPr::wjp_edgebars(data = d, target = "value", grouping = "label", labels = "label", cvec = st$col$main,
                         x_lab_pos = "order", ptheme = wjpr_theme())
    } else {
      WJPr::wjp_edgebars(data = d, target = "value", grouping = "label", labels = "label", cvec = st$col$main,
                         x_lab_pos = "order", y_lab_pos = lab_lo, bar_width = st$bar / st$pitch,
                         ptheme = wjpr_theme())
    })
  if (bare) return(wjpr_meta(p, base = paste0("WJPr::", base)))
  comp <- character()
  if (!wjpr_native_font()) {                         # main: la fuente sale de options(wjpr.family) (wjpr_require)
    comp <- "fuente (wjpr_font)"
    if (base == "wjp_edgebars") {
      # complemento: wjp_edgebars 1.1.0 mapea family = "Lato Full" dentro de aes() en el rótulo (geom_richtext):
      # wjpr_font quita ese mapeo y el rótulo quedaría sin familia (Helvetica); se fija la del reporte como parámetro
      p <- wjpr_edit_layers(p, wjpr_which_layers(p, "GeomRichText"), params = list(family = WJPR_FAMILY))
    }
    p <- wjpr_font(p)
  }
  it <- wjpr_which_layers(p, "GeomText")
  # complemento: valor a continuación del final de la barra (hjust = 0, gap_value pt) en semibold cursiva y en el
  # color del texto del reporte (WJPr: negrita #4a4a49 centrada en lab_pos; lollipops: en el valor + 7, sin lab_pos;
  # edgebars: hjust = -0.1 y el texto rehecho con round(): se dibuja df$text)
  vmap <- switch(base, wjp_bars = NULL, wjp_lollipops = ggplot2::aes(y = pos),
                 wjp_edgebars = ggplot2::aes(y = pos, label = text))
  p <- wjpr_edit_layers(p, it, params = list(hjust = 0, fontface = "bold.italic", colour = st$col$text), mapping = vmap)
  comp <- c(comp, "valores: anclaje, cara y color")
  if (base == "wjp_lollipops") comp <- c(comp, "valores: posición (mapeo y)")
  if (base == "wjp_edgebars") {
    # complemento: rótulo centrado en la barra (wjp_edgebars lo apoya encima: vjust = 0 + label.padding nudge_lab).
    # geom_richtext centra la caja de la línea (ascenso + descenso) y no la altura de mayúsculas: 0.216 em de relleno
    # arriba bajan el texto 0.108 em, hasta la línea base de los valores (medido en el SVG)
    p <- wjpr_edit_layers(p, wjpr_which_layers(p, "GeomRichText"), params = list(vjust = 0.5),
                          geom_params = list(label.padding = wjpr_unit_bp(c(0.216 * st$size_label, 0, 0, 0))))
    comp <- c(comp, "valores: texto = df$text (mapeo label)", "rótulos: centrados en la barra")
  }
  bottom <- (H - (geo$rows0 + n * st$pitch)) / st$pitch + 0.5
  top <- (geo$rows0 - geo$grid0) / st$pitch + 0.5
  # complemento: alto de fila del diseño (paso st$pitch): la rejilla empieza 2 pt sobre la primera barra y llega al
  # borde inferior (WJPr: expansión por defecto de la escala discreta, que reparte el alto de la figura); wjp_bars
  # invierte el orden con limits = rev, que se conserva
  xs <- if (base == "wjp_bars") {
    ggplot2::scale_x_discrete(limits = rev, expand = ggplot2::expansion(add = c(bottom, top)))
  } else {
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(bottom, top)))
  }
  maxw <- wjpr_labcol_w(d$label, rep("row", n), st$size_label)
  ax_txt <- ggplot2::element_text(size = st$size_label, colour = st$col$text, vjust = 0,
                                  margin = wjpr_margin_bp(b = geo$grid0 - geo$ax_base))
  p <- p + xs +
    # complemento: pista 0-100 % sin expansión (WJPr: limits = c(0, 110) o c(0, 105) y expansión por defecto)
    ggplot2::scale_y_continuous(limits = c(if (base == "wjp_edgebars") lab_lo else 0, 100), breaks = seq(0, 100, 20),
                                labels = paste0(seq(0, 100, 20), "%"), expand = c(0, 0), oob = scales::oob_keep,
                                position = "right") +
    # complemento: clip = "off": el valor de una barra cercana a 100 % se imprime fuera de la pista
    ggplot2::coord_flip(clip = "off") +
    ggplot2::theme(
      # complemento: color del texto del reporte (#1A1A1A; WJP_theme: #524F4C) y columna de etiquetas de L + 12 pt
      # alineadas a la izquierda desde x = 2 (el hjust = 0 de wjp_bars se conserva)
      axis.text.y = if (base == "wjp_edgebars") ggplot2::element_blank() else
        ggplot2::element_text(hjust = 0, colour = st$col$text, margin = wjpr_margin_bp(r = x0 - geo$lab_x - maxw)),
      axis.text.x.top = ax_txt,
      axis.ticks.length = wjpr_unit_bp(0),
      # complemento: márgenes que ponen la pista en x0..x0 + track y la rejilla desde grid0 hasta abajo
      plot.margin = wjpr_margin_bp(t = geo$ax_base - WJPR_TEXT_BOX_EM * st$size_label, r = W - x0 - track, b = 0,
                                   l = geo$lab_x))
  comp <- c(comp, "alto de fila y arranque de la rejilla (escala discreta)", "pista 0-100 % sin expansión", "clip off",
            "columna de etiquetas y márgenes", "color del texto")
  if (base == "wjp_edgebars") {
    # complemento: wjp_edgebars quita los rótulos del eje y la rejilla: se restituyen los de wjp_bars
    p <- p + ggplot2::theme(axis.text.x = ax_txt, panel.grid.major.x = ggplot2::element_line(
      colour = "#d1cfd1", linewidth = 0.25, linetype = "dashed"))
    comp <- c(comp, "eje y rejilla restituidos")
  }
  wjpr_meta(p, base = paste0("WJPr::", base), complementos = comp)
}


## Bloque wjpr paired_bars — figuras 2 ------------------------------------------------------------------------------
## Las barras pareadas (2), las columnas (30, 37), la severidad (7) y el embudo (8, 34) parten de WJPr::wjp_bars().
## Este bloque define también los auxiliares comunes a los cuatro (prefijo wjpr_wb_): datos largos país | UE, copia
## modificada de una capa de WJPr, pista, la llamada a wjp_bars con la leyenda de la build (main: show_legend; 1.1.0:
## complemento en la capa), su tema {country} | EU Average, la fuente según la build y medidas de Inter Tight en el
## texto de grid. Fig 2: ancho 8.4 in con columna de etiquetas propia (la más ancha + 2 pt), pista hasta la columna de
## valores.
## Lo dibujado por fila (revisión 4, G1; contrato drawn de wjpr_meta, también con bare = TRUE): wjpr_pb_drawn, una fila
## por categoría con val/frac (país) y val2/frac2 (UE) según el relleno de cada barra y la leyenda impresa, text/text2
## de la barra más cercana y label_drawn (la categoría impresa). Los auxiliares wjpr_wb_* no cambian.
## Prueba (galería, 2026-10-23, con y sin SRC_REPORTS_LIB): fig 2 f1 1.000 con 1.1.0@382aee9 (8 complementos) y con
## main a46c831 (7: leyenda con show_legend y fuente nativa), textos iguales, 0 dudosos; sin complementos 0.530 (1.1.0)
## y 0.332 (main: su leyenda, arriba y fuera del panel, desplaza el dibujo; sin show_legend daría 0.527).
## Prueba de lo dibujado por fila (revisión 4, 2026-10-27, G1: SCRATCH/fix4/G1/pruebas/iny_g1.sh, una inyección a la
## vez en el dibujo con --stage all --figs 02 --strict → exit 1): textos del país Public Services ↔ Housing (texto ·
## text), categorías Community ↔ Money & Debt (texto · label_drawn), barra del país +5 pp (dibujo · val 28.23→33.23),
## series cambiadas en una fila, solo barras (dibujo · val, val2) o barras y textos (texto + dibujo), leyenda
## intercambiada (texto + dibujo en las 12 filas); en verify, fallo y las filas de drawn.csv frente a la referencia.
## Sin inyección 12 filas sin diferencias (61/61 en drawn_ref_rows); sin complementos los dos textos de cada fila
## caen en la misma posición (wjp_bars apila) y salen como texto del país. Sin inyección: Hungría 10/10 con las dos
## builds (SVG y PNG de la 2 = ROOT), Polonia y Malta ok en figures, galería «12 filas ok».

# Medidas de Inter Tight en el texto de grid (medidas en el SVG de svglite), en em: ascendente que apoya vjust = 1 y
# alto de un texto de eje (ascendente + descendente de la fuente)
WJPR_WB_ASC <- 0.725
WJPR_WB_TEXT_H <- 0.94
WJPR_WB_LH <- 1.22 / 1.2       # interlineado de las figuras propias (1.22 em) en lineheight de grid (1.2 em por línea)
wjpr_wb_lw <- function(pt) pt / 0.75 / ggplot2::.pt      # grosor en pt → linewidth de ggplot2 (mm)

# Datos largos para wjp_bars: una fila por categoría y serie (país primero) con el texto de cada serie tal cual;
# un valor suprimido se dibuja como barra de largo 0 (na = TRUE) para que no cambie el sitio de la otra serie
wjpr_wb_long <- function(df, spec) {
  rows <- df %>% filter(kind == "row") %>% arrange(order)
  lv <- c(spec$country, "EU Average")
  bind_rows(rows %>% transmute(order, label, serie = lv[1], v = value, lab = text),
            rows %>% transmute(order, label, serie = lv[2], v = value2, lab = text2)) %>%
    mutate(serie = factor(serie, levels = lv), lab = coalesce(lab, ""), na = is.na(v), v = coalesce(v, 0))
}

# Copia de una capa de WJPr (sin tocar el objeto original) con campos (position, show.legend…), parámetros fijos
# añadidos o quitados y, si se da, otro mapping
wjpr_wb_layer <- function(l, ..., aes_params = list(), drop_params = character(), mapping = NULL) {
  l2 <- ggplot2::ggproto(NULL, l)
  mods <- list(...)
  for (k in names(mods)) l2[[k]] <- mods[[k]]
  l2$aes_params <- utils::modifyList(l2$aes_params[setdiff(names(l2$aes_params), drop_params)], aes_params)
  if (!is.null(mapping)) l2$mapping <- mapping
  l2
}

# Índices de las capas de barras y de texto de wjp_bars
wjpr_wb_is <- function(p, geom) vapply(p$layers, function(l) inherits(l$geom, geom), logical(1))

# Leyenda de wjp_bars: main la dibuja con su argumento show_legend = TRUE (wjpr_wb_bars(legend = TRUE) lo pasa, también
# sin complementos: es un argumento documentado); 1.1.0 la oculta con show.legend = FALSE fijo en geom_col y
# wjpr_wb_legend_on() la activa en una copia de la capa (complemento, solo en esa build)
wjpr_wb_native_legend <- function() wjpr_has_arg("wjp_bars", "show_legend")
wjpr_wb_bars <- function(..., legend = FALSE) {
  if (legend && wjpr_wb_native_legend()) WJPr::wjp_bars(..., show_legend = TRUE) else WJPr::wjp_bars(...)
}
wjpr_wb_legend_on <- function(p) {
  if (wjpr_wb_native_legend()) return(p)
  is_col <- wjpr_wb_is(p, "GeomCol")
  p$layers[is_col] <- lapply(p$layers[is_col], wjpr_wb_layer, show.legend = TRUE)
  p
}
# Nombre del complemento de leyenda según la build: en 1.1.0 la capa y el tema; en main solo el tema
wjpr_wb_legend_comp <- function(where) {
  if (wjpr_wb_native_legend()) paste0("leyenda ", where, ": texto y claves (tema)") else
    paste0("leyenda {country} | EU Average ", where, " (capa + tema)")
}
# Fuente: main la toma de options(wjpr.family) (wjpr_require); en 1.1.0 wjpr_font reemplaza la "Lato Full" fija
wjpr_wb_font <- function(p) if (wjpr_native_font()) p else wjpr_font(p)
wjpr_wb_font_comp <- function() if (wjpr_native_font()) character() else "fuente Inter Tight en capas"

# Capa de pista (barra al 100 % detrás de cada barra, 0 si el valor está suprimido) en la posición de las barras
wjpr_wb_track <- function(width, position, fill) {
  ggplot2::geom_col(data = function(x) dplyr::mutate(x, target_var = ifelse(na, 0, 100)),
                    ggplot2::aes(group = colors_var), fill = fill, width = width, position = position,
                    show.legend = FALSE, inherit.aes = TRUE)
}

# Leyenda {country} | EU Average dentro del panel, arriba a la izquierda (cuadros de 9 pt a top pt del borde, texto a
# gap_key pt del cuadro, gap_item pt entre elementos: chart_paired / chart_columns). Texto regular, ancho de la clave
# y margen de la caja explícitos: main (wjp_legend_theme) los fija en 11 pt negrita, 1.4 cm y 4 / 20 pt
wjpr_wb_legend <- function(st, top = 2, key = 9, gap_key = 4, gap_item = 14) {
  ggplot2::theme(
    legend.position = "inside", legend.position.inside = c(0, 1), legend.justification.inside = c(0, 1),
    legend.direction = "horizontal", legend.title = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(), legend.key = ggplot2::element_blank(),
    legend.margin = ggplot2::margin(top, 0, 0, 0), legend.box.margin = ggplot2::margin(0, 0, 0, 0),
    legend.key.size = grid::unit(key, "pt"), legend.key.width = grid::unit(key, "pt"),
    legend.key.spacing.x = grid::unit(gap_item, "pt"),
    legend.text = ggplot2::element_text(size = st$size_legend, colour = st$col$text, face = "plain",
                                        margin = ggplot2::margin(l = gap_key)))
}

# Lo dibujado por fila de las barras pareadas (contrato drawn de wjpr_meta, revisión 4), leído de b = ggplot_build()
# del gráfico final: una fila por categoría del eje (wjpr_pos_slots: con coord_flip, de arriba abajo), con
# kind/grouping/label = la fila del df cuya categoría ocupa esa posición (keys: level = categoría). Cada barra de la
# posición es de la serie que la leyenda impresa da a su relleno (wjpr_legend_drawn; sin leyenda, según fills): series[1]
# (el país) → val = su largo (en %, las unidades de df$value) y frac = val / 100 (el frac de la referencia); series[2]
# (la UE) → val2, frac2. La pista (relleno track_fill) no es serie; una barra de largo 0 con pista 0 o sin pista es un
# valor suprimido (wjpr_wb_long: na = TRUE) → val NA. text/text2 = los valores impresos en la posición, cada uno de la
# serie de la barra más cercana (van esquivados con ella); label_drawn = la categoría impresa en el eje en esa
# posición. Dos textos o dos categorías intercambiados, una barra más larga o las series cambiadas en una fila se ven
# aquí aunque el multiconjunto de textos sea igual.
wjpr_pb_drawn <- function(b, keys, series, fills, track_fill) {
  slots <- wjpr_pos_slots(b, "x")
  lg <- wjpr_legend_drawn(b)
  ib <- unname(which(vapply(b$plot$layers, function(l) inherits(l$geom, c("GeomCol", "GeomBar", "GeomRect")), logical(1))))
  bars <- bind_rows(tibble(x = numeric(), slot = integer(), len = numeric(), track = logical(), serie = character()),
                    lapply(ib, function(i) {
    d <- as.data.frame(b$data[[i]])
    if (!all(c("x", "ymin", "ymax", "fill") %in% names(d))) return(NULL)
    tibble(x = as.numeric(d$x), slot = wjpr_pos_slot_of(d$x, slots), len = abs(as.numeric(d$ymax) - as.numeric(d$ymin)),
           track = wjpr_same_color(d$fill, track_fill), serie = wjpr_series_of(d$fill, lg, "fill", series, fills))
  }))
  bars <- bars[!is.na(bars$slot) & is.finite(bars$len), , drop = FALSE]
  trk <- bars[bars$track, , drop = FALSE]
  ser <- bars[!bars$track & !is.na(bars$serie), , drop = FALSE]
  ser$val <- vapply(seq_len(nrow(ser)), function(j) {
    t <- trk$len[abs(trk$x - ser$x[j]) < 1e-6]
    if (ser$len[j] <= 1e-9 && (!length(t) || all(t <= 1e-9))) NA_real_ else ser$len[j]
  }, 0)
  tx <- wjpr_drawn_texts(b)
  tx <- tx[!is.na(tx$label), , drop = FALSE]
  txt <- tx[tx$source == "layer", , drop = FALSE]
  txt$slot <- wjpr_pos_slot_of(txt$x, slots); txt <- txt[!is.na(txt$slot), , drop = FALSE]
  txt$serie <- vapply(seq_len(nrow(txt)), function(j) {
    s <- ser[ser$slot == txt$slot[j], , drop = FALSE]
    if (nrow(s)) s$serie[which.min(abs(s$x - txt$x[j]))] else NA_character_
  }, NA_character_)
  ax <- tx[tx$source == "axis-x", , drop = FALSE]; ax$slot <- wjpr_pos_slot_of(ax$x, slots)
  k <- match(slots$level, keys$level)
  val_of <- function(s, se) { v <- ser$val[ser$slot == s & ser$serie %in% se]; if (length(v)) v[1] else NA_real_ }
  txt_of <- function(s, se) paste(txt$label[txt$slot == s & txt$serie %in% se], collapse = " ")
  v1 <- vapply(slots$slot, val_of, 0, se = series[1]); v2 <- vapply(slots$slot, val_of, 0, se = series[2])
  out <- tibble(order = slots$slot, kind = coalesce(keys$kind[k], ""), grouping = coalesce(keys$grouping[k], ""),
                label = coalesce(keys$label[k], slots$level), frac = v1 / 100, frac2 = v2 / 100,
                text = vapply(slots$slot, txt_of, "", se = series[1]), text2 = vapply(slots$slot, txt_of, "", se = series[2]),
                label_drawn = vapply(slots$slot, function(s) paste(ax$label[ax$slot %in% s], collapse = " "), ""),
                grouping_drawn = NA_character_, val = v1, val2 = v2)
  out[order(out$order), , drop = FALSE]
}
wjpr_pb_drawn_fun <- function(keys, series, fills, track_fill) {
  force(keys); force(series); force(fills); force(track_fill)
  function(b) wjpr_pb_drawn(b, keys, series, fills, track_fill)
}

# bare = TRUE: solo wjp_bars con argumentos documentados (para medir la brecha sin complementos)
wjpr_paired_bars <- function(df, spec, bare = !wjpr_complementos(), ...) {
  wjpr_require()
  st <- ST_WIDE2                                     # pista a toda la columna: pair_pitch 21, pair_bar 8
  W <- spec$w_in * 72
  d <- wjpr_wb_long(df, spec)
  rows <- df %>% filter(kind == "row")
  wmax <- max(vapply(rows$label, text_width, 0, size = st$size_label))
  L <- label_column(list(rows), st)                  # columna de etiquetas propia (borde derecho = la más ancha + 2)
  x0 <- L + st$gap_label
  track <- W - x0 - st$gap_value - st$value_w        # la pista llega hasta la columna de valores
  d$lab_pos <- 100 + 100 * st$gap_value / track      # valores a gap_value pt del final de la pista
  w <- 2 * st$pair_bar / st$pair_pitch               # dos barras de pair_bar pt por fila de pair_pitch pt
  p <- wjpr_wb_bars(data = d, target = "v", grouping = "label", labels = "lab", colors = "serie",
                    cvec = stats::setNames(c(st$col$main, st$col$eu), levels(d$serie)), direction = "horizontal",
                    stacked = FALSE, lab_pos = "lab_pos", order = "order", width = w, ptheme = wjpr_theme(),
                    legend = TRUE)
  # corregido (revisión 4): lo dibujado por fila (wjpr_pb_drawn: barras, valores y categoría impresos en cada
  # posición), también sin complementos; antes solo lo medían el parecido y el multiconjunto de textos, que no ven dos
  # textos o dos categorías intercambiados
  drawn <- wjpr_pb_drawn_fun(tibble(level = rows$label, kind = rows$kind, grouping = rows$grouping, label = rows$label),
                             levels(d$serie), c(st$col$main, st$col$eu), st$col$track)
  if (bare) return(wjpr_meta(p, base = "wjp_bars", drawn = drawn))
  # complemento: wjp_bars fija position = "stack" en geom_col (stacked solo cambia el color del texto); falta un
  # argumento position = "dodge" (con su ancho y el orden de las series: el país arriba)
  dodge <- ggplot2::position_dodge(width = 2 * (st$pair_bar + 2) / st$pair_pitch, reverse = TRUE)
  # complemento (1.1.0): wjp_bars oculta la leyenda (show.legend = FALSE fijo); main la trae (show_legend)
  p <- wjpr_wb_legend_on(p)
  is_col <- wjpr_wb_is(p, "GeomCol"); is_txt <- wjpr_wb_is(p, "GeomText")
  p$layers[is_col] <- lapply(p$layers[is_col], wjpr_wb_layer, position = dodge, aes_params = list(linewidth = 0))
  # complemento: el texto de wjp_bars no sigue a las barras (sin position), tiene color, fontface "bold" y 10 pt
  # fijos; faltan label_position, label_colors (por serie), label_face, label_size y label_hjust
  p$layers[is_txt] <- lapply(p$layers[is_txt], wjpr_wb_layer, position = dodge,
                             aes_params = list(fontface = "bold.italic", size = st$size_value / ggplot2::.pt, hjust = 0),
                             drop_params = "colour", mapping = ggplot2::aes(y = lab_pos, colour = colors_var))
  # complemento: WJPr no dibuja la pista (barra de fondo al 100 %); falta un argumento track (y su color)
  p$layers <- c(list(wjpr_wb_track(w, dodge, st$col$track)), p$layers)
  p <- suppressMessages(p +
    ggplot2::scale_colour_manual(values = stats::setNames(c(st$col$main, st$col$eu_text), levels(d$serie)), guide = "none") +
    # complemento: wjp_bars fija limits = c(0, 110) y la expansión por defecto del eje de valores; falta ylim/expand
    ggplot2::scale_y_continuous(limits = c(0, 100 * (W - x0) / track), expand = c(0, 0)) +
    # complemento: sin argumento para el alto de fila ni el margen de la leyenda: expansión del eje de categorías
    ggplot2::scale_x_discrete(limits = rev, expand = ggplot2::expansion(add = c(0.5 + 4 / st$pair_pitch, 0.5 + 20 / st$pair_pitch))))
  # complemento: WJPr deja el eje de valores arriba con rejilla discontinua y las etiquetas a la izquierda (hjust 0
  # fijo); falta axis = FALSE / grid = FALSE / label_align = "right"
  p <- p + ggplot2::theme(
    axis.text.y = ggplot2::element_text(hjust = 1, size = st$size_label, colour = st$col$text, margin = ggplot2::margin(r = st$gap_label)),
    axis.text.x = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(0, 0, 0, L - wmax)) +
    # complemento: leyenda dentro del panel, arriba a la izquierda (main: arriba, fuera del panel, 11 pt negrita; un
    # argumento de posición "inside" y de estilo en wjp_legend_theme / wjp_bars lo haría innecesario)
    wjpr_wb_legend(st)
  wjpr_meta(wjpr_wb_font(p), base = "wjp_bars",
            complementos = c("lado a lado (dodge, país arriba)", wjpr_wb_legend_comp("dentro del panel"),
                             "texto por serie (posición, color, semibold cursiva 9 pt)", "pista al 100 %",
                             "escala de valores sin expansión", "alto de fila (expansión discreta)",
                             "tema sin eje ni rejilla, etiquetas a la derecha", wjpr_wb_font_comp()),
            drawn = drawn)
}

## Bloque wjpr columns — figuras 30, 37 -----------------------------------------------------------------------------
## wjp_bars vertical con país y UE lado a lado, alturas relativas al máximo de las dos series (la más alta = col_h),
## valores encima de cada columna, etiquetas debajo y línea base (chart_columns); usa los auxiliares wjpr_wb_*.
## Prueba (galería, 2026-10-23, con y sin SRC_REPORTS_LIB): f1 0.998 / 1.000 con 1.1.0@382aee9 (7 complementos) y
## con main a46c831 (6), textos iguales, 0 dudosos; sin complementos 0.675 / 0.706 (1.1.0) y 0.538 / 0.601 (main, con
## su leyenda arriba; sin ella 0.675 / 0.706). La 30 desde la referencia usa value redondeado (wjpr_ref_df descarta
## frac): alturas a ≤ 3 pt de las publicadas; con value = frac·100 la misma función da 1.000 en las dos builds.

wjpr_columns <- function(df, spec, bare = !wjpr_complementos(), ...) {
  wjpr_require()
  st <- ST_COLS                                      # col_w 46, col_h 180
  W <- spec$w_in * 72; H <- spec$h_in * 72
  d <- wjpr_wb_long(df, spec)
  n <- length(unique(d$label)); m <- 6; gap <- 4
  gw <- (W - 2 * m) / n                              # ancho de cada grupo
  vmax <- max(d$v, na.rm = TRUE)
  yb <- 24 + st$size_value + 6 + st$col_h            # línea base (pt desde arriba): la más alta llega a 39 pt
  d$lab_pos <- d$v + vmax * 4 / st$col_h             # valor a 4 pt de la cima de su columna
  w <- 2 * st$col_w / gw
  p <- wjpr_wb_bars(data = d, target = "v", grouping = "label", labels = "lab", colors = "serie",
                    cvec = stats::setNames(c(st$col$main, st$col$eu), levels(d$serie)), direction = "vertical",
                    stacked = FALSE, lab_pos = "lab_pos", order = "order", width = w, ptheme = wjpr_theme(),
                    legend = TRUE)
  if (bare) return(wjpr_meta(p, base = "wjp_bars"))
  # complemento: wjp_bars fija position = "stack" en geom_col; falta un argumento position = "dodge" (y su ancho)
  dodge <- ggplot2::position_dodge(width = 2 * (st$col_w + gap) / gw)
  # complemento (1.1.0): wjp_bars oculta la leyenda (show.legend = FALSE fijo); main la trae (show_legend)
  p <- wjpr_wb_legend_on(p)
  is_col <- wjpr_wb_is(p, "GeomCol"); is_txt <- wjpr_wb_is(p, "GeomText")
  p$layers[is_col] <- lapply(p$layers[is_col], wjpr_wb_layer, position = dodge, aes_params = list(linewidth = 0))
  # complemento: el texto no sigue a su columna ni se apoya encima (vjust), y color, fontface y tamaño son fijos;
  # faltan label_position, label_vjust, label_colors, label_face y label_size
  p$layers[is_txt] <- lapply(p$layers[is_txt], wjpr_wb_layer, position = dodge,
                             aes_params = list(fontface = "bold.italic", size = st$size_value / ggplot2::.pt, vjust = 0),
                             drop_params = "colour", mapping = ggplot2::aes(y = lab_pos, colour = colors_var))
  # etiqueta de categoría: línea base a 6 + 0.95·size bajo la línea base (grid apoya el tope de la ascendente)
  mt <- 6 + 0.95 * st$size_label - WJPR_WB_ASC * st$size_label
  wrapped <- vapply(unique(as.character(d$label)), function(s) paste(wrap_text(s, st$size_label, gw - 10), collapse = "\n"), "")
  nl <- max(lengths(strsplit(wrapped, "\n", fixed = TRUE)))
  text_h <- (WJPR_WB_TEXT_H + (nl - 1) * 1.22) * st$size_label
  p <- suppressMessages(p +
    ggplot2::scale_colour_manual(values = stats::setNames(c(st$col$main, st$col$eu_text), levels(d$serie)), guide = "none") +
    # complemento: wjp_bars fija limits = c(0, 110) y cortes cada 20 %; falta escalar al máximo (ylim / scale_max)
    ggplot2::scale_y_continuous(limits = c(0, vmax * yb / st$col_h), expand = c(0, 0)) +
    # complemento: wjp_bars no ajusta las etiquetas largas ni deja fijar el ancho de grupo; falta label_wrap / expand
    ggplot2::scale_x_discrete(labels = function(x) unname(wrapped[x]), expand = ggplot2::expansion(add = 0.5)))
  # complemento: WJPr dibuja eje de valores, rejilla discontinua y el título '% of respondents' fijo, sin línea base;
  # falta axis = FALSE / grid = FALSE / ylab / baseline = TRUE
  p <- p + ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = st$size_label, colour = st$col$text, vjust = 1, lineheight = WJPR_WB_LH,
                                        margin = ggplot2::margin(t = mt)),
    axis.text.y = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(), axis.line.x = ggplot2::element_line(colour = st$col$axis, linewidth = wjpr_wb_lw(0.6)),
    panel.grid.major.x = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(0, m, max(0, H - yb - mt - text_h), m)) +
    # complemento: leyenda dentro del panel, arriba a la izquierda (main: arriba, fuera del panel, 11 pt negrita; un
    # argumento de posición "inside" y de estilo en wjp_legend_theme / wjp_bars lo haría innecesario)
    wjpr_wb_legend(st, top = 3)
  wjpr_meta(wjpr_wb_font(p), base = "wjp_bars",
            complementos = c("lado a lado (dodge)", wjpr_wb_legend_comp("dentro del panel"),
                             "texto por serie (posición, color, semibold cursiva 9 pt)", "escala al máximo sin expansión",
                             "etiquetas con ajuste de línea", "tema sin eje ni rejilla, con línea base", wjpr_wb_font_comp()))
}

## Bloque wjpr severity_bars — figuras 7 ----------------------------------------------------------------------------
## wjp_bars vertical con las dos series lado a lado por nivel de severidad 0-10 y la línea de corte en 3.5 (entre 3 y
## 4), como F03.R:28-45: geom_col dodge 0.8 width 0.75 y theme_minimal('inter') con textos a 13 sobre un lienzo × 1.3
## que se reduce a w_in. Aquí todo va en pt finales (pt del original × k = 10 / base_pt = 1/1.3: texto a 10 pt). Usa wjpr_wb_layer().
## Prueba (galería, 2026-10-23, con y sin SRC_REPORTS_LIB): f1 0.999 con 1.1.0@382aee9 (7 complementos) y con main
## a46c831 (6), textos iguales, 0 dudosos; sin complementos 0.380 (1.1.0) y 0.266 (main, con su leyenda a la
## izquierda; sin ella 0.380); idéntica al F03 original salvo los guiones de la línea de corte (svglite no escala el
## guion por debajo de lwd 1: 4 pt, no 3.5). base_pt (2026-10-25): k = 10 / base_pt con el 13 del manifiesto da el mismo
## SVG byte a byte que el k = 1 / 1.3 fijo de antes.
## Lo dibujado por fila (revisión 4, G1; contrato drawn de wjpr_meta, también con bare = TRUE): wjpr_sev_drawn, una
## fila por barra con val = alto, label_drawn = etiqueta del eje y grouping_drawn = texto de la leyenda para su relleno;
## la línea de corte (diseño fijo, sin fila) se comprueba aparte: si falta, «fig 07 mensaje: línea de corte entre 3 y 4
## ausente del dibujo…» en el log de figures, sin cambiar el estado (para que lo cambie: pendiente 4 del INFORME de G1).
## Prueba de lo dibujado por fila (revisión 4, 2026-10-27, G1: SCRATCH/fix4/G1/pruebas/iny_g1.sh, --stage all --figs 07
## --strict): barra de severidad 5 +1 pp (dibujo · val 12.21→13.21, exit 1), leyenda Hungary ↔ EU Average (texto ·
## grouping_drawn de las 22 barras, exit 1), niveles 3 ↔ 4 del eje (texto · label_drawn, exit 1), línea de corte
## quitada (ok con el mensaje en el log, exit 0); sin inyección 22 filas sin diferencias (67/67 en drawn_ref_rows); sin
## complementos (1.1.0) grouping_drawn "" (WJPr no dibuja la leyenda). Sin inyección: Hungría 10/10 con las dos builds
## (SVG y PNG de la 7 = ROOT), Polonia y Malta ok en figures sin el mensaje del corte, galería «22 filas ok».

WJPR_SEV_CUT <- 3.5   # umbral de no trivial (F03.R:28-45): la línea de corte va entre las severidades 3 y 4

# Lo dibujado por fila de la severidad (contrato drawn de wjpr_meta, revisión 4), leído de b = ggplot_build() del
# gráfico final: una fila por barra, de izquierda a derecha (nivel del eje, wjpr_pos_slots, y dentro de él la x de la
# barra esquivada). kind "bar", label = el nivel de severidad que ocupa esa posición del eje y grouping = la serie que
# ocupa ese lugar del par (series[1] a la izquierda: el país; series[2] la UE, el orden del dodge); val = el alto de la
# barra (en %, las unidades de df$value; una barra de alto 0 no se ve y no codifica nada: NA, el valor suprimido que el
# adaptador dibuja con alto 0); label_drawn = la etiqueta impresa en el eje en esa posición; grouping_drawn = el texto
# de la leyenda impresa para el relleno de la barra ("" si la leyenda no lo nombra); text = los textos impresos en la
# posición, de la barra más cercana ("" en la 7). Una barra más alta, dos niveles del eje o las etiquetas de la leyenda
# intercambiados se ven aquí. La línea de corte (diseño fijo, sin fila en df) la comprueba wjpr_sev_cut_missing, que
# el adaptador declara en wjpr_meta(…, marcas = )
wjpr_sev_drawn <- function(b, series) {
  slots <- wjpr_pos_slots(b, "x")
  lg <- wjpr_legend_drawn(b)
  num <- function(d, nm) if (nm %in% names(d)) suppressWarnings(as.numeric(d[[nm]])) else rep(NA_real_, nrow(d))
  ib <- unname(which(vapply(b$plot$layers, function(l) inherits(l$geom, c("GeomCol", "GeomBar", "GeomRect")), logical(1))))
  bars <- bind_rows(tibble(x = numeric(), slot = integer(), len = numeric(), fill = character()), lapply(ib, function(i) {
    d <- as.data.frame(b$data[[i]])
    if (!all(c("x", "ymin", "ymax", "fill") %in% names(d))) return(NULL)
    tibble(x = num(d, "x"), slot = wjpr_pos_slot_of(d$x, slots), len = abs(num(d, "ymax") - num(d, "ymin")),
           fill = as.character(d$fill))
  }))
  bars <- bars[!is.na(bars$slot) & is.finite(bars$len), , drop = FALSE]
  bars <- bars[order(bars$slot, bars$x), , drop = FALSE]
  kpos <- stats::ave(seq_len(nrow(bars)), bars$slot, FUN = seq_along)
  tx <- wjpr_drawn_texts(b)
  tx <- tx[!is.na(tx$label), , drop = FALSE]
  txt <- tx[tx$source == "layer", , drop = FALSE]
  txt$slot <- wjpr_pos_slot_of(txt$x, slots); txt <- txt[!is.na(txt$slot), , drop = FALSE]
  txt$bar <- vapply(seq_len(nrow(txt)), function(j) {
    k <- which(bars$slot == txt$slot[j]); if (length(k)) k[which.min(abs(bars$x[k] - txt$x[j]))] else NA_integer_
  }, NA_integer_)
  ax <- tx[tx$source == "axis-x", , drop = FALSE]; ax$slot <- wjpr_pos_slot_of(ax$x, slots)
  lev <- slots$level[match(bars$slot, slots$slot)]
  tibble(order = seq_len(nrow(bars)), kind = "bar", grouping = ifelse(kpos <= length(series), series[pmin(kpos, length(series))], ""),
         label = lev, frac = NA_real_, frac2 = NA_real_,
         text = vapply(seq_len(nrow(bars)), function(j) paste(txt$label[txt$bar %in% j], collapse = " "), ""), text2 = "",
         label_drawn = vapply(bars$slot, function(s) paste(ax$label[ax$slot %in% s], collapse = " "), ""),
         grouping_drawn = coalesce(wjpr_series_of(bars$fill, lg, "fill"), ""),
         val = ifelse(bars$len <= 1e-9, NA_real_, bars$len), val2 = NA_real_)
}
wjpr_sev_drawn_fun <- function(series) {
  force(series)
  function(b) wjpr_sev_drawn(b, series)
}

# corregido (revisión 4): la línea de corte, marca fija del diseño sin fila en df, como marca de wjpr_meta(…, marcas =)
# (figures: estado "dibujo" si falta); antes su ausencia solo iba al log y la figura seguía ok (parecido_local 0.118,
# igual que sin error). cut = c(nivel, nivel) entre los que va: character() si hay una geom_vline visible (color
# distinto del fondo, ancho > 0, tipo de línea no blank) en la posición media de los dos niveles del eje; si no, su
# descripción
wjpr_sev_cut_missing <- function(b, cut) {
  slots <- wjpr_pos_slots(b, "x")
  num <- function(d, nm) if (nm %in% names(d)) suppressWarnings(as.numeric(d[[nm]])) else rep(NA_real_, nrow(d))
  at <- mean(slots$pos[match(as.character(cut), slots$level)])
  th <- ggplot2::complete_theme(b$plot$theme)
  bg <- wjpr_theme_fill(th, "panel.background", wjpr_theme_fill(th, "plot.background"))
  iv <- which(vapply(b$plot$layers, function(l) inherits(l$geom, "GeomVline"), logical(1)))
  seen <- any(vapply(iv, function(i) {
    d <- as.data.frame(b$data[[i]]); if (!nrow(d)) return(FALSE)
    tc <- wjpr_rgba(if ("colour" %in% names(d)) d$colour else "black")
    vis <- apply(abs(tc[1:3, , drop = FALSE] - bg), 2, max) * tc[4, ] * coalesce(num(d, "alpha"), 1) > WJPR_VISIBLE_MIN
    lt <- if ("linetype" %in% names(d)) as.character(d$linetype) else rep("solid", nrow(d))
    any(is.finite(num(d, "xintercept")) & abs(num(d, "xintercept") - at) < 1e-6 & coalesce(num(d, "linewidth"), 0.5) > 0 &
          vis & !lt %in% c("blank", "0"))
  }, logical(1)))
  if (isTRUE(seen)) character() else paste0("línea de corte entre ", cut[1], " y ", cut[2])
}
wjpr_sev_marcas_fun <- function(cut) { force(cut); function(b) wjpr_sev_cut_missing(b, cut) }

wjpr_severity_bars <- function(df, spec, bare = !wjpr_complementos(), ...) {
  wjpr_require()
  # corregido: k sale de base_pt del manifiesto (13 = textos del original sobre el lienzo × 1.3), como en los adaptadores
  # de 3, 6 y 29; antes 1 / 1.3 fijo y la columna no tenía efecto en la 7. Sin base_pt, 13 (lo publicado)
  st <- STD; k <- 10 / (if (is.null(spec$base_pt) || is.na(spec$base_pt)) 13 else spec$base_pt)
  lv <- c(spec$country, "EU Average")
  d <- df %>% filter(kind == "bar") %>%
    mutate(serie = factor(ifelse(grouping == lv[2], lv[2], lv[1]), levels = lv), sev = as.numeric(label),
           value = coalesce(value, 0), label = factor(label, levels = unique(label[order(sev)])))
  p <- wjpr_wb_bars(data = d, target = "value", grouping = "label", colors = "serie",
                    cvec = stats::setNames(c(st$col$main, st$col$eu), lv), direction = "vertical", stacked = FALSE,
                    order = "sev", width = 0.75, ptheme = wjpr_theme(), legend = TRUE)
  # corregido (revisión 4): lo dibujado por fila (wjpr_sev_drawn: alto, nivel del eje y serie de la leyenda de cada
  # barra), también sin complementos; antes la 7 no tenía textos que comparar y solo la medía el parecido, que no ve
  # una barra 1 pp más alta, dos niveles del eje o la leyenda intercambiados
  if (bare) return(wjpr_meta(p, base = "wjp_bars", drawn = wjpr_sev_drawn_fun(lv)))
  # complemento: wjp_bars fija position = "stack" en geom_col; falta un argumento position = "dodge" (y su ancho)
  dodge <- ggplot2::position_dodge(width = 0.8)
  # complemento (1.1.0): wjp_bars oculta la leyenda (show.legend = FALSE fijo); main la trae (show_legend).
  # linewidth: el de geom_col por defecto (0.5 mm) × k; sin contorno solo reduce el cuadro de la leyenda, como en el
  # original
  p <- wjpr_wb_legend_on(p)
  is_col <- wjpr_wb_is(p, "GeomCol")
  p$layers[is_col] <- lapply(p$layers[is_col], wjpr_wb_layer, position = dodge, aes_params = list(linewidth = 0.5 * k))
  # complemento: WJPr no tiene líneas de referencia; falta un argumento ref_lines (posición, tipo, color, grosor)
  cut <- WJPR_SEV_CUT - min(d$sev) + 1               # 3.5 en la escala discreta (posición 1 = la severidad mínima)
  p$layers <- c(p$layers, list(ggplot2::geom_vline(xintercept = cut, linetype = "dashed", colour = st$col$text,
                                                   linewidth = 0.4 * k)))
  ext <- 0.8 / 4 + 0.75 / 4                          # media barra más medio desplazamiento: borde de las barras
  p <- suppressMessages(p +
    # complemento: wjp_bars fija limits = c(0, 110) y cortes cada 20 %; falta ylim / breaks / expand_mult
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), expand = ggplot2::expansion(mult = c(0, 0.08))) +
    # complemento: el eje de categorías de WJPr expande 0.6; el original es continuo (5 % del rango de las barras)
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = ext + 0.05 * (max(d$sev) - min(d$sev) + 2 * ext))))
  # complemento: WJPr fija rejilla discontinua #d1cfd1, '% of respondents', textos de 10 pt en #524F4C y la leyenda
  # (1.1.0: ninguna; main, wjp_legend_theme: a la izquierda, 11 pt negrita, clave de 1.4 cm, márgenes 4 / 20 pt);
  # se lleva el tema de F03 (theme_minimal base 11, textos 13, clave 1.2 líneas = 17.28 pt) a pt finales
  p <- p + ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = 13 * k, colour = st$col$text, vjust = 1, margin = ggplot2::margin(t = 4.95 * k)),
    axis.text.y = ggplot2::element_text(size = 13 * k, colour = st$col$text, hjust = 1, margin = ggplot2::margin(r = 4.95 * k)),
    axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(colour = "#eeeeee", linewidth = 0.5 * k, linetype = "solid"),
    legend.position = "top", legend.justification = "center", legend.direction = "horizontal",
    legend.title = ggplot2::element_blank(), legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 13 * k, colour = st$col$text, face = "plain",
                                        margin = ggplot2::margin(l = 0.5 * 13 * k)),
    legend.key.size = grid::unit(17.28 * k, "pt"), legend.key.width = grid::unit(17.28 * k, "pt"),
    legend.key.spacing.x = grid::unit(5.5 * k, "pt"),
    legend.margin = ggplot2::margin(5.5 * k, 5.5 * k, 5.5 * k, 5.5 * k), legend.box.margin = ggplot2::margin(0, 0, 0, 0),
    legend.box.spacing = grid::unit(11 * k, "pt"),
    plot.margin = ggplot2::margin(5.5 * k, 5.5 * k, 5.5 * k, 5.5 * k))
  wjpr_meta(wjpr_wb_font(p), base = "wjp_bars",
            complementos = c("lado a lado (dodge)", wjpr_wb_legend_comp("arriba, centrada"), "línea de corte en 3.5",
                             "escala y en % sin límite fijo", "expansión del eje x como el original",
                             "tema de F03 (rejilla, leyenda, textos 10 pt)", wjpr_wb_font_comp()),
            drawn = wjpr_sev_drawn_fun(lv),
            marcas = wjpr_sev_marcas_fun(as.character(c(floor(WJPR_SEV_CUT), ceiling(WJPR_SEV_CUT)))))
}
## Marcas fijas (revisión 4, 2026-10-27, integración): sin la línea de corte, --figs 07 --strict → figures «dibujo ·
## marca fija ausente: línea de corte entre 3 y 4», verify fallo, exit 1 (con G1 solo iba al log); con ella, ok.

## Bloque wjpr funnel — figuras 8, 34 -------------------------------------------------------------------------------
## WJPr no tiene embudo: wjp_bars horizontal apilada (su position = "stack" fija) con un tramo transparente antes de
## cada barra para centrarla (ancho = value / máximo) y los valores en blanco que da stacked = TRUE en el centro
## (lab_pos = 50); complementos: conectores, tipografía del valor, etiquetas con salto de línea a la derecha y la
## geometría de chart_funnel (barra 40 pt, hueco 18 pt, 4 pt arriba y abajo). Usa wjpr_wb_layer().
## Prueba (galería, 2026-10-23, con y sin SRC_REPORTS_LIB): f1 1.000 / 1.000 con 1.1.0@382aee9 (8 complementos) y
## con main a46c831 (7: fuente nativa), textos iguales, 0 dudosos; sin complementos 0.645 / 0.650 en las dos.

wjpr_funnel <- function(df, spec, bare = !wjpr_complementos(), ...) {
  wjpr_require()
  st <- ST_FUNNEL                                    # funnel_bar 40, funnel_gap 18, funnel_value 11, funnel_label_w 190
  s <- df %>% filter(kind == "stage") %>% arrange(order) %>% mutate(frac = coalesce(value / max(value, na.rm = TRUE), 0))
  # etiquetas: el salto de línea que trae la etapa o, si no trae, ajuste a funnel_label_w (como chart_funnel)
  lines <- lapply(s$label, function(l) if (grepl("\n", l)) strsplit(l, "\n", fixed = TRUE)[[1]] else wrap_text(l, st$size_label, st$funnel_label_w))
  s$label <- vapply(lines, paste, "", collapse = "\n")
  wmax <- max(unlist(lapply(lines, function(ls) vapply(ls, text_width, 0, size = st$size_label))))
  L <- ceiling(wmax) + 2                             # borde derecho de las etiquetas; las barras empiezan en L + 16
  # complemento: wjp_bars no centra las barras (falta align = "center"): tramo transparente (pad) apilado antes de
  # cada barra, que queda centrada en 50
  d <- bind_rows(s %>% transmute(order, label, tramo = "bar", v = 100 * frac, lab = coalesce(text, "")),
                 s %>% transmute(order, label, tramo = "pad", v = 50 * (1 - frac), lab = "")) %>%
    mutate(tramo = factor(tramo, levels = c("bar", "pad")), lab_pos = 50)
  pitch <- st$funnel_bar + st$funnel_gap
  p <- WJPr::wjp_bars(data = d, target = "v", grouping = "label", labels = "lab", colors = "tramo",
                      cvec = c(bar = st$col$main, pad = NA), direction = "horizontal", stacked = TRUE,
                      lab_pos = "lab_pos", order = "order", width = st$funnel_bar / pitch, ptheme = wjpr_theme())
  if (bare) return(wjpr_meta(p, base = "wjp_bars"))
  # complemento: el valor de wjp_bars va en negrita a 10 pt fijos; faltan label_face y label_size (11 pt semibold cursiva)
  is_txt <- wjpr_wb_is(p, "GeomText")
  p$layers[is_txt] <- lapply(p$layers[is_txt], wjpr_wb_layer,
                             aes_params = list(fontface = "bold.italic", size = st$funnel_value / ggplot2::.pt))
  # complemento: WJPr no tiene embudo ni conectores entre barras; falta wjp_funnel() (o connectors = TRUE)
  n <- nrow(s); hb <- st$funnel_bar / pitch / 2
  pos <- n + 1 - seq_len(n)                          # posición en la escala discreta (limits = rev: la primera arriba)
  if (n > 1) {
    conn <- bind_rows(lapply(seq_len(n - 1), function(i) tibble(
      id = i, x = c(pos[i] - hb, pos[i] - hb, pos[i + 1] + hb, pos[i + 1] + hb),
      y = c(50 * (1 - s$frac[i]), 50 * (1 + s$frac[i]), 50 * (1 + s$frac[i + 1]), 50 * (1 - s$frac[i + 1])))))
    p$layers <- c(list(ggplot2::geom_polygon(data = conn, ggplot2::aes(x = x, y = y, group = id), inherit.aes = FALSE,
                                             fill = st$col$eu, alpha = 0.45, colour = NA)), p$layers)
  }
  p <- suppressMessages(p +
    # complemento: wjp_bars fija limits = c(0, 110) y la expansión; la barra más ancha debe llenar el panel (ylim/expand)
    ggplot2::scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    # complemento: alto de barra y hueco en pt sin argumento: expansión discreta (4 pt arriba y abajo)
    ggplot2::scale_x_discrete(limits = rev, expand = ggplot2::expansion(add = hb + 4 / pitch)))
  # complemento: WJPr deja el eje de valores arriba con rejilla y las etiquetas a la izquierda (hjust 0) con el
  # interlineado por defecto; falta axis = FALSE / grid = FALSE / label_align = "right" / label_lineheight
  p <- p + ggplot2::theme(
    axis.text.y = ggplot2::element_text(hjust = 1, size = st$size_label, colour = st$col$text, lineheight = WJPR_WB_LH,
                                        margin = ggplot2::margin(r = 16)),
    axis.text.x = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(0, 4, 0, L - wmax))
  wjpr_meta(wjpr_wb_font(p), base = "wjp_bars",
            complementos = c("barra centrada (tramo transparente apilado)", "conectores entre etapas", "valor 11 pt semibold cursiva",
                             "escala 0-100 sin expansión", "alto de barra y hueco (expansión discreta)",
                             "etiquetas a la derecha con salto de línea", "tema sin eje ni rejilla", wjpr_wb_font_comp()))
}


## Bloque wjpr cards — figuras 32, 41, 42 ----------------------------------------------------------------------------
## WJPr 1.1.0 no tiene función de tarjetas (ni de mapa de calor): la base es un ggplot genérico de celdas —
## geom_tile(fill = value) + geom_text(text) + geom_text(text2) con la familia y el tamaño que WJPr fija en sus capas
## (WJP_theme()$axis.text.x$family, 3.514598 mm = 10 pt), escala secuencial entre el gris claro de WJPr ("#e5e8e8",
## colores por defecto de wjp_groupbars) y wjp_palette()[1], filas y columnas como texto de eje (WJP_theme()) — y los
## complementos lo llevan al diseño de chart_scorecards (report_charts.R:393-431, ST_CARDS): tarjetas de 48 pt con 6 pt
## de separación y esquinas de 3 pt, columna de etiquetas de 170 pt, color por figura (lo, hi del df) con la mezcla RGB
## card_low → main, texto blanco sobre fondo oscuro, valor 16 pt bold y delta 9 pt cursiva. Filas = df$grouping y
## columnas = df$label en el orden de df$order; la tarjeta de referencia es la que tiene text2 == "".
## complementos = FALSE devuelve la base sin complementos (para medir cuánto aporta WJPr; la galería lo lee de
## getOption("wjpr.complementos")).

# Familia que WJPr fija en sus capas y temas (hoy "Lato Full"); la base la usa para comportarse como WJPr
wjpr_wjp_family <- function() WJPr::WJP_theme()$axis.text.x$family %||% "Lato Full"

# Las capas que reemplazan una escala ya presente se añaden con suppressMessages() (ggplot2 avisa "Scale for … is
# already present"; el reemplazo es intencional)

# Mezcla RGB de dos colores (t en 0-1), la de mix_col() del diseño (report_charts.R:387-390)
wjpr_mix_rgb <- function(c1, c2, t) {
  a <- grDevices::col2rgb(c1)[, 1]; b <- grDevices::col2rgb(c2)[, 1]
  vapply(t, function(u) sprintf("#%02X%02X%02X", round(a[1] + (b[1] - a[1]) * u), round(a[2] + (b[2] - a[2]) * u),
                                round(a[3] + (b[3] - a[3]) * u)), "")
}

# Familia con la cara Bold Italic (700) de Inter Tight: la familia "inter" de setup_fonts() registra bold.italic como
# SemiBoldItalic (600, la de las figuras ggplot publicadas), pero los encabezados de las figuras svg_r (tarjetas,
# sankey) son 700 cursiva. svglite escribe el peso real del archivo (font-weight: bold) y outline_svg_text() usa
# entonces InterTight-BoldItalic. Se registra una vez (systemfonts, global) y se aplica después de wjpr_font().
wjpr_family_bi700 <- function(name = "inter_bi700") {
  if (!name %in% systemfonts::registry_fonts()$family) {
    f <- font_file(700, TRUE)
    systemfonts::register_font(name = name, plain = f, bold = f, italic = f, bolditalic = f)
  }
  name
}

# Polígonos de rectángulos con esquinas redondeadas (x0, y0 = esquina superior izquierda en pt, y hacia abajo); una
# fila por vértice con id = el índice del rectángulo (geom_polygon(aes(group = id)))
wjpr_round_rect_df <- function(x0, y0, w, h, r, n = 6) {
  bind_rows(lapply(seq_along(x0), function(i) {
    ri <- min(r, w[i] / 2, h[i] / 2)
    arc <- function(cx, cy, a0) { a <- seq(a0, a0 + pi / 2, length.out = n); tibble(x = cx + ri * cos(a), y = cy + ri * sin(a)) }
    bind_rows(arc(x0[i] + w[i] - ri, y0[i] + h[i] - ri, 0), arc(x0[i] + ri, y0[i] + h[i] - ri, pi / 2),
              arc(x0[i] + ri, y0[i] + ri, pi), arc(x0[i] + w[i] - ri, y0[i] + ri, 3 * pi / 2)) %>% mutate(id = i)
  }))
}

wjpr_cards <- function(df, spec, complementos = wjpr_complementos(), st = ST_CARDS) {
  wjpr_require()
  df <- df[order(df$order), ]
  rows <- unique(df$grouping); cols <- unique(df$label)
  d <- df %>% mutate(i = match(grouping, rows), j = match(label, cols), is_ref = !nzchar(coalesce(text2, "")))
  fam <- wjpr_wjp_family(); pal <- WJPr::wjp_palette()
  base <- "WJP_theme() + wjp_palette() (sin función de tarjetas)"
  # base: celdas genéricas (una por fila × columna), valor y delta centrados, escala secuencial de WJPr
  p <- ggplot(d, aes(x = j, y = i)) +
    geom_tile(aes(fill = value), color = "white") +
    geom_text(aes(label = text), family = fam, fontface = "bold", size = 3.514598, vjust = 0) +
    geom_text(aes(label = text2), family = fam, size = 3.514598, vjust = 1.2) +
    scale_fill_gradient(low = "#e5e8e8", high = pal[1], guide = "none") +
    scale_x_continuous(breaks = seq_along(cols), labels = cols, position = "top") +
    scale_y_reverse(breaks = seq_along(rows), labels = rows) +
    labs(x = NULL, y = NULL) + WJPr::WJP_theme()
  if (!isTRUE(complementos)) return(wjpr_meta(p, base = base))

  W <- spec$w_in * 72; H <- spec$h_in * 72
  nr <- length(rows); nc <- length(cols)
  lw <- st$card_label_w; cg <- 6; ch <- st$card_h; ls <- st$card_label
  cw <- (W - lw - 12 - cg * (nc - 1)) / nc
  heads <- lapply(cols, wrap_text, size = ls, max_w = cw - 8, weight = 700, italic = TRUE)
  hstep <- ls * 1.2; nl <- max(lengths(heads)); top <- 6 + nl * hstep + 6
  lo <- df$lo[1] %||% NA_real_; hi <- df$hi[1] %||% NA_real_
  if (is.na(lo) || is.na(hi)) { lo <- min(round(df$value)); hi <- max(round(df$value)) }
  d <- d %>% mutate(x0 = lw + 12 + (j - 1) * (cw + cg), y0 = top + (i - 1) * (ch + cg), xc = x0 + cw / 2, yc = y0 + ch / 2,
                    t = 0.12 + 0.88 * (round(value) - .env$lo) / (if (.env$hi > .env$lo) .env$hi - .env$lo else 1), dark = t > 0.55,
                    fill = wjpr_mix_rgb(st$col$card_low, st$col$main, t),
                    col_val = if_else(dark, "#FFFFFF", st$col$text), col_delta = if_else(dark, "#FFFFFF", st$col$delta),
                    y_val = if_else(is_ref, yc + 0.36 * st$card_value, yc + 2), y_delta = yc + 2 + st$card_delta + 4)
  comp <- character()
  # complemento: WJPr no tiene geom de tarjeta: geom_tile no hace esquinas redondeadas ni separación fija en pt; las
  # tarjetas son polígonos de esquina 3 pt en la rejilla del diseño (170 pt de etiquetas + 12, columnas de ancho
  # (W − 182 − 6·(nc − 1)) / nc, filas de 48 pt cada 54 pt)
  rr <- wjpr_round_rect_df(d$x0, d$y0, rep(cw, nrow(d)), rep(ch, nrow(d)), 3) %>% mutate(fill = d$fill[id])
  p$layers[[1]] <- geom_polygon(data = rr, aes(x = x, y = y, group = id, fill = fill), inherit.aes = FALSE)
  comp <- c(comp, "tarjetas redondeadas en la rejilla del diseño")
  # complemento: WJPr no tiene escala secuencial con los colores del reporte (card_low #ECECF4 → main #575796, mezcla
  # RGB) ni un rango por figura (lo, hi); se usa el color calculado (identidad)
  p <- suppressMessages(p + scale_fill_identity()); comp <- c(comp, "escala de color del reporte (lo/hi por figura)")
  # complemento: posición y estilo de los textos de la tarjeta (valor 16 pt bold, centrado en la tarjeta de referencia
  # y 2 pt bajo el centro en las demás; delta 9 pt cursiva debajo) y color según el fondo (blanco si t > 0.55)
  p$layers[[2]] <- geom_text(data = d, aes(x = xc, y = y_val, label = text, colour = col_val), family = fam,
                             fontface = "bold", size = st$card_value / ggplot2::.pt, vjust = 0, inherit.aes = FALSE)
  p$layers[[3]] <- geom_text(data = d %>% filter(!is_ref), aes(x = xc, y = y_delta, label = text2, colour = col_delta),
                             family = fam, fontface = "italic", size = st$card_delta / ggplot2::.pt, vjust = 0, inherit.aes = FALSE)
  p <- suppressMessages(p + scale_colour_identity()); comp <- c(comp, "valor y delta: tamaño, posición y color según el fondo")
  # complemento: ejes en coordenadas del diseño (pt): columnas centradas sobre las tarjetas, filas en su centro, panel
  # = la zona de tarjetas
  yb <- top + (seq_len(nr) - 1) * (ch + cg) + ch / 2
  xb <- lw + 12 + (seq_len(nc) - 1) * (cw + cg) + cw / 2
  p <- suppressMessages(p + scale_x_continuous(breaks = xb, labels = vapply(heads, paste, "", collapse = "\n"), position = "top",
                              limits = c(lw + 12, W), expand = c(0, 0), oob = scales::oob_keep) +
    scale_y_reverse(breaks = yb, labels = vapply(rows, function(r) paste(wrap_text(r, ls, lw - 4), collapse = "\n"), ""),
                    limits = c(H - 4, top), expand = c(0, 0), oob = scales::oob_keep))
  comp <- c(comp, "ejes en la rejilla del diseño")
  # complemento: WJP_theme() da rejilla discontinua (panel.grid.major explícito: theme(panel.grid = element_blank())
  # no la quita), texto de eje 10 pt #524F4C y márgenes 0: aquí sin rejilla, etiquetas de fila 11 pt #1A1A1A
  # alineadas a la derecha a 12 pt de las tarjetas (margen izquierdo = 170 − la más ancha, medida con text_width) y
  # encabezados 11 pt cursiva main con paso de línea 13.2 pt (lineheight 1 × 1.2 de las métricas de Inter Tight) y la
  # última línea a 7 pt de las tarjetas (margen superior = top − alto del bloque − 7; alto = 0.9418·tamaño + pasos)
  maxw <- max(vapply(rows, function(r) max(vapply(wrap_text(r, ls, lw - 4), text_width, 0, size = ls)), 0))
  p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks.length = unit(0, "pt"),
                 axis.text.y = element_text(size = ls, colour = st$col$text, hjust = 1, lineheight = 1.22 / 1.2, margin = margin(r = 12, unit = "bigpts")),
                 axis.text.x.top = element_text(size = ls, colour = st$col$main, face = "bold.italic", lineheight = 1,
                                                vjust = 0, margin = margin(b = 7, unit = "bigpts")),
                 plot.margin = margin(top - (0.9418 * ls + (nl - 1) * 1.2 * ls) - 7, 0, 4, lw - maxw, unit = "bigpts"))
  comp <- c(comp, "tema: sin rejilla; etiquetas 11 pt y encabezados 11 pt bold cursiva")
  # complemento: WJPr fija "Lato Full" en capas y tema, sin argumento para cambiarla
  p <- wjpr_font(p); comp <- c(comp, "fuente Inter Tight (wjpr_font)")
  # complemento: encabezados en Bold Italic 700 como el diseño (WJPr no tiene argumento de peso; "bold.italic" de la
  # familia "inter" es 600)
  p <- p + theme(axis.text.x.top = element_text(family = wjpr_family_bi700(), face = "italic"))
  comp <- c(comp, "encabezados 700 cursiva (familia inter_bi700)")
  wjpr_meta(p, base = base, complementos = comp)
}

## Bloque wjpr matrix — figura 6 -----------------------------------------------------------------------------------
## WJPr 1.1.0 no tiene mapa de calor: la base es geom_tile(fill = banda) + geom_text(text) con WJP_theme(), la familia
## y el tamaño de las capas de WJPr y wjp_palette() (categórica: violeta, azul verdoso, naranja) para las bandas;
## filas = df$grouping de arriba abajo y columnas = df$label en el orden de df$order; banda = df$text2 ("" en la
## diagonal y en las N/A: celda blanca). bands = nombres de las bandas de la más baja a la más alta (los labels del
## cut() de fig_06, F01.R:17, que la figura pasa): cada banda toma su color por nombre (cols_band en ese orden), la
## leyenda muestra las presentes en ese orden y el texto blanco va en la última, como F01.R:25-28, aunque en los datos
## falte alguna; una banda de text2 que no está en bands detiene la figura. Los complementos lo llevan al diseño de
## run_F01 (src/hungary/F01.R:24-41: theme_minimal 'inter', bandas COL_BG / COL_LIGHT / COL_MAIN, texto blanco en la
## banda alta, columnas arriba a 40°, títulos bold cursiva main, leyenda arriba), que se dibujó en un lienzo
## k = base_pt / 10 veces mayor (render_figure.R:29-38; 06_export.R): aquí cada tamaño va dividido por k porque
## wjpr_render() exporta al tamaño final, también la clave de la leyenda (legend.key.size). Los títulos de eje son
## texto fijo del diseño (argumentos). La galería cuenta text2 (la banda) como texto esperado: se dibuja como color y
## leyenda, no como texto en cada celda.
## Lo dibujado por celda (revisión 4, contrato extendido; wjpr_matrix_drawn, con complementos y sin ellos): una fila
## por posición de la rejilla con la celda que la ocupa según los ejes, lo impreso en ella, la banda que dice su color
## (decodificada con el mismo mapa de la escala) y las etiquetas de eje impresas en su fila y su columna.

# corregido (revisión 4): lo dibujado por celda, porque el multiconjunto de textos y el parecido no veían errores que
# conservan los textos (parecido_local con dos textos de celda intercambiados 0.083, con dos etiquetas de fila
# intercambiadas 0.431: ok). b = ggplot_build() del gráfico final; fill_map = color de cada banda en el orden de bands
# (el de la escala del adaptador: cols_band, o wjp_palette() sin complementos). Una fila por posición de la rejilla:
# niveles del eje y de arriba abajo × niveles del eje x de izquierda a derecha (wjpr_slots; order = esa secuencia, la
# de df: por filas), kind "cell", grouping = el nivel del eje y y label = el del eje x en esa posición (la fila de df
# que la ocupa según el dibujo, nunca df copiado en orden); text = lo impreso y visible en la celda (wjpr_drawn_texts:
# alfa 0 o del color de la celda no cuentan); text2 = la banda que dice el relleno de la celda superior en esa posición
# con fill_map ("" si es blanco, transparente o no hay celda: sin banda, como la diagonal y las N/A; "color <hex>" si
# no es de ninguna banda); label_drawn = la etiqueta del eje x impresa en esa columna y grouping_drawn = la del eje y
# en esa fila ("" si nada). frac, val: NA (la celda codifica la banda, no el valor). figures compara text, text2,
# label_drawn y grouping_drawn con df (estado "texto"); verify, con la referencia. Dos textos de celda intercambiados,
# dos etiquetas de fila o de columna intercambiadas, la matriz traspuesta o el color de una celda cambiado se ven aquí.
wjpr_matrix_drawn <- function(b, bands, fill_map) {
  sx <- wjpr_slots(b, "x"); sy <- wjpr_slots(b, "y")
  cells <- inner_join(select(sy, PANEL, grouping = level, rslot = slot), select(sx, PANEL, label = level, cslot = slot),
                      by = "PANEL", relationship = "many-to-many") %>% arrange(rslot, cslot)
  ckey <- paste(cells$PANEL, cells$rslot, cells$cslot)
  key_of <- function(d) paste(as.integer(as.character(d$PANEL)), wjpr_slot_of(d, sy, "y"), wjpr_slot_of(d, sx, "x"))
  # relleno de cada posición: el de la última celda dibujada en ella (la de encima)
  # sin nombres: con los de las capas (geom_tile), bind_rows tomaba la lista como una columna y las claves quedaban NA
  is_tile <- unname(which(vapply(b$plot$layers, function(l) inherits(l$geom, c("GeomTile", "GeomRect", "GeomRaster")), logical(1))))
  tiles <- bind_rows(tibble(k = character(), fill = character(), alpha = numeric()), lapply(is_tile, function(i) {
    d <- as.data.frame(b$data[[i]])
    if (!nrow(d) || !all(c("x", "y", "fill", "PANEL") %in% names(d))) return(NULL)
    tibble(k = key_of(d), fill = as.character(d$fill),
           alpha = if ("alpha" %in% names(d)) suppressWarnings(as.numeric(d$alpha)) else NA_real_)
  }))
  top <- tiles[!duplicated(tiles$k, fromLast = TRUE), , drop = FALSE]
  fill <- top$fill[match(ckey, top$k)]; alpha <- top$alpha[match(ckey, top$k)]
  seen <- !is.na(fill) & wjpr_rgba(fill)[4, ] * coalesce(alpha, 1) > 0 & !wjpr_same_color(fill, "white")
  band <- rep("", length(fill))
  for (j in rev(seq_along(bands))) band[seen & wjpr_same_color(fill, fill_map[j])] <- bands[j]
  odd <- seen & !nzchar(band)
  band[odd] <- paste("color", fill[odd])
  # lo impreso: textos de las capas en la celda; etiquetas de eje en su columna (x) y en su fila (y)
  tx <- wjpr_drawn_texts(b)
  lt <- tx[tx$source == "layer", , drop = FALSE]
  at_cell <- if (nrow(lt)) key_of(lt) else character()
  ax <- tx[tx$source == "axis-x", , drop = FALSE]; ay <- tx[tx$source == "axis-y", , drop = FALSE]
  ax_slot <- paste(ax$PANEL, wjpr_slot_of(ax, sx, "x")); ay_slot <- paste(ay$PANEL, wjpr_slot_of(ay, sy, "y"))
  glue <- function(lab, where, keys) vapply(keys, function(k) paste(lab[where == k], collapse = " "), "", USE.NAMES = FALSE)
  tibble(order = seq_len(nrow(cells)), kind = "cell", grouping = cells$grouping, label = cells$label,
         frac = NA_real_, frac2 = NA_real_, text = glue(lt$label, at_cell, ckey), text2 = band,
         label_drawn = glue(ax$label, ax_slot, paste(cells$PANEL, cells$cslot)),
         grouping_drawn = glue(ay$label, ay_slot, paste(cells$PANEL, cells$rslot)), val = NA_real_, val2 = NA_real_)
}
wjpr_matrix_drawn_fun <- function(bands, fill_map) {
  force(bands); force(fill_map)
  function(b) wjpr_matrix_drawn(b, bands, fill_map)
}

wjpr_matrix <- function(df, spec, complementos = wjpr_complementos(),
                        x_title = "Among people who experienced problems with:", y_title = "People who also experienced:",
                        bands = c("Less than 25%", "25% to 50%", "50% or more"),
                        cols_band = c(STD$col$track, STD$col$eu, STD$col$main)) {
  wjpr_require()
  if (length(cols_band) != length(bands)) {
    stop("wjpr_matrix: cols_band necesita un color por banda (", length(bands), ")", call. = FALSE)
  }
  df <- df[order(df$order), ]
  rows <- unique(df$grouping); cols <- unique(df$label)
  has_band <- nzchar(coalesce(df$text2, ""))
  # corregido: bandas por nombre (las de bands, en su orden), como F01.R:26-27, y no las presentes ordenadas por su
  # mínimo: si faltaba una, el color, la leyenda y el texto blanco se corrían a la banda siguiente (revisión V1-03).
  # Una banda desconocida se pintaba de blanco sin aviso: ahora se detiene con su nombre
  otras <- setdiff(unique(df$text2[has_band]), bands)
  if (length(otras) > 0) {
    stop("wjpr_matrix: bandas de text2 que no están en bands: ", paste(otras, collapse = ", "), call. = FALSE)
  }
  d <- df %>% mutate(band = factor(if_else(has_band, text2, NA_character_), levels = bands),
                     xf = factor(label, levels = cols), yf = factor(grouping, levels = rev(rows)))
  fam <- wjpr_wjp_family(); pal <- WJPr::wjp_palette()
  base <- "WJP_theme() + wjp_palette() (sin función de mapa de calor)"
  p <- ggplot(d, aes(x = xf, y = yf)) +
    geom_tile(aes(fill = band)) +
    geom_text(aes(label = text), family = fam, size = 3.514598, colour = "#4a4a49") +
    scale_fill_manual(values = setNames(pal[seq_along(bands)], bands), breaks = bands, na.value = "white") +
    scale_x_discrete(position = "top") +
    labs(x = x_title, y = y_title, fill = NULL) +
    WJPr::WJP_theme()
  # corregido (revisión 4): lo dibujado por celda también sin complementos, con el mapa de su escala (wjp_palette)
  if (!isTRUE(complementos)) return(wjpr_meta(p, base = base, drawn = wjpr_matrix_drawn_fun(bands, pal[seq_along(bands)])))

  k <- (if (is.null(spec$base_pt) || is.na(spec$base_pt)) 10 else spec$base_pt) / 10   # base_pt vacío llega como NA
  if (is.na(k)) k <- 1
  comp <- character()
  # complemento: WJPr solo trae una paleta categórica (wjp_palette) y el gris "#e5e8e8"; no tiene la escala ordinal
  # del reporte (COL_BG #e5e8e8, COL_LIGHT #a8a8c8, COL_MAIN #575796), aquí por nombre de banda (F01.R:26-27)
  p <- suppressMessages(p + scale_fill_manual(values = setNames(cols_band, bands), breaks = bands, na.value = "white"))
  comp <- c(comp, "escala ordinal del reporte (3 bandas)")
  # complemento: celdas separadas por un borde blanco y texto blanco sobre la banda alta (la última de bands, por
  # nombre: F01.R:25; #1A1A1A en el resto); WJPr no tiene argumento de borde ni de color de texto según el fondo
  p$layers[[1]] <- geom_tile(aes(fill = band), colour = "white", linewidth = 0.8 / k)
  p$layers[[2]] <- geom_text(aes(label = text, colour = band %in% tail(bands, 1)), family = fam, size = 3.6 / k,
                             show.legend = FALSE)
  p <- p + scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = STD$col$text), guide = "none")
  comp <- c(comp, "borde blanco de celda y texto blanco en la banda alta")
  # complemento: WJP_theme() es un tema parcial sobre theme_grey(11) (leyenda, espaciados): aquí la base del diseño,
  # theme_minimal a 11 / k pt, con WJP_theme() encima
  p <- p + theme_minimal(base_size = 11 / k) + WJPr::WJP_theme()
  comp <- c(comp, "tema base theme_minimal(11 / k)")
  # complemento: estilo del diseño sobre WJP_theme(): sin rejilla (panel.grid.major es explícito en WJP_theme),
  # columnas arriba a 40°, textos 11 / k pt #1A1A1A, títulos 12 / k pt bold cursiva main (el de x alineado a la
  # izquierda; margen de theme_minimal, no el margin(10) de los títulos de WJP_theme()),
  # leyenda arriba sin título, márgenes del lienzo original
  # corregido: la clave de la leyenda también va dividida por k (legend.key.size = 1.2 / k líneas): la de
  # theme_minimal son 1.2 líneas de grid, 17.28 pt que no escalan con base_size, y salía de 15.22 pt (13.65 en la
  # publicada), con la matriz 1.56 pt más abajo (revisión V3-05)
  p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 axis.text.x.top = element_text(angle = 40, hjust = 0, vjust = 0, size = 11 / k, colour = STD$col$text),
                 axis.text.y = element_text(size = 11 / k, colour = STD$col$text),
                 axis.title.x.top = element_text(face = "bold.italic", colour = STD$col$main, hjust = 0, size = 12 / k,
                                                 margin = margin(b = 11 / 4 / k)),
                 axis.title.y = element_text(face = "bold.italic", colour = STD$col$main, size = 12 / k,
                                             margin = margin(r = 11 / 4 / k)),
                 legend.position = "top", legend.title = element_blank(), legend.text = element_text(size = 11 / k),
                 legend.key.size = grid::unit(1.2 / k, "lines"),
                 plot.margin = margin(10 / k, 45 / k, 10 / k, 10 / k), plot.background = element_rect(fill = "white", colour = NA))
  comp <- c(comp, "tema del diseño (ejes, títulos, leyenda, márgenes)")
  # complemento: WJPr fija "Lato Full" en capas y tema, sin argumento para cambiarla
  p <- wjpr_font(p); comp <- c(comp, "fuente Inter Tight (wjpr_font)")
  # corregido (revisión 4): lo dibujado por celda (wjpr_matrix_drawn) con el mapa de la escala del diseño (cols_band)
  wjpr_meta(p, base = base, complementos = comp, compara = "text",  # text2 = banda (color)
            drawn = wjpr_matrix_drawn_fun(bands, cols_band))
}
## Prueba del bloque (revisión 3, grupo F2, 2026-10-23; pruebas/p02_matriz.R y p06, ggplot_build del adaptador): con
## el df de lo publicado, 0 de 144 rellenos distintos de F01.R:26 y el mismo df que antes; sin las celdas «Less than
## 25%», 0 (antes 90: «25% to 50%» en #E5E8E8 y «50% or more» en #A8A8C8); sin «50% or more», 0 textos blancos fuera de
## esa banda (antes 49); banda desconocida en text2: error con su nombre (antes, celda blanca sin aviso). Claves de la
## leyenda de 13.65 pt en el SVG, como la publicada (antes 15.22); parecido a r = 0 / 1 / 2 px 0.9987 / 1.0000 / 1.0000
## y mad 0.0005 (antes 0.9350 / 0.9922 / 0.9997 y 0.0286); main.R --stage all con 1.1.0@382aee9 y con main a46c831:
## 416/416 ok, f1 0.9997 → 1.0000.
## Lo dibujado por celda (revisión 4, 2026-10-27; SCRATCH/fix4/INT/pruebas/iny_g2.R e iny_run.sh): 144 celdas, verify
## 706/706 (antes 418); cada inyección, una a la vez y solo en el dibujo, --stage all --figs 06 --strict → exit 1: dos
## textos de celda intercambiados (texto · text Consumer: 62%→64%), dos etiquetas de fila (texto · grouping_drawn, 24
## celdas), la matriz traspuesta (texto, 236), el color de banda de una celda (texto · text2 50% or more→25% to 50%).
## SVG y PNG de Hungría iguales (md5) a los de antes.

## Bloque wjpr scatter — figura 3 ----------------------------------------------------------------------------------
## WJPr 1.1.0 no tiene diagrama de dispersión x-y (wjp_dots es un diagrama de puntos por categoría y wjp_lines une los
## puntos con líneas): la base es geom_smooth(lm) + geom_point + ggrepel::geom_text_repel (como wjp_lines(repel =
## TRUE): "Lato Full" bold 3.514598) + facet_wrap(grouping) con WJP_theme() y wjp_palette()[1]; x = df$value
## (prevalencia %), y = df$value2 (severidad 0-10; NA = sin punto), facetas en el orden de df$order. Destacadas =
## df$series == "destacada": data() las toma de config/<pais>.yml (figures: fig03: highlight; las de lo publicado son
## el top_cats de F02.R:33, una elección editorial); highlight (vector de categorías) solo vale para un df sin series,
## como el de la referencia. Un punto sin prevalencia o sin severidad (suprimido) no se dibuja, como F02.R:38
## (na.rm): el log cuenta cuántos faltan por faceta, y una faceta con menos de min_points puntos dibujables se quita
## del dibujo con aviso (sin ninguna que llegue, error). Los complementos lo llevan al diseño de run_F02
## (src/hungary/F02.R:33-50: theme_minimal 'inter', destacadas en main y el resto #c4c4c4, punto hueco si n < 30,
## rótulos solo de las destacadas con seed 7, ejes 0-50 % y 4-8, rejilla #eeeeee, facetas bold cursiva main),
## dibujado en un lienzo k = base_pt / 10 mayor: aquí cada tamaño va dividido por k, también los rellenos de ggrepel
## (box.padding, min.segment.length, point.size, segment.size). El contrato del scatter no imprime valores (text = ""):
## los rótulos de categoría (destacadas con punto dibujable) van en rotulos y entran en la comparación de textos.
## Lo dibujado por punto (revisión 4, contrato extendido; wjpr_scatter_drawn, con complementos y sin ellos): una fila
## por punto dibujado, con la categoría que ocupa esa posición, x e y decodificados (val, val2), el rótulo anclado en
## él (label_drawn), la faceta impresa (grouping_drawn) y si es hueco (text2 = WJPR_SCATTER_HUECO, el text2 que data()
## pone a los puntos dibujables con n < 30); df$text2 ya no es texto impreso: compara = "text".

# text2 de un punto hueco (n < 30) en df (data() de la figura) y en lo dibujado por punto ("" = punto lleno)
WJPR_SCATTER_HUECO <- "n < 30"

# ¿Punto hueco? Formas de R sin relleno (pch 0-14 y sus nombres de ggplot2) o con relleno (21-25) que no se ve
wjpr_shape_hollow <- function(shape, fill = NA) {
  s <- as.character(shape); n <- suppressWarnings(as.numeric(s))
  open <- c("square open", "circle open", "triangle open", "plus", "cross", "diamond open", "triangle down open",
            "square cross", "asterisk", "diamond plus", "circle plus", "star", "square plus", "circle cross",
            "square triangle", "triangle square")
  with_fill <- n %in% 21:25 | s %in% c("circle filled", "square filled", "diamond filled", "triangle filled", "triangle down filled")
  fill <- rep_len(as.character(fill), length(s))
  no_fill <- is.na(fill) | wjpr_rgba(fill)[4, ] == 0 | wjpr_same_color(fill, "white")
  (!is.na(n) & n %in% 0:14) | s %in% open | (with_fill & no_fill)
}

# Asigna puntos a claves por cercanía (D = distancias puntos × claves): de menor a mayor distancia, cada punto a una
# clave libre y cada clave a un punto; los puntos que sobran quedan NA
wjpr_nearest_assign <- function(D) {
  out <- rep(NA_integer_, nrow(D))
  if (!length(D)) return(out)
  D[!is.finite(D)] <- Inf
  while (any(is.finite(D))) {
    m <- which(D == min(D), arr.ind = TRUE)[1, ]
    out[m[1]] <- m[2]; D[m[1], ] <- Inf; D[, m[2]] <- Inf
  }
  out
}

# corregido (revisión 4): lo dibujado por punto, porque el parecido no veía un punto hueco con n ≥ 30
# (parecido_local 0.271: ok) ni, sin fila, un rótulo junto a otro punto o un punto desplazado unas décimas. b =
# ggplot_build() del gráfico final; keys = df del adaptador (order, kind, grouping, label, value, value2, destacada).
# Una fila por punto dibujado (capas GeomPoint: x e y finitos, forma, tamaño y color presentes, alfa y tamaño > 0): su
# clave sale de la POSICIÓN: grouping = la faceta de su panel y label = la categoría de esa faceta más cercana a (x,
# y) en unidades del panel (wjpr_nearest_assign, una a una; una clave sin prevalencia o sin severidad solo se asigna
# si no queda otra; un punto que sobra lleva label "": sobrante en figures). val, val2 = x, y decodificados con la
# inversa de la escala (unidades de df$value, df$value2); text = "" (no se imprimen valores); text2 =
# WJPR_SCATTER_HUECO si el punto es hueco (wjpr_shape_hollow) y "" si es lleno; label_drawn = el rótulo (capas de
# texto visibles, wjpr_drawn_texts) anclado en ese punto: ggrepel mueve el rótulo al dibujar, pero ggplot_build da su
# ancla (x, y del dato), que se compara con la del punto (|Δ| ≤ 1e-6 del rango del panel); sin rótulo, "" si la
# categoría es destacada (se esperaba) y NA si no (nada impreso ni esperado: df$label no aplica); grouping_drawn = el
# strip impreso de su panel ("" si no se ve; NA sin facetas). Además, una fila por clave de df sin punto (suprimida,
# o de una faceta quitada por min_points) con val/val2/label_drawn/grouping_drawn NA y textos "", para que lo dibujado
# cubra cada fila de df como los slots sin barra de wjpr_gb_drawn; un rótulo sin punto en su ancla es una fila sobrante
# (label "", label_drawn = el rótulo). order: paneles de izquierda a derecha y de arriba abajo y, en cada uno, el orden
# de df de la clave (un scatter no tiene orden visual dentro del panel); sobrantes al final de su panel.
wjpr_scatter_drawn <- function(b, keys, hollow = WJPR_SCATTER_HUECO) {
  lay <- b$layout$layout
  fv <- names(b$layout$facet$params$facets %||% list())
  faceted <- length(fv) > 0 && fv[1] %in% names(lay)
  o <- order(lay$ROW, lay$COL); rk <- integer(nrow(lay)); rk[o] <- seq_along(o)
  pan <- tibble(PANEL = as.character(lay$PANEL), grupo = if (faceted) as.character(lay[[fv[1]]]) else NA_character_, rank = rk)
  inv <- function(sc) {
    tr <- tryCatch(sc$get_transformation(), error = function(e) sc$trans)
    if (is.null(tr) || !is.function(tr$inverse)) identity else tr$inverse
  }
  tx <- wjpr_drawn_texts(b)
  strips <- tx[tx$source == "strip", , drop = FALSE]
  lab <- tx[tx$source == "layer", , drop = FALSE]; lab$used <- rep(FALSE, nrow(lab))
  is_pt <- unname(which(vapply(b$plot$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))))   # sin nombres (bind_rows)
  num <- function(d, nm, default) if (nm %in% names(d)) suppressWarnings(as.numeric(d[[nm]])) else rep(default, nrow(d))
  pts <- bind_rows(tibble(PANEL = character(), x = numeric(), y = numeric(), shape = character(), fill = character()),
                   lapply(is_pt, function(i) {
    d <- as.data.frame(b$data[[i]])
    if (!nrow(d)) return(NULL)
    ok <- is.finite(num(d, "x", NA)) & is.finite(num(d, "y", NA)) & !is.na(if ("shape" %in% names(d)) d$shape else 19) &
      coalesce(num(d, "size", 1), 0) > 0 & coalesce(num(d, "alpha", 1), 1) > 0 & !is.na(if ("colour" %in% names(d)) d$colour else "black")
    tibble(PANEL = as.character(d$PANEL), x = num(d, "x", NA), y = num(d, "y", NA),
           shape = if ("shape" %in% names(d)) as.character(d$shape) else "19",
           fill = if ("fill" %in% names(d)) as.character(d$fill) else NA_character_)[ok, ]
  }))
  rows <- list(); matched <- rep(FALSE, nrow(keys))
  for (k in seq_len(nrow(pan))) {
    P <- pts[pts$PANEL == pan$PANEL[k], , drop = FALSE]
    if (!nrow(P)) next
    pp <- b$layout$panel_params[[match(pan$PANEL[k], as.character(lay$PANEL))]]
    ix <- inv(b$layout$panel_scales_x[[lay$SCALE_X[k]]]); iy <- inv(b$layout$panel_scales_y[[lay$SCALE_Y[k]]])
    vx <- ix(P$x); vy <- iy(P$y)
    xr <- abs(diff(range(ix(pp$x.range)))); yr <- abs(diff(range(iy(pp$y.range))))
    cand <- which(if (faceted) keys$grouping == pan$grupo[k] else rep(TRUE, nrow(keys)))
    cand <- cand[!matched[cand]]
    term <- function(a, b, r) { t <- outer(a, b, function(u, v) ((u - v) / r)^2); t[is.na(t)] <- 1e6; t }
    D <- if (length(cand)) term(vx, keys$value[cand], xr) + term(vy, keys$value2[cand], yr) else matrix(numeric(), nrow(P), 0)
    a <- wjpr_nearest_assign(D); ki <- cand[a]
    matched[ki[!is.na(ki)]] <- TRUE
    # rótulos anclados en cada punto (en coordenadas del panel, antes de la inversa)
    L <- which(lab$PANEL == pan$PANEL[k])
    tolx <- 1e-6 * abs(diff(pp$x.range)); toly <- 1e-6 * abs(diff(pp$y.range))
    lab_at <- vapply(seq_len(nrow(P)), function(j) {
      hit <- L[abs(lab$x[L] - P$x[j]) <= tolx & abs(lab$y[L] - P$y[j]) <= toly & !lab$used[L]]
      if (!length(hit)) return(NA_character_)
      lab$used[hit] <<- TRUE
      paste(lab$label[hit], collapse = " ")
    }, "")
    dest <- !is.na(ki) & coalesce(keys$destacada[ki], FALSE)
    rows[[length(rows) + 1]] <- tibble(
      .rank = pan$rank[k], .ord = coalesce(as.numeric(keys$order[ki]), Inf), .x = vx,
      kind = coalesce(keys$kind[ki], "point"), grouping = if (faceted) pan$grupo[k] else coalesce(keys$grouping[ki], ""),
      label = coalesce(keys$label[ki], ""), frac = NA_real_, frac2 = NA_real_, text = "",
      text2 = if_else(wjpr_shape_hollow(P$shape, P$fill), hollow, ""),
      label_drawn = coalesce(lab_at, if_else(dest, "", NA_character_)),
      grouping_drawn = if (faceted) coalesce(strips$label[match(pan$PANEL[k], strips$PANEL)], "") else NA_character_,
      val = vx, val2 = vy)
  }
  # claves de df sin punto: fila sin marca (NA) en su lugar del orden; rótulos sin punto en su ancla: sobrantes
  un <- which(!matched)
  if (length(un)) {
    rows[[length(rows) + 1]] <- tibble(
      .rank = coalesce(pan$rank[match(keys$grouping[un], pan$grupo)], Inf), .ord = as.numeric(keys$order[un]), .x = NA_real_,
      kind = keys$kind[un], grouping = keys$grouping[un], label = keys$label[un], frac = NA_real_, frac2 = NA_real_,
      text = "", text2 = coalesce(keys$text2_sin_punto[un], ""), label_drawn = NA_character_, grouping_drawn = NA_character_,
      val = NA_real_, val2 = NA_real_)
  }
  orph <- which(!lab$used)
  if (length(orph)) {
    rows[[length(rows) + 1]] <- tibble(
      .rank = coalesce(pan$rank[match(lab$PANEL[orph], pan$PANEL)], Inf), .ord = Inf, .x = lab$x[orph], kind = "point",
      grouping = if (faceted) pan$grupo[match(lab$PANEL[orph], pan$PANEL)] else "", label = "", frac = NA_real_,
      frac2 = NA_real_, text = "", text2 = "", label_drawn = lab$label[orph], grouping_drawn = NA_character_,
      val = NA_real_, val2 = NA_real_)
  }
  out <- bind_rows(tibble(.rank = numeric(), .ord = numeric(), .x = numeric()), rows)
  out <- out[order(out$.rank, out$.ord, out$.x), , drop = FALSE]
  out %>% mutate(order = row_number(), .before = 1) %>% select(-.rank, -.ord, -.x)
}
wjpr_scatter_drawn_fun <- function(keys, hollow = WJPR_SCATTER_HUECO) {
  force(keys); force(hollow)
  function(b) wjpr_scatter_drawn(b, keys, hollow)
}

wjpr_scatter <- function(df, spec, complementos = wjpr_complementos(), highlight = NULL,
                         x_title = "Prevalence", y_title = "Mean seriousness (0-10)", min_points = 2) {
  wjpr_require()
  df <- df[order(df$order), ]
  tag <- sprintf("fig %02d", as.integer(spec$fig %||% 3L))
  small <- if ("n" %in% names(df)) coalesce(df$n < 30, FALSE) else rep(FALSE, nrow(df))   # sin n: ningún punto hueco
  # corregido: las destacadas vienen del df (series = "destacada", de config/<pais>.yml) y no de la lista fija de
  # lo publicado en el adaptador, que en otro país rotulaba en silencio las mismas 6 (revisión V2-03)
  dest <- if ("series" %in% names(df)) coalesce(df$series == "destacada", FALSE) else df$label %in% highlight
  d <- df %>% mutate(facet = factor(grouping, levels = unique(grouping)), destacada = dest,
                     hueco = small | is.na(value2))
  # corregido: los puntos sin dibujar se cuentan por faceta en el log y una faceta con menos de min_points puntos no
  # se dibuja; antes desaparecían sin aviso y la faceta podía quedar vacía (Malta: Rural sin ningún punto, revisión
  # V4-03). En lo publicado faltan 2 de 12 en Rural y 1 de 12 en Urban: mismo dibujo
  vis <- d %>% group_by(facet) %>% summarise(n = n(), dib = sum(!is.na(value) & !is.na(value2)), .groups = "drop")
  if (any(vis$dib < vis$n)) {
    log_msg(tag, ": puntos sin dibujar (sin prevalencia o sin severidad): ",
            paste0(vis$facet, " ", vis$n - vis$dib, " de ", vis$n, collapse = ", "))
  }
  fuera <- as.character(vis$facet[vis$dib < min_points])
  cuenta <- paste0(vis$facet, " ", vis$dib, collapse = ", ")
  if (length(fuera) == nrow(vis)) {
    stop(tag, ": ninguna faceta con ", min_points, " puntos o más para dibujar (puntos con prevalencia y severidad: ",
         cuenta, ")", call. = FALSE)
  }
  if (length(fuera) > 0) {
    log_msg(tag, " aviso: faceta ", paste(fuera, collapse = ", "), " sin dibujar: menos de ", min_points,
            " puntos con prevalencia y severidad (", cuenta, ")")
    d <- d %>% filter(!facet %in% fuera) %>% mutate(facet = droplevels(facet))
  }
  # corregido (revisión 4): lo dibujado por punto (wjpr_scatter_drawn), con complementos y sin ellos; las claves son
  # las de df entero (también las de una faceta que no se dibuje: quedan como filas sin punto). text2_sin_punto: en una
  # faceta que el diseño no dibuja (fuera, con aviso en el log) la marca de hueco de df no se compara (sin punto no
  # hay hueco que ver); en las demás, una clave sin punto con la marca es un punto esperado que falta
  drawn <- wjpr_scatter_drawn_fun(tibble(order = df$order, kind = df$kind, grouping = df$grouping, label = df$label,
                                         value = df$value, value2 = df$value2, destacada = dest,
                                         text2_sin_punto = if_else(df$grouping %in% fuera, df$text2, "")))
  # corregido (V3-07): las categorías rotuladas entran en la comparación de textos (attr(p, "wjpr")$rotulos); antes
  # figures y verify no las miraban y la 3 quedaba «sin textos que comparar»
  rot <- as.character(d$label[d$destacada & is.finite(d$value) & is.finite(d$value2)])
  fam <- wjpr_wjp_family(); pal <- WJPr::wjp_palette()
  base <- "WJP_theme() + wjp_palette() (sin función de dispersión)"
  p <- ggplot(d, aes(x = value, y = value2)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = pal[9], na.rm = TRUE) +
    geom_point(colour = pal[1], na.rm = TRUE) +
    ggrepel::geom_text_repel(data = d %>% filter(destacada), aes(label = label), family = fam, fontface = "bold",
                             size = 3.514598, seed = 7, na.rm = TRUE) +
    facet_wrap(~facet, nrow = 1) +
    labs(x = x_title, y = y_title) +
    WJPr::WJP_theme()
  # corregido (revisión 4): compara = "text" (df$text2 marca el punto hueco, WJPR_SCATTER_HUECO: no se imprime) y
  # lo dibujado por punto
  if (!isTRUE(complementos)) return(wjpr_meta(p, base = base, compara = "text", rotulos = rot, drawn = drawn))

  k <- (if (is.null(spec$base_pt) || is.na(spec$base_pt)) 10 else spec$base_pt) / 10   # base_pt vacío llega como NA
  if (is.na(k)) k <- 1
  comp <- character()
  # complemento: línea de tendencia gris discontinua fina; WJPr no tiene capa de tendencia
  p$layers[[1]] <- geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "#9aa0a6", linewidth = 0.5 / k,
                               linetype = "dashed", na.rm = TRUE)
  # complemento: destacadas en main (#575796) y el resto #c4c4c4, punto hueco si n < 30; WJPr no tiene el color
  # principal del reporte ni un argumento para resaltar un subconjunto
  p$layers[[2]] <- geom_point(aes(colour = destacada, shape = hueco), size = 3.4 / k, stroke = 1.1 / k, na.rm = TRUE)
  p <- p + scale_colour_manual(values = c(`TRUE` = STD$col$main, `FALSE` = "#c4c4c4"), guide = "none") +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), guide = "none")
  comp <- c(comp, "tendencia discontinua gris", "puntos destacados y huecos (n < 30)")
  # complemento: rótulos en main, regular (WJPr: bold #4a4a49), 4 / k mm, con la semilla y el segmento del diseño
  # corregido: los rellenos de ggrepel también van divididos por k (box.padding 0.25 líneas, min.segment.length 0.3
  # líneas, point.size 1 y el segmento de 0.5 mm del lienzo original): sin dividir, los rótulos se movían unos px
  # respecto a la publicada (revisión V3-06)
  p$layers[[3]] <- ggrepel::geom_text_repel(data = d %>% filter(destacada), aes(label = label), family = fam,
                                            size = 4 / k, colour = STD$col$main, seed = 7, box.padding = 0.25 / k,
                                            min.segment.length = 0.3 / k, point.size = 1 / k, segment.size = 0.5 / k,
                                            na.rm = TRUE)
  comp <- c(comp, "rótulos de las destacadas (main, 4 mm, seed 7)")
  # complemento: ejes del diseño (0-50 % con sufijo, 4-8 cada 1); WJPr fija sus ejes dentro de cada función. Si un
  # punto cae fuera (otro país: p. ej. severidad 8.10), el eje se amplía en vez de descartarlo en silencio
  xl <- c(0, max(50, ceiling(max(d$value, na.rm = TRUE) / 10) * 10))
  yl <- c(min(4, floor(min(d$value2, na.rm = TRUE))), max(8, ceiling(max(d$value2, na.rm = TRUE))))
  p <- p + scale_x_continuous(limits = xl, labels = function(x) paste0(x, "%")) +
    scale_y_continuous(limits = yl, breaks = seq(yl[1], yl[2]))
  comp <- c(comp, "escalas 0-50 % y 4-8")
  # complemento: WJP_theme() es parcial sobre theme_grey(11) (franjas grises en las facetas): base theme_minimal a
  # 11 / k pt con WJP_theme() encima; rejilla continua #eeeeee (WJP_theme: discontinua #5e5c5a), textos 12 / k
  # #1A1A1A, facetas 14 / k bold cursiva main, títulos con el margen de theme_minimal, fondo blanco
  p <- p + theme_minimal(base_size = 11 / k) + WJPr::WJP_theme() +
    theme(strip.text = element_text(face = "bold.italic", colour = STD$col$main, size = 14 / k),
          axis.text = element_text(size = 12 / k, colour = STD$col$text),
          axis.text.x = element_text(size = 12 / k, colour = STD$col$text),
          axis.text.y = element_text(size = 12 / k, colour = STD$col$text),
          axis.title.x = element_text(size = 12 / k, colour = STD$col$text, margin = margin(t = 11 / 4 / k)),
          axis.title.y = element_text(size = 12 / k, colour = STD$col$text, margin = margin(r = 11 / 4 / k)),
          panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "#eeeeee", linetype = "solid", linewidth = 0.5 / k),
          plot.margin = margin(11 / 2 / k, 11 / 2 / k, 11 / 2 / k, 11 / 2 / k),
          plot.background = element_rect(fill = "white", colour = NA))
  comp <- c(comp, "tema del diseño (theme_minimal, rejilla, facetas, ejes)")
  # complemento: WJPr fija "Lato Full" en capas y tema, sin argumento para cambiarla
  p <- wjpr_font(p); comp <- c(comp, "fuente Inter Tight (wjpr_font)")
  wjpr_meta(p, base = base, complementos = comp, compara = "text", rotulos = rot, drawn = drawn)
}
## Prueba del bloque (revisión 3, grupo F2, 2026-10-23; pruebas/p03_dispersion.R y p05_barrido.R): lo publicado: df
## de antes + series (las 6 de config/hungary.yml, las mismas rotuladas), parecido a r = 0 / 1 / 2 px 0.9806 / 0.9997 /
## 1.0000 y mad 0.0007 (antes 0.6843 / 0.9682 / 0.9996 y 0.0079), log «Rural 2 de 12, Urban 1 de 12». Malta y Bulgaria:
## antes un panel Rural vacío o con 1 punto; ahora solo Urban y aviso. Sin la clave (Poland, Italy, Greece): aviso con
## las 6 de mayor prevalencia (antes, en silencio, las 6 publicadas). Nombre que no es categoría: aviso y se ignora;
## ninguna faceta con 2 puntos: error legible (antes «Problem while computing stat.»). 26 países LNS sin error.
## Rótulos (2026-10-25, main.R --stage all --figs 03 en las dos builds): con rotulos = las destacadas con punto, textos
## «iguales» en figures y verify (antes «sin textos que comparar»); SVG igual byte a byte.
## Lo dibujado por punto (revisión 4, 2026-10-27; iny_g2.R): 24 filas (21 puntos y 3 claves sin punto), verify 156/156
## (antes 81); --figs 03 --strict → exit 1 con: un punto +3 pp en x (dibujo · val Accidental 23.43→26.43), un punto
## hueco con n ≥ 30 (texto · text2 Consumer: →n < 30; antes pasaba, parecido_local 0.271), dos rótulos intercambiados
## y un rótulo junto a otro punto (texto · label_drawn). Hungría no tiene puntos huecos; SVG y PNG iguales (md5).

## Bloque wjpr upset — figura 29 -----------------------------------------------------------------------------------
## WJPr 1.1.0 no tiene diagrama UpSet. Lo publicado salió de ComplexUpset::upset() (render_figure.R:53-75), que
## devuelve un patchwork de tres ggplot: la galería (ggplot_build, wjpr_font) solo vería el último. Por eso aquí es UN
## ggplot en coordenadas de la figura (pt, y hacia abajo) con la geometría de ese UpSet: barras de intersección
## (ancho 0.9 del paso, expansión 0.6 por lado como una escala discreta, alto = value con 25 % de aire arriba), matriz
## de conjuntos (paso de fila = alto / (nº de conjuntos + 0.2), puntos activos e inactivos, segmento entre el primero y
## el último activo), barras de tamaño de conjunto a la izquierda (fuera del panel, coord_cartesian(clip = "off"),
## 0.22 del ancho de paneles, 70 % de aire) y los nombres de conjunto como texto del eje y (no son capas). Base:
## WJP_theme() + wjp_palette() con las capas de texto como las de WJPr ("Lato Full" bold 3.514598, horizontal, centrado
## sobre la barra); los complementos lo llevan al diseño publicado (lienzo k = base_pt / 10 = 1.024: cada medida de
## ComplexUpset va dividida por k). Filas: kind row, label "A + B" (conjuntos de df kind set), en el orden de df$order
## de izquierda a derecha; conjuntos: kind set en el orden de df$order de arriba abajo; text tal cual.
wjpr_upset <- function(df, spec, complementos = wjpr_complementos(),
                       y_title = "% of people within\nthe justice gap", width_ratio = 0.22) {
  wjpr_require()
  df <- df[order(df$order), ]
  inter <- df[df$kind == "row", ]; sets <- df[df$kind == "set", ]
  memb <- lapply(strsplit(inter$label, " + ", fixed = TRUE), trimws)
  bad <- setdiff(unlist(memb), sets$label)
  if (length(bad)) stop("wjpr_upset: conjuntos sin fila set en df: ", paste(unique(bad), collapse = ", "), call. = FALSE)
  k <- (if (is.null(spec$base_pt) || is.na(spec$base_pt)) 10 else spec$base_pt) / 10   # base_pt vacío llega como NA
  if (is.na(k)) k <- 1
  W <- spec$w_in * 72; H <- spec$h_in * 72
  m <- 2 * 5.48 / k                                   # margen de ggplot + margen de patchwork (5.5 pt TeX = 5.48 bp)
  lab_size <- 0.8 * 12.8 / k                          # axis.text = rel(0.8) del texto de ComplexUpset (12.8)
  tit_size <- 11 / k
  tl <- strsplit(y_title, "\n", fixed = TRUE)[[1]]
  th <- tit_size * (0.9418 + 0.9 * 1.2 * (length(tl) - 1))   # alto del título girado (métricas de Inter Tight)
  lmar <- 4.43 / k                                    # nombres de conjunto → matriz (margen + marca del eje)
  maxw <- max(vapply(sets$label, text_width, 0, size = lab_size))
  lab_w <- th + 3.2 / k + maxw + lmar                 # columna del título (alineada por patchwork) + nombres
  n <- nrow(inter); ns <- nrow(sets)
  # paneles: [tamaños | nombres | matriz/barras]; anchos de panel 0.22 / 0.78 (width_ratio de ComplexUpset)
  avail <- W - 3 * m - lab_w
  sw <- width_ratio * avail; mw <- avail - sw
  xs1 <- m + sw; xp0 <- xs1 + m + lab_w; xp1 <- W - m
  # alturas: barras / matriz 2:1 (height_ratio 0.5), con un hueco 2m entre ambos paneles
  ph <- H - 3 * m; bh <- ph * 2 / 3; mh <- ph / 3
  yb0 <- m; yb1 <- m + bh; ym0 <- yb1 + m; ym1 <- H - m
  pitch <- mw / (n + 0.2); xc <- xp0 + pitch * (0.6 + seq_len(n) - 1)
  rpitch <- mh / (ns + 0.2); yr <- setNames(ym0 + rpitch * (0.6 + seq_len(ns) - 1), sets$label)
  hb <- inter$value * bh / (max(inter$value) * 1.25)
  bars <- tibble(xmin = xc - 0.45 * pitch, xmax = xc + 0.45 * pitch, ymin = yb1 - hb, ymax = yb1, text = inter$text, x = xc, y = yb1 - hb)
  dots <- bind_rows(lapply(seq_len(n), function(i) tibble(x = xc[i], y = unname(yr), on = sets$label %in% memb[[i]])))
  segs <- bind_rows(lapply(seq_len(n), function(i) {
    yy <- unname(yr[memb[[i]]]); if (length(yy) < 2) return(NULL); tibble(x = xc[i], y = min(yy), yend = max(yy))
  }))
  wsz <- sets$value * sw / (max(sets$value) * 1.7)
  sbar <- tibble(xmin = xs1 - wsz, xmax = xs1, ymin = unname(yr) - 0.35 * rpitch, ymax = unname(yr) + 0.35 * rpitch,
                 x = xs1 - wsz, y = unname(yr), text = sets$text)
  fam <- wjpr_wjp_family(); pal <- WJPr::wjp_palette()
  base <- "WJP_theme() + wjp_palette() (sin función UpSet; geometría de ComplexUpset en un ggplot)"
  p <- ggplot() +
    geom_rect(data = bars, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = pal[1]) +
    geom_text(data = bars, aes(x = x, y = y, label = text), family = fam, fontface = "bold", size = 3.514598, colour = "#4a4a49") +
    geom_segment(data = segs, aes(x = x, xend = x, y = y, yend = yend), colour = pal[1]) +
    geom_point(data = dots, aes(x = x, y = y, colour = on), size = 3.5) +
    geom_rect(data = sbar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = pal[1]) +
    geom_text(data = sbar, aes(x = x, y = y, label = text), family = fam, fontface = "bold", size = 3.514598,
              colour = "#4a4a49", hjust = 1) +
    scale_colour_manual(values = c(`TRUE` = pal[1], `FALSE` = "#e5e8e8"), guide = "none") +
    scale_x_continuous(limits = c(xp0, xp1), expand = c(0, 0), breaks = NULL, oob = scales::oob_keep) +
    scale_y_reverse(limits = c(ym1, yb0), expand = c(0, 0), breaks = unname(yr), labels = names(yr), oob = scales::oob_keep) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = y_title) +
    WJPr::WJP_theme() +
    theme(plot.margin = margin(m, m, m, xp0 - lab_w, unit = "bigpts"))
  if (!isTRUE(complementos)) return(wjpr_meta(p, base = base))

  comp <- character()
  # complemento: colores del diseño (barras main #575796; puntos activos #1A1A1A, inactivos #E5E8E8; segmento negro);
  # WJPr no tiene argumentos de color para estas capas porque no tiene la función
  p$layers[[1]] <- geom_rect(data = bars, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = STD$col$main)
  p$layers[[3]] <- geom_segment(data = segs, aes(x = x, xend = x, y = y, yend = yend), colour = "black", linewidth = 0.5 / k)
  p$layers[[4]] <- geom_point(data = dots, aes(x = x, y = y, colour = on), size = 3.5 / k, stroke = 0.5 / k)
  p$layers[[5]] <- geom_rect(data = sbar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = STD$col$main)
  p <- suppressMessages(p + scale_colour_manual(values = c(`TRUE` = STD$col$text, `FALSE` = STD$col$track), guide = "none"))
  comp <- c(comp, "colores del diseño (barras, puntos, segmento)")
  # complemento: valores de las intersecciones en vertical (angle 90) a continuación de la barra (hjust −0.15), 3.6 / k
  # mm, regular, main; WJPr (wjp_bars) los centra en el extremo, en horizontal y bold #4a4a49
  p$layers[[2]] <- geom_text(data = bars, aes(x = x, y = y, label = text), family = fam, size = 3.6 / k, colour = STD$col$main,
                             angle = 90, hjust = -0.15, vjust = 0.5)
  # complemento: tamaños de conjunto a la izquierda de su barra (hjust 1.15), regular, #1A1A1A
  p$layers[[6]] <- geom_text(data = sbar, aes(x = x, y = y, label = text), family = fam, size = 3.6 / k,
                             colour = STD$col$text, hjust = 1.15)
  comp <- c(comp, "valores verticales tras la barra y tamaños de conjunto")
  # complemento: rejilla clara de la matriz (#EBEBEB, columnas y filas), en vez de la rejilla discontinua de WJP_theme()
  # sobre todo el panel
  grid_v <- tibble(x = xc, y = ym0, yend = ym1); grid_h <- tibble(y = unname(yr), x = xp0, xend = xp1)
  p$layers <- c(list(geom_segment(data = grid_v, aes(x = x, xend = x, y = y, yend = yend), colour = "#EBEBEB", linewidth = 0.5 / k),
                     geom_segment(data = grid_h, aes(x = x, xend = xend, y = y, yend = y), colour = "#EBEBEB", linewidth = 0.5 / k)),
                p$layers)
  comp <- c(comp, "rejilla clara de la matriz")
  # complemento: tema: sin rejilla de WJP_theme (panel.grid.major explícito), nombres de conjunto 10.24 / k pt #4D4D4D
  # a 4.43 / k pt de la matriz en una columna que deja sitio al título (como alinea patchwork las columnas de título
  # y de eje), título del eje 11 / k pt negro centrado en el panel de barras y montado sobre esa columna (margen
  # derecho negativo = su alto)
  L <- mean(vapply(tl, text_width, 0, size = tit_size))       # hjust de un título girado: centro del bloque en el
  hj <- ((ym1 - (yb0 + yb1) / 2) - L / 2) / ((ym1 - yb0) - L)  # centro del panel de barras
  p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 axis.text.y = element_text(size = lab_size, colour = "#4D4D4D", hjust = 1,
                                            margin = margin(r = lmar, l = lab_w - maxw - lmar, unit = "bigpts")),
                 axis.ticks.length = unit(0, "pt"),
                 axis.title.y = element_text(size = tit_size, colour = "black", angle = 90, hjust = hj, vjust = 1,
                                             margin = margin(r = -th, unit = "bigpts")))
  comp <- c(comp, "tema: nombres de conjunto y título del eje")
  # complemento: WJPr fija "Lato Full" en capas y tema, sin argumento para cambiarla
  p <- wjpr_font(p); comp <- c(comp, "fuente Inter Tight (wjpr_font)")
  wjpr_meta(p, base = base, complementos = comp)
}

## Bloque wjpr network — figura 5 ----------------------------------------------------------------------------------
## WJPr 1.1.0 no tiene grafo de red: la base es geom_segment (aristas, ancho según lift) + geom_point (nodos) +
## geom_text (nombre y df$text) con WJP_theme(), wjp_palette() y las capas de texto de WJPr, sobre las coordenadas del
## layout de df (x, y de los nodos: network_graph.R:148-199, sin renormalizar). df trae todas las parejas (kind edge,
## value = lift): series "sig" = arista dibujada (lift > 1.2 y n >= 10, edges_sig de network_graph.R:111-112) y "no" =
## solo cuenta para la centralidad; sin columna series (df de la referencia, que solo trae las dibujadas) todas son
## "sig". Los complementos lo llevan al diseño de charts5.py:196-226 network() (la publicada): lienzo = caja de los
## nodos ± (radio 35 + margen 6) en unidades del SVG fuente, que mide 518.03 unidades por unidad de layout
## (network_graph.R; medido en la publicada), escalado al ancho de la figura (k_s = W / (518.03·Δx + 82)); el alto que
## pide el layout es k_s·(518.03·Δy + 82) pt: el h_in del manifiesto es el de lo publicado (5.869) y, si difiere en
## más de h_tol, el log da el valor para manifest/figures_<slug>.csv (con otro alto los rótulos se acercan al borde del
## nodo o quedan franjas en blanco); aristas #5D6D7E con ancho 0.8 + 3.4·t^0.8 (t = lift normalizado entre las
## dibujadas; 0.5 si todas tienen el mismo), opacidad min(0.85, (lift − 1)·0.35 + 0.2) (network_graph.R:213), de la más
## fina a la más gruesa; nodos de radio 35·k_s con relleno por centralidad (suma de los lifts de TODAS las parejas del
## nodo, sig y no, normalizada; 0.5 si el rango es 0: #A8A8C8 → #39396B, network_graph.R:116-141) y borde #2C3E50
## 1.2·k_s; rótulos bold 10.5·k_s (nombre con los cortes de network_graph.R:219-228 + text) en blanco si la luminancia
## del relleno < 0.45, interlineado 1.15. Los nodos son polígonos de 72 lados (radio exacto en datos).
## El contrato solo pone "(40%)" en text: los nombres de los nodos (con sus cortes de línea) van en rotulos y entran en
## la comparación de textos.
## Lo dibujado por fila (revisión 4, contrato extendido; wjpr_net_drawn): la red no tiene eje, así que la clave de cada
## fila es la POSICIÓN: un nodo dibujado es la fila node del df cuyo x, y está en su centro y una arista, la fila sig
## cuyos dos nodos están en sus extremos; nombre y porcentaje impresos en cada nodo, lift decodificado del ancho de cada
## arista y los nodos y aristas dibujados sin fila en el df como sobrantes.

# Polígonos de círculos (centro x, y y radio r en unidades de datos); id = índice del círculo
wjpr_circle_df <- function(x, y, r, n = 72) {
  a <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
  bind_rows(lapply(seq_along(x), function(i) tibble(x = x[i] + r * cos(a), y = y[i] + r * sin(a), id = i)))
}

# Índice del punto de (qx, qy) más cercano a cada (px, py) y su distancia (NA e Inf si no hay puntos q)
wjpr_net_nearest <- function(px, py, qx, qy) {
  if (!length(px)) return(list(i = integer(), d = numeric()))
  if (!length(qx)) return(list(i = rep(NA_integer_, length(px)), d = rep(Inf, length(px))))
  D <- sqrt(outer(px, qx, "-")^2 + outer(py, qy, "-")^2)
  i <- max.col(-D, ties.method = "first")
  list(i = i, d = D[cbind(seq_along(px), i)])
}

# Lo dibujado por fila de la red (contrato drawn de wjpr_meta; revisión 4, contrato extendido), leído de b =
# ggplot_build() del gráfico final, con la posición como clave (en unidades del layout, las de df$x, df$y):
# - nodos: el centro de cada nodo dibujado (media de los vértices de su polígono; sin complementos, el punto) es la
#   fila node del df cuyo x, y está a ≤ pos_tol; sin ninguna, una fila sobrante (kind "", label "nodo en (x, y)").
#   label_drawn = el nombre impreso en el nodo (textos visibles de la capa lay_name a ≤ r_node de su centro, de arriba
#   abajo, con los cortes de línea como espacio) y text = el porcentaje impreso (capa lay_val); val NA: el nodo, de radio
#   fijo, no codifica df$value (su relleno es la centralidad, que df no trae).
# - aristas: una fila por arista significativa del df (series "sig"): si hay un segmento con un extremo en cada uno de
#   sus dos nodos (a ≤ pos_tol de su x, y del df), frac = 1 y val = el lift decodificado del ancho dibujado (decode: la
#   inversa del ancho del diseño); si no, frac = 0 y val = 0 (no hay arista que codifique el lift: figures lo ve como
#   dibujo ≠ df, porque una arista sig tiene lift > 1.2). Cada segmento que no es el de una arista sig del df (otra
#   pareja, una "no", uno repetido o con un extremo fuera de los nodos) es una fila sobrante (kind "", label "A|B" o
#   sus extremos) con su val, que figures da como dibujo en una posición sin fila.
# order = el de la fila del df que ocupa la posición (la red no tiene orden de lectura); las sobrantes, detrás. Unos
# nombres o porcentajes intercambiados, un nodo desplazado o una arista quitada se ven aquí aunque el multiconjunto de
# textos sea el mismo y el parecido local no llegue al umbral.
wjpr_net_drawn <- function(b, nodes, edges, pos_tol, r_node, decode, lay_name, lay_val) {
  # sin nombres (los de las capas): bind_rows tomaría una lista con nombres por columnas anidadas
  geo <- function(cls) unname(which(vapply(b$plot$layers, function(l) inherits(l$geom, cls), logical(1))))
  nl <- geo(c("GeomPolygon", "GeomPoint"))
  ctr <- tibble(cx = numeric(), cy = numeric())
  if (length(nl)) {
    d <- as.data.frame(b$data[[nl[1]]])
    ctr <- if (inherits(b$plot$layers[[nl[1]]]$geom, "GeomPolygon")) {
      g <- d$group
      tibble(cx = as.numeric(tapply(d$x, g, mean)), cy = as.numeric(tapply(d$y, g, mean)))
    } else tibble(cx = as.numeric(d$x), cy = as.numeric(d$y))
  }
  m <- wjpr_net_nearest(ctr$cx, ctr$cy, nodes$x, nodes$y)
  hit <- !is.na(m$i) & m$d <= pos_tol
  tx <- wjpr_drawn_texts(b)                            # solo lo que se ve
  tx <- tx[tx$source == "layer" & tx$layer %in% c(lay_name, lay_val), , drop = FALSE]
  tn <- wjpr_net_nearest(tx$x, tx$y, ctr$cx, ctr$cy)
  tx$node <- ifelse(tn$d <= r_node, tn$i, NA_integer_)
  printed <- function(k, lay) {
    t <- tx[!is.na(tx$node) & tx$node == k & tx$layer == lay, , drop = FALSE]
    wjpr_norm_text(paste(t$label[order(-t$y)], collapse = " "))
  }
  ki <- ifelse(hit, m$i, NA_integer_)
  node_rows <- tibble(order = as.integer(nodes$order[ki]), kind = ifelse(hit, nodes$kind[ki], ""),
                      grouping = ifelse(hit, nodes$grouping[ki], ""),
                      label = ifelse(hit, nodes$label[ki], sprintf("nodo en (%.3f, %.3f)", ctr$cx, ctr$cy)),
                      frac = NA_real_, text = vapply(seq_len(nrow(ctr)), printed, "", lay = lay_val),
                      label_drawn = vapply(seq_len(nrow(ctr)), printed, "", lay = lay_name), val = NA_real_)
  segs <- bind_rows(tibble(x = numeric(), y = numeric(), xend = numeric(), yend = numeric(), lw = numeric()),
                    lapply(geo("GeomSegment"), function(i) {
                      d <- as.data.frame(b$data[[i]])
                      tibble(x = as.numeric(d$x), y = as.numeric(d$y), xend = as.numeric(d$xend), yend = as.numeric(d$yend),
                             lw = if ("linewidth" %in% names(d)) as.numeric(d$linewidth) else NA_real_)
                    }))
  a <- wjpr_net_nearest(segs$x, segs$y, nodes$x, nodes$y); z <- wjpr_net_nearest(segs$xend, segs$yend, nodes$x, nodes$y)
  ia <- ifelse(a$d <= pos_tol, a$i, NA_integer_); iz <- ifelse(z$d <= pos_tol, z$i, NA_integer_)
  pair <- ifelse(is.na(ia) | is.na(iz), NA_character_, paste(pmin(ia, iz), pmax(ia, iz)))
  e_pair <- paste(pmin(edges$i, edges$j), pmax(edges$i, edges$j))
  used <- rep(FALSE, nrow(segs)); seg_of <- rep(NA_integer_, nrow(edges))
  for (k in seq_len(nrow(edges))) {
    s <- which(!used & !is.na(pair) & pair == e_pair[k])[1]
    if (!is.na(s)) { seg_of[k] <- s; used[s] <- TRUE }
  }
  edge_rows <- tibble(order = as.integer(edges$order), kind = edges$kind, grouping = edges$grouping, label = edges$label,
                      frac = ifelse(is.na(seg_of), 0, 1), text = "", label_drawn = NA_character_,
                      val = ifelse(is.na(seg_of), 0, decode(segs$lw[seg_of])))
  sx <- which(!used)
  extra_rows <- tibble(order = rep(NA_integer_, length(sx)), kind = "", grouping = "",
                       label = as.character(ifelse(!is.na(ia[sx]) & !is.na(iz[sx]), paste(nodes$label[ia[sx]], nodes$label[iz[sx]], sep = "|"),
                                                   sprintf("arista (%.3f, %.3f)-(%.3f, %.3f)", segs$x[sx], segs$y[sx], segs$xend[sx], segs$yend[sx]))),
                       frac = rep(1, length(sx)), text = "", label_drawn = NA_character_, val = as.numeric(decode(segs$lw[sx])))
  out <- bind_rows(node_rows, edge_rows, extra_rows)
  n0 <- max(c(nodes$order, edges$order, 0L), na.rm = TRUE); free <- is.na(out$order)
  out$order[free] <- as.integer(n0 + seq_len(sum(free)))
  out %>% arrange(order) %>%
    transmute(order, kind, grouping, label, frac, frac2 = NA_real_, text, text2 = "", label_drawn,
              grouping_drawn = NA_character_, val, val2 = NA_real_)
}
wjpr_net_drawn_fun <- function(nodes, edges, pos_tol, r_node, decode, lay_name = 3L, lay_val = 4L) {
  force(nodes); force(edges); force(pos_tol); force(r_node); force(decode); force(lay_name); force(lay_val)
  function(b) wjpr_net_drawn(b, nodes, edges, pos_tol, r_node, decode, lay_name, lay_val)
}

wjpr_network <- function(df, spec, complementos = wjpr_complementos(),
                         name_lines = c("Public Services" = "Public\nServices", "Citizenship & ID" = "Citizenship\n& ID",
                                        "Money & Debt" = "Money &\nDebt", "Law Enforcement" = "Law\nEnforcement"),
                         src_scale = 518.03, h_tol = 0.01) {
  wjpr_require()
  df <- df[order(df$order), ]
  tag <- sprintf("fig %02d", as.integer(spec$fig %||% 5L))
  nodes <- df[df$kind == "node", ]; edges <- df[df$kind == "edge", ]
  ab <- strsplit(edges$label, "|", fixed = TRUE)
  # corregido: df trae todas las parejas y series marca las que se dibujan ("sig"); e_all (todas) da la centralidad,
  # como network_graph.R:116-126 (strength sobre g_full = edges_all), y e (las sig) el dibujo, como edges_sig. Antes
  # df solo traía las significativas y el relleno salía de ellas: distinto del original en 10 de los 20 países
  # dibujables (Polonia: 11 de 12 nodos; revisión V1-01). En lo publicado las 66 parejas son sig: mismo dibujo
  sig <- if ("series" %in% names(edges)) coalesce(edges$series == "sig", FALSE) else rep(TRUE, nrow(edges))
  e_all <- tibble(from = vapply(ab, `[`, "", 1), to = vapply(ab, `[`, "", 2), lift = edges$value, sig = sig) %>%
    mutate(i = match(from, nodes$label), j = match(to, nodes$label))
  if (anyNA(c(e_all$i, e_all$j))) stop("wjpr_network: aristas con nodos que no están en df", call. = FALSE)
  e_all <- e_all %>% mutate(x = nodes$x[i], y = nodes$y[i], xend = nodes$x[j], yend = nodes$y[j])
  e <- e_all %>% filter(sig)                                        # aristas dibujadas
  if (nrow(e) == 0) log_msg(tag, " aviso: ninguna pareja significativa (lift > 1.2 y n >= 10): la red sale sin aristas")
  nm <- ifelse(nodes$label %in% names(name_lines), name_lines[nodes$label], nodes$label)
  fam <- wjpr_wjp_family(); pal <- WJPr::wjp_palette()
  base <- "WJP_theme() + wjp_palette() (sin función de red)"
  p <- ggplot() +
    geom_segment(data = e, aes(x = x, y = y, xend = xend, yend = yend, linewidth = lift), colour = pal[9]) +
    geom_point(data = nodes, aes(x = x, y = y), colour = pal[1], size = 12) +
    geom_text(data = nodes %>% mutate(nm = nm), aes(x = x, y = y, label = nm), family = fam, fontface = "bold",
              size = 3.514598, colour = "#ffffff", vjust = 0) +
    geom_text(data = nodes, aes(x = x, y = y, label = text), family = fam, fontface = "bold", size = 3.514598,
              colour = "#ffffff", vjust = 1.2) +
    scale_linewidth(range = c(0.2, 2), guide = "none") +
    labs(x = NULL, y = NULL) +
    WJPr::WJP_theme()
  # corregido (revisión 4): lo dibujado por fila (wjpr_net_drawn), también sin complementos. Antes la red no lo exponía
  # y pasaban como ok dos nombres o dos porcentajes de nodo intercambiados (mismo multiconjunto de textos; parecido
  # local 0.181 y 0.021) y la arista más gruesa o la más fina quitadas (0.444, 0.035). Claves: los nodos del df (x, y)
  # y las aristas sig con los índices de sus dos nodos; tolerancia de posición 0.5 unidades del SVG fuente (≈ 0.4 pt) y
  # textos del nodo a ≤ su radio (35 unidades)
  net_nodes <- nodes %>% select(order, kind, grouping, label, x, y)
  net_edges <- tibble(order = edges$order[sig], kind = edges$kind[sig], grouping = edges$grouping[sig],
                      label = edges$label[sig], i = e$i, j = e$j)
  rl0 <- if (nrow(e) > 0) range(e$lift) else c(0, 0)
  drawn_of <- function(decode) wjpr_net_drawn_fun(net_nodes, net_edges, 0.5 / src_scale, 35 / src_scale, decode)
  # corregido (V3-07): los nombres de nodo entran en la comparación de textos (attr(p, "wjpr")$rotulos)
  if (!isTRUE(complementos)) {
    # sin complementos el ancho es el de scale_linewidth(range = c(0.2, 2)): lineal entre el lift mínimo y el máximo
    dec0 <- function(lwd) if (diff(rl0) > 0) rl0[1] + (lwd - 0.2) / 1.8 * diff(rl0) else rep(rl0[1], length(lwd))
    return(wjpr_meta(p, base = base, rotulos = nm, drawn = drawn_of(dec0)))
  }

  comp <- character()
  W <- spec$w_in * 72
  dx <- diff(range(nodes$x)); ks <- W / (src_scale * dx + 82)      # pt de la figura por unidad del SVG fuente
  b <- ks * src_scale                                               # pt por unidad de layout
  r <- 35 / src_scale; pad <- (35 + 6) / src_scale                  # radio y margen en unidades de layout
  lw <- function(pt) pt / (0.75 * ggplot2::.pt)                     # ancho en pt → linewidth de ggplot
  # corregido: el alto del lienzo depende del layout de cada país y el manifiesto trae el de lo publicado: si el que
  # pide el layout difiere en más de h_tol, el log da el valor para fijarlo por país (antes la figura salía deformada
  # con estado ok y sin aviso: Finlandia 6.402 in, Polonia 5.512; revisión V4-02). Lo publicado: 5.8693 frente a 5.869
  h_need <- ks * (src_scale * diff(range(nodes$y)) + 82) / 72
  if (is.finite(h_need) && abs(h_need - spec$h_in) > h_tol) {
    log_msg(tag, " aviso: el layout de ", spec$country %||% "este país", " pide h_in = ", sprintf("%.3f", h_need),
            " in y el manifiesto da ", sprintf("%.3f", spec$h_in), " (",
            if (h_need > spec$h_in) "los rótulos se acercan al borde de los nodos y quedan franjas a los lados" else
              "quedan franjas en blanco arriba y abajo",
            "): fíjelo en manifest/figures_<slug>.csv con la fila «", spec$fig, ",", sprintf("%.3f", h_need),
            "» (columnas fig,h_in)")
  }
  # complemento: aristas del diseño (ancho 0.8-4.2 con gamma 0.8 según el lift, opacidad según el lift, #5D6D7E,
  # extremos redondeados, de la más fina a la más gruesa); WJPr no tiene capa de aristas
  # corregido: t = 0.5 si las dibujadas tienen todas el mismo lift (una sola arista: antes 0/0 = NaN)
  rl <- if (nrow(e) > 0) range(e$lift) else c(0, 0)
  e <- e %>% mutate(t = if (diff(rl) > 0) (lift - rl[1]) / diff(rl) else 0.5, w = 0.8 + (4.2 - 0.8) * t^0.8,
                    alpha = pmin(0.85, (lift - 1) * 0.35 + 0.2)) %>% arrange(w)
  p$layers[[1]] <- geom_segment(data = e, aes(x = x, y = y, xend = xend, yend = yend, linewidth = lw(w * ks), alpha = alpha),
                                colour = "#5D6D7E", lineend = "round")
  p <- suppressMessages(p + scale_linewidth_identity() + scale_alpha_identity())
  comp <- c(comp, "aristas: ancho y opacidad según el lift")
  # complemento: nodos de radio fijo en datos (polígonos) con relleno por centralidad (#A8A8C8 → #39396B) y borde
  # #2C3E50; WJPr no tiene escala de centralidad ni el degradado del reporte. Centralidad = suma de los lifts de todas
  # las parejas del nodo (e_all, network_graph.R:116-126), no solo de las dibujadas
  strength <- vapply(seq_len(nrow(nodes)), function(i) sum(e_all$lift[e_all$i == i | e_all$j == i], na.rm = TRUE), 0)
  cn <- if (diff(range(strength)) == 0) rep(0.5, nrow(nodes)) else (strength - min(strength)) / diff(range(strength))
  fill <- wjpr_mix_rgb("#A8A8C8", "#39396B", cn)
  circ <- wjpr_circle_df(nodes$x, nodes$y, r) %>% mutate(fill = fill[id])
  p$layers[[2]] <- geom_polygon(data = circ, aes(x = x, y = y, group = id, fill = fill), colour = "#2C3E50",
                                linewidth = lw(1.2 * ks))
  p <- p + scale_fill_identity()
  comp <- c(comp, "nodos: radio del diseño y relleno por centralidad")
  # complemento: rótulos bold 10.5·k_s centrados en el nodo (nombre en sus líneas + text debajo, interlineado 1.15) en
  # blanco si el relleno es oscuro (luminancia < 0.45) y #1A1A1A si no; WJPr los dibuja a 10 pt en un color fijo
  rgbm <- grDevices::col2rgb(fill) / 255
  ink <- ifelse(0.2126 * rgbm[1, ] + 0.7152 * rgbm[2, ] + 0.0722 * rgbm[3, ] < 0.45, "#FFFFFF", STD$col$text)
  size <- 10.5 * ks; step <- size * 1.15
  nl <- lengths(strsplit(nm, "\n", fixed = TRUE)) + 1                # líneas por nodo (nombre + valor)
  y_first <- -(step * (nl - 1) / 2) + size * 0.36                    # pt bajo el centro (y de la figura hacia abajo)
  lab <- nodes %>% mutate(nm = nm, ink = ink, y_nm = y - (y_first + step * (nl - 2)) / b, y_tx = y - (y_first + step * (nl - 1)) / b)
  p$layers[[3]] <- geom_text(data = lab, aes(x = x, y = y_nm, label = nm, colour = ink), family = fam, fontface = "bold",
                             size = size / ggplot2::.pt, lineheight = 1.15 / 1.2, vjust = 0)
  p$layers[[4]] <- geom_text(data = lab, aes(x = x, y = y_tx, label = text, colour = ink), family = fam, fontface = "bold",
                             size = size / ggplot2::.pt, vjust = 0)
  p <- p + scale_colour_identity()
  comp <- c(comp, "rótulos: tamaño, líneas y color según el relleno")
  # complemento: lienzo = caja de los nodos ± (radio + margen) con escala igual en x e y y sin ejes ni rejilla (WJP_theme
  # da ejes, rejilla discontinua y panel.grid.major explícito)
  p <- p + coord_fixed(xlim = range(nodes$x) + c(-pad, pad), ylim = range(nodes$y) + c(-pad, pad), expand = FALSE) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_blank(), plot.margin = margin(0, 0, 0, 0))
  comp <- c(comp, "lienzo de la red (coord_fixed, sin ejes)")
  # complemento: WJPr fija "Lato Full" en capas y tema, sin argumento para cambiarla
  p <- wjpr_font(p); comp <- c(comp, "fuente Inter Tight (wjpr_font)")
  # lo dibujado por fila: el lift de cada segmento sale de su ancho invirtiendo el del diseño (linewidth = lw(w·k_s),
  # w = 0.8 + 3.4·t^0.8, t = (lift − mín) / rango de las dibujadas; con un solo lift, el mínimo)
  dec <- function(lwd) {
    w <- lwd * 0.75 * ggplot2::.pt / ks
    if (diff(rl) > 0) rl[1] + (pmax(w - 0.8, 0) / (4.2 - 0.8))^(1 / 0.8) * diff(rl) else rep(rl[1], length(lwd))
  }
  wjpr_meta(p, base = base, complementos = comp, rotulos = nm, drawn = drawn_of(dec))
}
## Prueba del bloque (revisión 3, grupo F2, 2026-10-23; pruebas/p04_red.R y p05_barrido.R): relleno de los 12 nodos
## frente al original (network_graph.R:8-141, strength sobre las 66 parejas) en los 26 países con módulo LNS: 0 nodos
## distintos (antes 10-12 de 12 en Croatia, Cyprus, Czechia, Estonia, Finland, Luxembourg, Poland, Romania, Slovenia y
## Sweden); Bulgaria, Greece, Latvia, Lithuania, Malta y Portugal, antes «Weights must be positive», ahora se dibujan
## (aviso con sus parejas de lift 0). Lo publicado: df de antes + series (66 sig) y SVG idéntico byte a byte. Alto:
## aviso en 24 países (Finland 6.402, Malta 7.017, Poland 5.512…), ninguno en lo publicado (5.8693) ni en Slovenia
## (5.875); con manifest/figures_finland.csv «5,6.402» el aviso desaparece. Una sola arista sig: ancho finito (antes
## 0/0 = NaN); ninguna: red sin aristas, con aviso.
## Rótulos (2026-10-25, main.R --stage all --figs 05 en las dos builds): con rotulos = los 12 nombres de nodo, textos
## «iguales» (antes solo se comparaban los 12 «(NN%)»); SVG igual byte a byte.
## Lo dibujado por fila (revisión 4, 2026-10-27; SCRATCH/fix4/G3/pruebas/t_drawn.R, iny_g3.sh): 78 filas en lo publicado
## (12 nodos con label_drawn y text, 66 aristas con frac 1 y val = lift decodificado, |Δ| ≈ 0 frente a df), 0 texto y 0
## dibujo en figures; verify 201/201 (antes 123: + 12 label_drawn, 66 val y el orden de drawn.csv). Cada inyección, una
## a la vez y solo en el dibujo, con --stage all --figs 05 --strict → exit 1 y «fallo · figures: …», con el multiconjunto
## de textos «iguales» en todas: nombres Consumer/Land intercambiados (texto · label_drawn), porcentajes (40%)/(18%)
## (texto · text), arista más gruesa quitada (dibujo · val Law Enforcement|Citizenship & ID 11.78→0), la más fina
## (dibujo · val Consumer|Public Services 1.57→0), Education desplazado 0.06 unidades con rótulos y aristas (texto: su
## fila sin nodo y un nodo sobrante; dibujo: 11 aristas → 0 y 11 segmentos sobrantes). Sin complementos los rótulos
## blancos sobre el geom_point no cuentan como visibles (wjpr_drawn_texts no trata los puntos como marca): label_drawn y
## text "", igual que el multiconjunto («faltan 24»). Galería --figs 5: «78 filas ok». SVG y PNG idénticos (md5).

## Bloque wjpr sankey — figuras 11, 16 -----------------------------------------------------------------------------
## WJPr 1.1.0 no tiene sankey (ni aluvial). Viable con ggplot: la geometría de chart_sankey (05_charts.R, bloque
## sankey: sankey_def(), ST_SANKEY) se pasa a capas de ggplot en coordenadas de la figura (pt, y hacia abajo): flujos =
## polígonos de las mismas curvas de Bézier cúbicas del SVG (40 puntos por borde), nodos = geom_rect, cajas =
## polígonos de esquina 2.5 pt con borde (wjpr_round_rect_df del bloque cards), textos = geom_text con la línea base
## de box() (etiqueta 10 pt, valor = df$text 10 pt semibold cursiva, nota 8.75 pt cursiva) y encabezados de columna
## como texto del eje x superior (no son capas). Base: esa geometría con WJP_theme(), wjp_palette() (resultados:
## violeta, azul verdoso, gris) y los textos de WJPr ("Lato Full", 3.514598, #4a4a49); los complementos ponen los
## colores, opacidades, cajas, tamaños y estilos del diseño. which (assistance | drm) se deduce de los nodos de la
## columna 2 ("turned" = drm). Las etiquetas, encabezados y notas son el texto fijo de sankey_def(): la galería cuenta
## las etiquetas y la nota como texto sobrante (el contrato solo pone los valores en text).
## Lo dibujado por fila (revisión 4, contrato extendido; wjpr_sk_drawn): una fila por barra de nodo, con la fila del df
## que da su posición (columna y puesto de arriba abajo), el valor y la etiqueta impresos en su caja y su alto en % en
## las columnas 1 y 2 (en la 3 el alto no es df$value). Alto del lienzo: si la caja o el texto más bajo pasa del alto
## del manifiesto (el de lo publicado, 372 pt), el log da el h_in que lo contiene, como la 5.

# Banda entre (xa, [ya0, ya1]) y (xb, [yb0, yb1]) con las curvas del SVG (M … C xm ya0 xm yb0 xb yb0 L … C … Z)
wjpr_bezier_band <- function(xa, ya0, ya1, xb, yb0, yb1, n = 40) {
  t <- seq(0, 1, length.out = n); xm <- (xa + xb) / 2
  bez <- function(p0, p1, p2, p3) (1 - t)^3 * p0 + 3 * (1 - t)^2 * t * p1 + 3 * (1 - t) * t^2 * p2 + t^3 * p3
  tibble(x = c(bez(xa, xm, xm, xb), bez(xb, xm, xm, xa)), y = c(bez(ya0, ya0, yb0, yb0), bez(yb1, yb1, ya1, ya1)))
}

# Límites de las cajas de una capa construida (rect: una por fila; polígono: una por grupo, en orden de aparición),
# en el orden de las columnas de wjpr_marks_at()
wjpr_sk_box_bounds <- function(d) {
  if (all(c("xmin", "xmax", "ymin", "ymax") %in% names(d))) {
    return(tibble(x0 = pmin(d$xmin, d$xmax), x1 = pmax(d$xmin, d$xmax), y0 = pmin(d$ymin, d$ymax), y1 = pmax(d$ymin, d$ymax)))
  }
  g <- if ("group" %in% names(d)) d$group else rep(1L, nrow(d))
  bind_rows(tibble(x0 = numeric(), x1 = numeric(), y0 = numeric(), y1 = numeric()),
            lapply(unique(g), function(id) {
              k <- g == id
              tibble(x0 = min(d$x[k]), x1 = max(d$x[k]), y0 = min(d$y[k]), y1 = max(d$y[k]))
            }))
}

# Lo dibujado por fila del sankey (contrato drawn de wjpr_meta; revisión 4, contrato extendido), leído de b =
# ggplot_build() del gráfico final (coordenadas construidas: la escala y va invertida, y = −pt, así que la y mayor es la
# de arriba): cada barra de nodo (rectángulo de ancho BAR; sin complementos las cajas también son geom_rect, más
# anchas) está en la columna cuya x es la suya (X = X1, X2, X3) y, dentro de ella, en un puesto de arriba abajo; la fila
# es el nodo del diseño en ese puesto (sankey_def: order1, order2, order3) — la posición, no el df en orden — y order su
# puesto visual (columnas de izquierda a derecha, cada una de arriba abajo: el del df); sin nodo del diseño en esa
# posición, una fila sobrante (kind ""). frac = alto dibujado / alto del 100 % (sc · 100 pt; como el drawn de
# chart_sankey) y val = alto / sc (el % que codifica): en las columnas 1 y 2 el diseño es proporcional (alto =
# df$value · sc); en la 3 val es NA, porque su alto es la suma de los nodos de la columna 2 que llegan (s$M, % de todos
# los encuestados) y df$value es otra cifra (access, no_access: % de quienes lo necesitaban; excluded: la suma
# redondeada), así que el alto no codifica df$value (una barra sobrante sí lleva su val). La caja del nodo es la de la
# capa lay_box que contiene el centro de la barra (si ninguna, la de su columna que solapa la barra con el centro más
# cercano): text = el valor impreso y visible en ella (capa lay_val), label_drawn = la clave del nodo cuya etiqueta del
# diseño (LAB1, LAB2, LAB3: primero las de su columna) es la impresa en ella (capa lay_lab, líneas de arriba abajo; si
# no es ninguna, lo impreso tal cual; "" si nada) y grouping_drawn = la columna ("1", "2", "3") cuyo encabezado del
# diseño (s$headers) es el impreso sobre la barra (eje x superior; si no es ninguno, lo impreso; "" si nada). Dos
# valores o dos etiquetas intercambiados o una barra con el alto de otra se ven aquí aunque el multiconjunto de textos
# sea el mismo.
wjpr_sk_drawn <- function(b, s, keys, X, BAR, sc, lay_box = 3L, lay_lab = 4L, lay_val = 5L) {
  geo <- function(cls) unname(which(vapply(b$plot$layers, function(l) inherits(l$geom, cls), logical(1))))
  bars <- bind_rows(tibble(x0 = numeric(), x1 = numeric(), y0 = numeric(), y1 = numeric()),
                    lapply(geo("GeomRect"), function(i) wjpr_sk_box_bounds(as.data.frame(b$data[[i]]))))
  bars <- bars[abs((bars$x1 - bars$x0) - BAR) < 0.01, , drop = FALSE]
  ords <- list(s$order1, s$order2, s$order3); off <- cumsum(c(0, lengths(ords)))
  bars$col <- vapply(bars$x0, function(x) { k <- which(abs(x - X) < 0.5); if (length(k)) k[1] else NA_integer_ }, 0L)
  bars$rank <- NA_integer_
  for (cl in unique(stats::na.omit(bars$col))) {
    k <- which(bars$col %in% cl); bars$rank[k] <- as.integer(rank(-bars$y1[k], ties.method = "first"))
  }
  key <- vapply(seq_len(nrow(bars)), function(k) {
    cl <- bars$col[k]; r <- bars$rank[k]
    if (is.na(cl) || r > length(ords[[cl]])) NA_character_ else ords[[cl]][r]
  }, "")
  ki <- ifelse(is.na(key), NA_integer_, match(paste(bars$col, key), paste(keys$grouping, keys$label)))
  hit <- !is.na(ki)
  # caja de cada barra y textos visibles de cada caja
  db <- if (length(b$data) >= lay_box) as.data.frame(b$data[[lay_box]]) else data.frame()
  gb <- if (length(b$data) >= lay_box) b$plot$layers[[lay_box]]$geom else NULL
  bb <- if (nrow(db)) wjpr_sk_box_bounds(db) else tibble(x0 = numeric(), x1 = numeric(), y0 = numeric(), y1 = numeric())
  cx <- (bars$x0 + bars$x1) / 2; cy <- (bars$y0 + bars$y1) / 2
  mk <- if (nrow(db)) wjpr_marks_at(db, gb, cx, cy) else NULL
  box_of <- vapply(seq_len(nrow(bars)), function(k) {
    h <- if (!is.null(mk)) which(mk$inside[k, ]) else integer()
    if (length(h)) return(h[1])
    cand <- which(bb$x0 <= cx[k] & bb$x1 >= cx[k] & bb$y0 <= bars$y1[k] & bb$y1 >= bars$y0[k])
    if (!length(cand)) return(NA_integer_)
    cand[which.min(abs((bb$y0[cand] + bb$y1[cand]) / 2 - cy[k]))]
  }, 0L)
  tx <- wjpr_drawn_texts(b)                            # solo lo que se ve
  ax <- tx[tx$source == "axis-x", , drop = FALSE]
  tl <- tx[tx$source == "layer" & tx$layer %in% c(lay_lab, lay_val), , drop = FALSE]
  mt <- if (nrow(db) && nrow(tl)) wjpr_marks_at(db, gb, tl$x, tl$y) else NULL
  tl$box <- if (is.null(mt)) rep(NA_integer_, nrow(tl)) else
    apply(mt$inside, 1, function(r) { h <- which(r); if (length(h)) h[1] else NA_integer_ })
  in_box <- function(k, lay) {
    if (is.na(box_of[k])) return("")
    t <- tl[!is.na(tl$box) & tl$box == box_of[k] & tl$layer == lay, , drop = FALSE]
    wjpr_norm_text(paste(t$label[order(-t$y)], collapse = " "))
  }
  labs <- list(s$LAB1, s$LAB2, s$LAB3)
  dict_of <- function(l) setNames(names(l), wjpr_norm_text(vapply(l, paste, "", collapse = " ")))
  dict_all <- dict_of(c(s$LAB1, s$LAB2, s$LAB3))
  lab_key <- function(x, c) {
    if (!nzchar(x)) return("")
    own <- if (!is.na(c)) dict_of(labs[[c]]) else character()
    if (x %in% names(own)) unname(own[[x]]) else if (x %in% names(dict_all)) unname(dict_all[[x]]) else x
  }
  hdict <- setNames(as.character(seq_along(s$headers)), wjpr_norm_text(vapply(s$headers, paste, "", collapse = " ")))
  head_of <- function(k) {
    a <- ax[is.finite(ax$x) & abs(ax$x - cx[k]) <= BAR, , drop = FALSE]
    if (!nrow(a)) return("")
    h <- wjpr_norm_text(a$label[which.min(abs(a$x - cx[k]))])
    if (h %in% names(hdict)) unname(hdict[[h]]) else h
  }
  hgt <- bars$y1 - bars$y0
  n0 <- max(c(off[length(off)], keys$order, 0L), na.rm = TRUE)
  ord <- ifelse(hit, off[pmax(1L, coalesce(bars$col, 1L))] + bars$rank, NA_integer_)
  ord[!hit] <- as.integer(n0 + seq_len(sum(!hit)))
  tibble(order = as.integer(ord), kind = ifelse(hit, keys$kind[ki], ""), grouping = ifelse(hit, keys$grouping[ki], ""),
         label = as.character(ifelse(hit, keys$label[ki], sprintf("barra en x = %.1f, puesto %s", bars$x0, bars$rank))),
         frac = hgt / (100 * sc), frac2 = NA_real_,
         text = vapply(seq_len(nrow(bars)), in_box, "", lay = lay_val), text2 = "",
         label_drawn = vapply(seq_len(nrow(bars)), function(k) lab_key(in_box(k, lay_lab), bars$col[k]), ""),
         grouping_drawn = vapply(seq_len(nrow(bars)), head_of, ""),
         val = ifelse(!hit | bars$col %in% 1:2, hgt / sc, NA_real_), val2 = NA_real_) %>%
    arrange(order)
}
wjpr_sk_drawn_fun <- function(s, keys, X, BAR, sc) {
  force(s); force(keys); force(X); force(BAR); force(sc)
  function(b) wjpr_sk_drawn(b, s, keys, X, BAR, sc)
}

wjpr_sankey <- function(df, spec, complementos = wjpr_complementos(), which = NULL, st = ST_SANKEY) {
  wjpr_require()
  df <- df[order(df$order), ]
  which <- which %||% if ("turned" %in% df$label[df$grouping == "2"]) "drm" else "assistance"
  s <- sankey_def(which)
  node_vals <- function(g, keys, col) {
    r <- df[df$grouping == g, ]; i <- match(keys, r$label)
    if (anyNA(i)) stop("wjpr_sankey: faltan en df los nodos ", paste(keys[is.na(i)], collapse = ", "), call. = FALSE)
    setNames(r[[col]][i], keys)
  }
  C1 <- node_vals("1", s$order1, "value"); val2 <- node_vals("2", s$order2, "value")
  if (anyNA(c(C1, val2))) stop("wjpr_sankey: nodo sin valor en las columnas 1-2", call. = FALSE)
  txt <- list(node_vals("1", s$order1, "text"), node_vals("2", s$order2, "text"), node_vals("3", s$order3, "text"))
  W <- spec$w_in * 72; H <- spec$h_in * 72
  TOP <- st$sankey_top; GAP <- 9; BAR <- 11
  X1 <- st$sankey_margin; X3 <- W - st$sankey_margin - BAR; X2 <- (X1 + X3) / 2
  val3 <- vapply(s$order3, function(o) sum(val2[s$order2[unlist(s$M[s$order2]) == o]]), 0)
  sc <- (st$sankey_h - GAP * (length(s$order2) - 1)) / 100
  lay <- function(values, ord) {
    used <- sum(values[ord]) * sc + GAP * (length(ord) - 1)
    y <- TOP + (st$sankey_h - used) / 2; pos <- list()
    for (k in ord) { pos[[k]] <- c(y, y + values[[k]] * sc); y <- y + values[[k]] * sc + GAP }
    pos
  }
  p1 <- lay(C1, s$order1); p2 <- lay(val2, s$order2); p3 <- lay(val3, s$order3)
  # flujos: columna 1 → 2 (opacidad 0.30) y 2 → 3 (0.45), con el resultado (s$M) de cada nodo de la columna 2
  bands <- list(); cur1 <- vapply(p1, `[`, 0, 1); cur3 <- vapply(p3, `[`, 0, 1)
  for (k in s$order2) {
    h <- val2[[k]] * sc; par <- s$C2[[k]]; o <- s$M[[k]]
    bands[[length(bands) + 1]] <- wjpr_bezier_band(X1 + BAR, cur1[[par]], cur1[[par]] + h, X2, p2[[k]][1], p2[[k]][2]) %>%
      mutate(id = length(bands) + 1, out = o, op = 0.30)
    cur1[[par]] <- cur1[[par]] + h
  }
  for (k in s$order2) {
    h <- val2[[k]] * sc; o <- s$M[[k]]
    bands[[length(bands) + 1]] <- wjpr_bezier_band(X2 + BAR, p2[[k]][1], p2[[k]][2], X3, cur3[[o]], cur3[[o]] + h) %>%
      mutate(id = length(bands) + 1, out = o, op = 0.45)
    cur3[[o]] <- cur3[[o]] + h
  }
  bands <- bind_rows(bands)
  rects <- bind_rows(
    tibble(x = X1, y0 = vapply(p1, `[`, 0, 1), y1 = vapply(p1, `[`, 0, 2), out = "node"),
    tibble(x = X2, y0 = vapply(p2, `[`, 0, 1), y1 = vapply(p2, `[`, 0, 2), out = unlist(s$M[s$order2])),
    tibble(x = X3, y0 = vapply(p3, `[`, 0, 1), y1 = vapply(p3, `[`, 0, 2), out = s$order3))
  # cajas: líneas de etiqueta, valor (df$text) y nota, con el ancho y la línea base de box() (chart_sankey)
  size <- st$size_label; lh <- size * 1.22; ns <- st$sankey_note
  boxes <- list(); rows <- list()
  add_box <- function(xc, yc, lines, val, note, val_col) {
    r <- c(lines, val, note); kd <- c(rep("lab", length(lines)), "val", rep("note", length(note)))
    wd <- mapply(function(t, k) switch(k, lab = text_width(t, size), val = text_width(t, size, 600, TRUE),
                                       note = text_width(t, ns, 400, TRUE)), r, kd)
    w <- max(wd) + 14; h <- lh * length(r) + 6; x0 <- xc - w / 2; y0 <- yc - h / 2
    boxes[[length(boxes) + 1]] <<- tibble(x0 = x0, y0 = y0, w = w, h = h)
    rows[[length(rows) + 1]] <<- tibble(x = xc, y = y0 + 3 + lh * 0.8 + lh * (seq_along(r) - 1), label = r, kind = kd, val_col = val_col)
  }
  for (k in s$order1) add_box(X1 + BAR / 2, mean(p1[[k]]), s$LAB1[[k]], txt[[1]][[k]], NULL, st$col$main)
  for (k in s$order2) add_box(X2 + BAR / 2, mean(p2[[k]]), s$LAB2[[k]], txt[[2]][[k]], NULL, st$col$main)
  for (o in s$order3) add_box(X3 + BAR / 2, mean(p3[[o]]), s$LAB3[[o]], txt[[3]][[o]], s$NOTE3[[o]],
                              if (o == "excluded") st$col$eu_text else st$col$main)
  boxes <- bind_rows(boxes); rows <- bind_rows(rows)
  heads <- vapply(s$headers, paste, "", collapse = "\n"); xh <- c(X1, X2, X3) + BAR / 2
  fam <- wjpr_wjp_family(); pal <- WJPr::wjp_palette()
  base <- "WJP_theme() + wjp_palette() (sin función de sankey; geometría de chart_sankey en ggplot)"
  pal_out <- c(access = pal[1], no_access = pal[2], excluded = pal[9], node = pal[9])
  p <- ggplot() +
    geom_polygon(data = bands, aes(x = x, y = y, group = id, fill = out), alpha = 0.5) +
    geom_rect(data = rects, aes(xmin = x, xmax = x + BAR, ymin = y0, ymax = y1, fill = out)) +
    geom_rect(data = boxes, aes(xmin = x0, xmax = x0 + w, ymin = y0, ymax = y0 + h), fill = "white", colour = "#4a4a49") +
    geom_text(data = rows %>% filter(kind == "lab"), aes(x = x, y = y, label = label), family = fam, size = 3.514598,
              colour = "#4a4a49", vjust = 0) +
    geom_text(data = rows %>% filter(kind == "val"), aes(x = x, y = y, label = label), family = fam, fontface = "bold",
              size = 3.514598, colour = "#4a4a49", vjust = 0) +
    geom_text(data = rows %>% filter(kind == "note"), aes(x = x, y = y, label = label), family = fam, size = 3.514598,
              colour = "#4a4a49", vjust = 0) +
    scale_fill_manual(values = pal_out, guide = "none") +
    scale_x_continuous(limits = c(0, W), expand = c(0, 0), breaks = xh, labels = heads, position = "top", oob = scales::oob_keep) +
    scale_y_reverse(limits = c(H, 50), expand = c(0, 0), breaks = NULL, oob = scales::oob_keep) +
    coord_cartesian(clip = "off") + labs(x = NULL, y = NULL) +
    WJPr::WJP_theme()
  # corregido (revisión 4): lo dibujado por fila (wjpr_sk_drawn), también sin complementos. Antes el sankey no lo
  # exponía y dos valores o dos etiquetas de caja intercambiados o una barra con el alto de otra pasaban como ok
  # (mismo multiconjunto de textos). Capas: 2 = barras de nodo, 3 = cajas, 4 = etiquetas, 5 = valores
  drawn <- wjpr_sk_drawn_fun(s, df %>% select(order, kind, grouping, label), c(X1, X2, X3), BAR, sc)
  if (!isTRUE(complementos)) return(wjpr_meta(p, base = base, drawn = drawn))

  comp <- character()
  lw <- function(pt) pt / (0.75 * ggplot2::.pt)
  # complemento: colores del diseño (acceso main, sin acceso eu #A8A8C8, excluidos #C4C8C8, nodos de la columna 1
  # #8E9393) y opacidad de cada tramo (0.30 de la columna 1 a la 2, 0.45 de la 2 a la 3); WJPr no tiene la paleta del
  # reporte ni escala de opacidad por capa
  cols <- c(access = st$col$main, no_access = st$col$eu, excluded = st$col$excl, node = st$col$node)
  p$layers[[1]] <- geom_polygon(data = bands, aes(x = x, y = y, group = id, fill = out, alpha = op))
  p <- suppressMessages(p + scale_fill_manual(values = cols, guide = "none") + scale_alpha_identity())
  comp <- c(comp, "colores y opacidades del diseño")
  # complemento: cajas redondeadas (2.5 pt) #F2F3F3 con borde #1A1A1A de 0.6 pt; geom_rect no redondea
  rr <- wjpr_round_rect_df(boxes$x0, boxes$y0, boxes$w, boxes$h, 2.5)
  p$layers[[3]] <- geom_polygon(data = rr, aes(x = x, y = y, group = id), fill = st$col$box, colour = st$col$text,
                                linewidth = lw(0.6))
  comp <- c(comp, "cajas redondeadas con borde")
  # complemento: textos de caja del diseño: etiqueta 10 pt #1A1A1A, valor 10 pt semibold cursiva main (eu_text en los
  # excluidos), nota 8.75 pt cursiva #7D7DA8
  p$layers[[4]] <- geom_text(data = rows %>% filter(kind == "lab"), aes(x = x, y = y, label = label), family = fam,
                             size = size / ggplot2::.pt, colour = st$col$text, vjust = 0)
  p$layers[[5]] <- geom_text(data = rows %>% filter(kind == "val"), aes(x = x, y = y, label = label, colour = val_col),
                             family = fam, fontface = "bold.italic", size = size / ggplot2::.pt, vjust = 0)
  p$layers[[6]] <- geom_text(data = rows %>% filter(kind == "note"), aes(x = x, y = y, label = label), family = fam,
                             fontface = "italic", size = ns / ggplot2::.pt, colour = st$col$note, vjust = 0)
  p <- p + scale_colour_identity()
  comp <- c(comp, "textos de caja: tamaños, estilos y colores")
  # complemento: encabezados de columna como eje x superior: 10.8 pt bold cursiva main, interlineado 1.25, primera
  # línea a 14 pt del borde y el panel desde y = 50 (WJP_theme: 10 pt #524F4C), sin rejilla (panel.grid.major
  # explícito en WJP_theme)
  hs <- st$sankey_head
  p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks.length = unit(0, "pt"),
                 axis.text.x.top = element_text(size = hs, colour = st$col$main, face = "bold.italic", lineheight = 1.25 / 1.2,
                                                vjust = 1, margin = margin(b = 36 - 0.2148 * hs - (max(lengths(s$headers)) - 1) * 1.25 * hs,
                                                                           unit = "bigpts")),
                 plot.margin = margin(14 - 0.727 * hs, 0, 0, 0, unit = "bigpts"))
  comp <- c(comp, "encabezados de columna (eje superior)")
  # complemento: WJPr fija "Lato Full" en capas y tema, sin argumento para cambiarla
  p <- wjpr_font(p); comp <- c(comp, "fuente Inter Tight (wjpr_font)")
  # complemento: encabezados en Bold Italic 700 como el diseño (familia inter_bi700, bloque cards)
  p <- p + theme(axis.text.x.top = element_text(family = wjpr_family_bi700(), face = "italic"))
  comp <- c(comp, "encabezados 700 cursiva (familia inter_bi700)")
  # corregido (revisión 4): el lienzo mide el h_in del manifiesto (el de lo publicado, 372 pt) y cada caja se centra en
  # su nodo, así que con nodos bajos al fondo la caja más baja (o su texto) pasaba del borde inferior y se cortaba sin
  # aviso (hasta 5.3 pt en 12 de 26 países: Malta 5.3, Portugal 4.9, Greece y Cyprus 4.6; Hungría acaba a 366.6 pt).
  # Como en la 5, si pasa el log da el alto que la contiene con 3 pt de margen (redondeado hacia arriba a 3 decimales)
  # para manifest/figures_<slug>.csv; el dibujo no cambia (con ese alto el lienzo solo crece hacia abajo: la escala y
  # va de 50 a H pt a 1 pt por unidad). Fondo de la caja = y0 + h + la mitad de su borde de 0.6 pt; del texto, su línea
  # base + 0.25 em (descendentes)
  tag <- sprintf("fig %02d", as.integer(spec$fig %||% 11L))
  bottom <- max(c(boxes$y0 + boxes$h + 0.6 / 2, rows$y + 0.25 * ifelse(rows$kind == "note", ns, size)))
  if (is.finite(bottom) && bottom > H + 1e-6) {
    h_need <- ceiling((bottom + 3) / 72 * 1000) / 1000
    log_msg(tag, " aviso: el layout de ", spec$country %||% "este país", " pide h_in = ", sprintf("%.3f", h_need),
            " in y el manifiesto da ", sprintf("%.3f", spec$h_in), " (la caja o el texto más bajo acaba a ",
            sprintf("%.1f", bottom), " pt y el lienzo mide ", sprintf("%.1f", H), ": se corta por abajo): fíjelo en ",
            "manifest/figures_<slug>.csv con la fila «", spec$fig, ",", sprintf("%.3f", h_need), "» (columnas fig,h_in)")
  }
  wjpr_meta(p, base = base, complementos = comp, drawn = drawn)
}
## Prueba del bloque (revisión 4, 2026-10-27; SCRATCH/fix4/G3/pruebas/t_drawn.R, iny_g3.sh, t_sankey_h.R): lo dibujado
## por fila da 10 filas en la 11 (data()) y 9 en la 16 (referencia), con y sin complementos: text, label_drawn y
## grouping_drawn iguales a df y val = df$value en las columnas 1-2 (NA en la 3: en la 16 excluded mide 55 y df dice 56);
## verify de la 11: 56/56 (antes 29). Inyecciones, una a la vez con --stage all --figs 11 --strict → exit 1 y «fallo ·
## figures: …», multiconjunto «iguales»: valores appropriate/needed_no intercambiados (texto · text 21%→38%), etiquetas
## Appropriate/Non-appropriate adviser (texto · label_drawn), barra de appropriate con el alto de needed_no (dibujo · val
## 21.4→37.8). Alto: aviso en Malta (caja hasta 377.6 pt de 372 → «11,5.287»), Portugal (377.2 → «11,5.282») y Greece
## (377.0 → «11,5.278»), ninguno en Hungría (366.9 con el borde); con esa fila en manifest/figures_<slug>.csv no hay
## aviso y la caja cabe con 3.0 pt de margen. Galería --figs 11,16: «10 filas ok», «9 filas ok». SVG y PNG de Hungría
## idénticos (md5).


## Prueba
## Infraestructura (2026-10-23): ver tools/wjpr_gallery.R (galería de las figuras con WJPr y su fidelidad).
## Bloque grouped_bars (agente A, 2026-10-23): galería --figs 1,4,9,12,17,20-28,31,33,35,38-40 → 20 ok, 0 dudosos,
## textos iguales; f1_publicada 1.000 en las 20 (sin complementos 0.17-0.79).
## Bloques dumbbell y hbars (agente B, 2026-10-23): galería --figs 10,15,13,14,18,19,36 → 7 ok, 0 dudosos, textos
## iguales; f1_publicada 1.000 (sin complementos 0.13-0.86). En hbars, wjp_bars supera a wjp_edgebars y wjp_lollipops.
## Bloques paired_bars, columns, severity_bars y funnel (agente C, 2026-10-23): galería --figs 2,7,8,30,34,37 → 6 ok,
## 0 dudosos, textos iguales; f1_publicada 0.998-1.000 (sin complementos 0.38-0.71).
## Bloques cards, matrix, scatter, upset, network y sankey (agente D, 2026-10-23): cd /tmp && Rscript
## src/src_reports/tools/wjpr_gallery.R --figs 3,5,6,11,16,29,32,41,42 → 9 ok, 0 dudosos, 0 <text>; f1_publicada 1.000
## en 3, 5, 6, 11, 32, 41, 42; 0.997 en 16 (referencia con valores enteros; con rows_sankey(journey(ctx), "drm") 1.000,
## mad 0.0017) y 0.996 en 29. Sin complementos (options(wjpr.complementos = FALSE)): 0.22-0.89 y 21-173 textos
## dudosos. Textos: iguales en 29, 32, 41, 42; la galería cuenta como sobrantes los nombres/etiquetas fijas de 3, 5, 11,
## 16 (no son text/text2) y como faltantes las bandas de text2 en la 6 (se dibujan como color y leyenda). df barajado →
## mismos textos; scatter sin columna n → sin puntos huecos.
