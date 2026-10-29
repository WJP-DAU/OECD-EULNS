## 09_wjpr.R — figuras con WJPr (≥ 1.2.0) + lo que aún le falta ---------------------------------------------------
## Origen: nuevo (2026-10-23, pedido del usuario: usar WJPr en el mayor número de figuras y complementarlo con código
## hasta el diseño publicado); desde el 27-oct (pedido del usuario: dibujar con WJPr 1.2.0, que cubre lo que antes
## suplían los complementos) cada wjpr_<tipo>(df, spec, ...) es la función de WJPr 1.2.0 del tipo con sus argumentos
## (layout = wjp_layout(…), value_*, label_*, header_*, ptheme = WJP_theme(…) + theme(…), …), como los fid_<tipo> de
## dev/report-fidelity*.R del paquete, pero con el df del contrato de entrada (§4.4: order, kind, grouping, label,
## value, value2, text, text2 + extras; value crudo, text impreso) y las medidas del diseño de STD/ST_*/LABEL_COL
## (R/04_svg.R) y de spec (w_in, h_in, base_pt). Devuelve un ggplot con attr(p, "wjpr") = list(base, complementos,
## compara, drawn, rotulos, marcas) (wjpr_meta). La exporta y la mide tools/wjpr_gallery.R; es el motor de main.R
## (metodo wjpr: plot = wjpr_<tipo>(df, spec)). Verificación del dibujo (R/07_verify.R): wjpr_similarity() (parecido
## global y local con la publicada) y, en los adaptadores que la exponen, lo dibujado por fila (attr(p, "wjpr")$drawn,
## wjpr_drawn(): WJPR_DRAWN_COLS + las opcionales WJPR_DRAWN_OPT, leídas del dibujo con wjpr_slots(), wjpr_slot_of(),
## wjpr_pos_slots() y wjpr_drawn_texts(), que lee los textos de WJPr::wjp_drawn()).
## Lo que 1.2.0 aún no cubre lleva "# complemento: <qué le falta a WJPr>" y su propuesta de argumento: es la fuente
## del documento de brechas para el autor del paquete.
## Solo funciones: nada se ejecuta al cargar; WJPr solo se pide al llamar (wjpr_require: ≥ 1.2.0).

WJPR_FAMILY <- "inter"   # familia registrada por setup_fonts() (Inter Tight); la de las figuras publicadas
WJPR_MIN_VERSION <- "1.2.0"   # la que necesitan los adaptadores (deps.csv fija la build exacta: 1.2.0@1664a23)


## Utilidades comunes ----------------------------------------------------------------------------------------------

# Interruptor único de los complementos: options(wjpr.complementos = FALSE) dibuja cada figura con WJPr sola (la línea
# de base que mide la galería con --sin-complementos: lo que el paquete logra sin ayuda)
wjpr_complementos <- function() isTRUE(getOption("wjpr.complementos", TRUE))

# corregido (M1, 1.2.0): exige WJPr ≥ 1.2.0 con un error legible (antes aceptaba cualquier build y los adaptadores
# fallaban después con argumentos desconocidos o «Problem while computing aesthetics»); la familia de las figuras
# va siempre en options(wjpr.family = ), que 1.2.0 aplica a todas sus capas y temas (la rama de 1.1.0 sin
# wjp_font_family, con «Lato Full» fija y el complemento wjpr_font, ya no existe)
wjpr_require <- function() {
  if (!requireNamespace("WJPr", quietly = TRUE)) {
    stop("WJPr no está instalado: es el motor de dibujo (metodo wjpr). Instale la build de deps.csv ",
         "(remotes::install_github(\"worldjusticeproject-org/WJPr@1664a23\")) o defina SRC_REPORTS_LIB con una librería ",
         "que la tenga", call. = FALSE)
  }
  v <- utils::packageVersion("WJPr")
  if (v < package_version(WJPR_MIN_VERSION)) {
    stop("WJPr ", wjpr_build(), " es anterior a la ", WJPR_MIN_VERSION, " que necesitan los adaptadores de R/09_wjpr.R ",
         "(layout = wjp_layout(), wjp_drawn(), wjp_funnel(), …). Instale la build de deps.csv ",
         "(remotes::install_github(\"worldjusticeproject-org/WJPr@1664a23\")) o defina SRC_REPORTS_LIB con una librería ",
         "que la tenga (SRC_REPORTS_LIB=", Sys.getenv("SRC_REPORTS_LIB"), ")", call. = FALSE)
  }
  options(wjpr.family = WJPR_FAMILY)
  invisible(TRUE)
}

# Capacidades de la build cargada: con WJPr ≥ 1.2.0 no hay ramas por build; las usan las guardas de los adaptadores
# (una función o un argumento de 1.2.0 que falta → error legible). corregido (integración, 2026-10-28): se quitan
# wjpr_native_font() y wjpr_font(), que solo servían para 1.1.0 («Lato Full» fija en capas y temas)
wjpr_has_fun <- function(fun) requireNamespace("WJPr", quietly = TRUE) && exists(fun, envir = asNamespace("WJPr"), inherits = FALSE)
wjpr_has_arg <- function(fun, arg) wjpr_has_fun(fun) && arg %in% names(formals(get(fun, envir = asNamespace("WJPr"))))
# Versión y commit de la build cargada ("1.2.0@1664a23"; "local" si se instaló desde un clon): dos builds con el mismo
# número de versión pueden tener código distinto (pasó con 1.1.0: la del 13-jul frente a main)
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
# nombre impreso, el valor y el orden con la referencia. Lo exponen (revisión 4) wjpr_grouped_bars (wjpr_gb_drawn;
# desde M1 en el modo fila de 1.2.0: y continua, la fila de cada línea la dan los datos de las capas de WJPr),
# wjpr_paired_bars, wjpr_severity_bars, wjpr_dumbbell, wjpr_matrix, wjpr_scatter, wjpr_network y wjpr_sankey, y desde
# la migración a 1.2.0 (2026-10-27/28) también wjpr_hbars, wjpr_columns, wjpr_funnel, wjpr_cards y wjpr_upset: los
# 13 adaptadores, las 42 figuras. Un adaptador nuevo lo añade con wjpr_meta(…, drawn = ), y
# wjpr_slots()/wjpr_slot_of() (o wjpr_pos_slots()/wjpr_pos_slot_of() con un eje mapeado; o, con un panel de y continua,
# WJPr::wjp_drawn(p, "marks") y los datos de cada capa, como wjpr_gb_lines) le dan la fila de cada barra o texto y
# wjpr_drawn_texts() lo impreso en cada posición. NULL: el dibujo solo lo miden el parecido y el parecido local.
# rotulos = rótulos fijos
# del diseño que el adaptador dibuja y df no trae (nombres de nodo, categorías destacadas): entran con los de compara
# en la comparación de textos (drawn_text_diff_wjpr, 06_export.R); los declaran wjpr_scatter (categorías rotuladas,
# 3) y wjpr_network (el texto de cada nodo como lo imprime wjp_network: nombre en sus líneas + "\n" + df$text, con
# compara vacío, 5).
# corregido (revisión 4): marcas = function(b) con las marcas fijas del diseño que no tienen fila en df y faltan en el
# dibujo (character(), vacío si están todas; p. ej. la línea de corte de la 7, wjpr_sev_cut_missing): figures da
# estado "dibujo" si devuelve alguna. NULL: el adaptador no tiene marcas fijas que comprobar.
# corregido (cierre, revisión de usabilidad): alto = el h_in (in) que pide el diseño (hoy las agrupadas,
# wjpr_gb_alto): figures da "tamaño" si el manifiesto da menos (el dibujo se recorta abajo) y un aviso si da más
# (sobra espacio abajo), con el h_in a poner. Sin figura publicada, parecido no ve un alto equivocado. NULL: no lo
# declara (las demás figuras llenan el lienzo del manifiesto).
wjpr_meta <- function(p, base, complementos = character(), compara = c("text", "text2"), drawn = NULL,
                      rotulos = character(), marcas = NULL, alto = NULL) {
  if (!is.null(drawn) && !is.function(drawn)) stop("wjpr_meta: drawn debe ser una función de ggplot_build()", call. = FALSE)
  if (!is.null(marcas) && !is.function(marcas)) stop("wjpr_meta: marcas debe ser una función de ggplot_build()", call. = FALSE)
  attr(p, "wjpr") <- list(base = base, complementos = unique(complementos), compara = compara, drawn = drawn,
                          rotulos = as.character(rotulos), marcas = marcas, alto = alto)
  p
}

# Tema de partida: WJP_theme() con Inter Tight (por ptheme, el argumento documentado de las funciones de WJPr).
# transitorio (M1): con 1.2.0 equivale a WJPr::WJP_theme() (la familia sale de options(wjpr.family) también en el
# elemento raíz text); los bloques reescritos pasan ptheme = WJPr::WJP_theme(…) + ggplot2::theme(…) directamente
wjpr_theme <- function(family = WJPR_FAMILY) {
  wjpr_require()
  e <- ggplot2::element_text(family = family)
  WJPr::WJP_theme() + ggplot2::theme(text = e, axis.text = e, axis.text.x = e, axis.text.y = e, axis.title = e,
                                     axis.title.x = e, axis.title.y = e, strip.text = e, legend.text = e,
                                     legend.title = e, plot.title = e)
}

## Lo impreso en el dibujo (wjpr_drawn_texts) -------------------------------------------------------------------
## corregido (M1, 1.2.0): los textos de las capas salen de WJPr::wjp_drawn(p, "text") (etiqueta sin markup, x, y en
## coordenadas de datos, PANEL y color de cada texto de geom_text, geom_label, ggtext y ggrepel); antes se leían de
## ggplot_build() con código propio. wjpr_drawn_texts(b) conserva su salida (source "layer" | "axis-x" | "axis-y" |
## "strip", layer, PANEL, label, x, y, colour) y le añade lo que wjp_drawn no da:
## complemento: wjp_drawn lista también lo que no se ve (propuesta: wjp_drawn(visible = TRUE)): aquí se descarta un
## texto con alfa 0, del color de lo que tiene debajo (el fondo del panel o del gráfico, blanco por defecto, o el
## relleno de una barra, celda o polígono de una capa anterior: un texto blanco sobre una celda oscura sí se ve),
## tapado por una marca opaca de una capa posterior o de tamaño 0.
## complemento: wjp_drawn quita las etiquetas HTML pero no lee el Markdown de ggtext (escapes "\*", "1\.", entidades
## &amp;) ni el color de su <span style='color:…'> (propuesta: label = el texto leído y colour = el del span).
## complemento: en los ejes wjp_drawn da la posición de cada etiqueta en 0-1 del panel (break_positions), por eje de
## pantalla (con coord_flip "axis-x" es el horizontal), incluidas las de un eje con element_blank() o guide "none", y
## los strips de todos los paneles (propuesta: wjp_drawn(coords = "data") con solo los ejes y strips dibujados): las
## etiquetas de eje (en la posición de su break en datos, por eje de datos) y de strip (en el panel del borde donde se
## dibuja) se siguen leyendo de ggplot_build() como antes.
## Es lo que leen los adaptadores para saber qué etiqueta está impresa en cada posición (label_drawn, grouping_drawn)
## y lo que cuenta wjpr_texts().

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
  # Capas de texto: lo que imprime cada una según WJPr::wjp_drawn (que construye el gráfico otra vez: b$plot), fila a
  # fila con su capa construida (columnas por nombre exacto: d$x de un data.frame casaría con xmin), de donde salen el
  # Markdown sin leer, el tamaño, el alfa, el ajuste y el ángulo que necesita el filtro de visibilidad
  wd <- WJPr::wjp_drawn(b$plot, "text")
  wd <- wd[wd$source == "layer", , drop = FALSE]
  for (i in seq_along(b$data)) {
    l <- b$plot$layers[[i]]
    if (!inherits(l$geom, WJPR_TEXT_GEOMS)) next
    d <- as.data.frame(b$data[[i]])
    if (!nrow(d) || !"label" %in% names(d)) next
    cl <- function(nm, default) if (nm %in% names(d)) d[[nm]] else rep(default, nrow(d))
    md <- inherits(l$geom, c("GeomRichText", "GeomTextBox"))
    raw <- as.character(d[["label"]])
    x <- suppressWarnings(as.numeric(cl("x", NA))); y <- suppressWarnings(as.numeric(cl("y", NA)))
    # filas que lista wjp_drawn (etiqueta no vacía y posición finita), en el orden de la capa
    k0 <- which(!is.na(raw) & nzchar(trimws(raw)) & is.finite(x) & is.finite(y))
    w <- wd[wd$layer == i, , drop = FALSE]
    if (nrow(w) != length(k0)) {
      stop("wjpr_drawn_texts: WJPr::wjp_drawn da ", nrow(w), " textos en la capa ", i, " y ggplot_build() ", length(k0),
           call. = FALSE)
    }
    if (!length(k0)) next
    lab <- wjpr_plain_text(w$label, markdown = md)
    size <- suppressWarnings(as.numeric(cl("size", 1)))[k0]
    ok <- !is.na(lab) & nzchar(lab) & coalesce(size, 1) > 0
    if (!any(ok)) next
    col <- coalesce(as.character(w$colour), as.character(cl("colour", l$aes_params$colour %||% "black"))[k0])
    if (md) col <- coalesce(wjpr_markup_colour(raw[k0]), col)
    pan <- coalesce(as.character(w$PANEL), "1")
    j <- k0[ok]
    box <- if (inherits(l$geom, c("GeomLabel", "GeomLabelRepel"))) as.character(cl("fill", "white"))[j] else NULL
    pr <- wjpr_text_probe(w$x[ok], w$y[ok], cl("hjust", 0.5)[j], cl("vjust", 0.5)[j], cl("angle", 0)[j],
                          inherits(b$layout$coord, "CoordFlip"))
    vis <- wjpr_text_visible(b, i, pan[ok], pr$x, pr$y, col[ok], cl("alpha", NA)[j], bg_panel, box)
    k <- which(ok)[vis]
    if (length(k)) rows[[length(rows) + 1]] <- tibble(source = "layer", layer = i, PANEL = pan[k], label = lab[k],
                                                       x = w$x[k], y = w$y[k], colour = col[k])
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
# en columns frac = value/vmax y value = frac · (vmax implícito de la referencia): mismas alturas relativas y value en
# %; en el resto, value/value2 tal cual.
# Extras (x, y, n, lo, hi) se conservan.
wjpr_ref_df <- function(spec, cfg) {
  f <- file.path(cfg$paths$reference, paste0(figure_stem(spec), ".csv"))
  if (!file.exists(f)) stop("sin referencia: ", basename(f), call. = FALSE)
  d <- read_csv(f, col_types = cols(.default = col_character()), na = character())
  num <- intersect(c("order", "value", "value2", "frac", "frac2", "x", "y", "n", "lo", "hi"), names(d))
  d <- d %>% mutate(across(all_of(num), ~ suppressWarnings(as.numeric(.x))))
  if (spec$tipo %in% c("grouped_bars", "dumbbell", "hbars", "paired_bars", "columns")) {
    # corregido (revisión 5, M2): en columns frac = value / vmax (la más alta = 1) y la escala es el vmax que implica la
    # referencia (mediana de value / frac): las mismas alturas relativas que frac·100 y value en %, las unidades de la
    # referencia con que verify y la galería comparan lo dibujado por fila (val); con frac·100, val 100 frente a 36
    vm <- function() { r <- c(d$value / d$frac, d$value2 / d$frac2); stats::median(r[is.finite(r) & r > 0]) }
    scale <- if (spec$tipo == "columns") vm() else if (spec$tipo == "grouped_bars" && !any(grepl("%", d$text))) 10 else 100
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
# texto sin markup). complemento: WJPr::wjp_drawn (1.2.0) no lee la leyenda (propuesta: wjp_drawn(what = "legend"),
# una fila por clave con su texto y su color): se lee de las guías que ggplot_build() ya entrenó (b$plot$guides: key
# con .label y el color de cada clave). Vacía si no
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


## Prueba de la infraestructura con 1.2.0 (M1, 2026-10-27/28; SCRATCH/fix5/M1/pruebas): p03_textos_iguales.R →
## wjpr_drawn_texts() con WJPr::wjp_drawn da la misma salida (identical) que la anterior (la de ROOT) en las 40 figuras
## que se dibujan (df de data() en el lote 1, referencia en el resto; las 10 y 15 fallan en su adaptador, «Problem while
## computing aesthetics», antes de llegar aquí), también con las agrupadas en modo fila; 0.04-0.7 s por figura (antes
## 0.01-0.13: wjp_drawn construye el gráfico otra vez). p02_ejes.R: wjp_drawn da las etiquetas de eje en 0-1 del panel y
## por eje de pantalla (con coord_flip "axis-x" es el horizontal) y los strips de todos los paneles: por eso ejes y
## strips se siguen leyendo de ggplot_build(). wjpr_require() con SRC_REPORTS_LIB de 1.1.0@382aee9 → la etapa figures
## se detiene con «WJPr 1.1.0@382aee9 es anterior a la 1.2.0 …» (exit 1); con 1.2.0@1664a23 y el deps.csv nuevo, sin
## «aviso deps.csv». Galería de las 42 con 1.2.0: 40 ok y las 10 y 15 en error (su bloque), igual que antes de M1.

## Bloques por tipo ---------------------------------------------------------------------------------------------------
## (cada bloque: encabezado "## Bloque wjpr <tipo> — figuras …", la función wjpr_<tipo>(df, spec, ...) y sus
## auxiliares; se añaden debajo)

## Bloque wjpr grouped_bars — figuras 1, 4, 9, 12, 17, 20-28, 31, 33, 35, 38-40 --------------------------------------
## Base (desde el 27-oct, WJPr 1.2.0): WJPr::wjp_groupbars() en modo fila (group_header = "row" y layout =
## wjp_layout(…)), solo con sus argumentos: una fila por categoría del df (grouping, label, value crudo, labels =
## df$text impreso tal cual), group_order y level_order en el orden del df (de arriba abajo), national_value con
## nombres (National Average y, si la hay, EU Average) y national_value_label = su df$text, na_label = "N/A",
## scale_max (100, o 10 en la media 0-10 de la 4), show_complement = FALSE en la 4 (sin pista), label_position
## "column" (la 4: "end", el valor tras la barra), value_*, label_*, highlight_* y header_* (tamaños, caras 600 cursiva
## y colores de STD) y ptheme = WJPr::WJP_theme(). La geometría va en wjp_layout() desde STD (la 27: ST_A01, con la
## pista que llena los 6.5 in como chart_grouped(fill_w = spec$w_in * 72), y sin filas de promedio) y
## LABEL_COL[["std"]]: columna de etiquetas, pista, columna de valores, alto de fila y de barra, bloque de encabezado
## y su línea base, márgenes. Con layout el gráfico tiene un tamaño definido: el ancho de la columna de valores y el
## margen inferior salen de spec (w_in, h_in) para que mida exactamente lo que el manifiesto (wjpr_gb_layout): el SVG
## se exporta a ese tamaño y el dibujo llena el lienzo como la publicada (barra en x0 = L + gap_label, primera fila a
## 2 pt del borde superior).
## Capas de 1.2.0 en modo fila (un solo panel; y continua, en pt desde abajo): GeomRect pista 0-100 (si
## show_complement), GeomRect barra, GeomText valores (x ≥ 0) y GeomText etiquetas, promedios y encabezados (x < 0,
## alineados a la derecha al final de la columna de etiquetas). Los datos de cada capa traen la línea de WJPr que
## dibujan (kind avg | header | row, group, label, y): con ellos lee lo dibujado por fila wjpr_gb_drawn().
## Antes (1.1.0@382aee9 y main a46c831): 9-11 complementos (fila EU como dato, encabezados como facetas de una fila,
## columna de valores, N/A, texto del valor nacional, valores 9 pt, pista y panel del ancho publicado, etiquetas 10 pt
## y promedios/encabezados en Markdown, su línea base, alto de fila y de encabezado, márgenes y columna de etiquetas,
## fuente en 1.1.0); con 1.2.0 ninguno en las 20 de Hungría y uno en la media 0-10 de la 4 cuando trae celdas N/A
## (Polonia, Malta: el N/A en la columna de valores). complementos = FALSE devuelve la llamada a WJPr sola (igual a la
## completa salvo ese N/A).
## Unidades: puntos de 1/72 in (los de svglite, de 04_svg.R y de wjp_layout).

# Medidas del diseño publicado (pt): st, ancho W y alto H de la figura (spec), columna de etiquetas L, escala (100; 10
# en la media 0-10: la 4, o textos sin "%"), pista (no en la media) y valor tras la barra (en la media)
# origen: el wjpr_gb_design anterior (09_wjpr.R del 23-oct) + H de spec
wjpr_gb_design <- function(df, spec) {
  fig <- as.integer(spec$fig)
  st <- if (identical(fig, 27L)) ST_A01 else STD
  W <- spec$w_in * 72; H <- spec$h_in * 72; L <- LABEL_COL[["std"]]
  if (identical(fig, 27L)) st$track <- W - L - st$gap_label - st$gap_value - st$value_w   # la pista llena la figura
  media <- identical(fig, 4L) || !any(grepl("%", df$text, fixed = TRUE))
  list(st = st, W = W, H = H, L = L, scale = if (media) 10 else 100, track = !media, value_at_end = media)
}

# wjp_layout() del diseño. Alto: 2 pt + una fila (st$pitch) por promedio y categoría + un bloque (st$header) por
# encabezado + margen inferior; el margen inferior (4 pt en el diseño, chart_grouped: rows$y + 4) es lo que falta
# hasta spec$h_in (redondeado a 3 decimales: ± 0.04 pt) y el ancho de la columna de valores (27 pt) lo que falta hasta
# spec$w_in (std: 26.97 pt), de modo que el tamaño definido del gráfico es el del manifiesto. Si el diseño no cabe
# (h_in o w_in menores que lo que mide), aviso y el margen o la columna quedan en 0 (el dibujo se saldría del lienzo
# y lo vería parecido; el alto correcto es el del aviso).
# Alto (in) que pide el diseño: el de wjpr_gb_layout con el margen inferior del diseño (4 pt), la regla de CLAUDE.md
# (6 + 15.5 · filas avg y row + 22 · encabezados) / 72 con las medidas de STD
wjpr_gb_alto <- function(df, g, top = 2) {
  (top + sum(df$kind == "header") * g$st$header + sum(df$kind %in% c("avg", "row")) * g$st$pitch + 4) / 72
}
wjpr_gb_layout <- function(df, g, top = 2) {
  st <- g$st
  body <- sum(df$kind == "header") * st$header + sum(df$kind %in% c("avg", "row")) * st$pitch
  value_w <- g$W - g$L - st$gap_label - st$track - st$gap_value
  bottom <- g$H - top - body
  if (bottom < 0) {
    warning(sprintf("wjpr_grouped_bars: el diseño mide %.2f pt de alto y la figura %.2f (h_in %s); h_in = %.3f in",
                    top + body + 4, g$H, format(g$H / 72), (top + body + 4) / 72), call. = FALSE)
  }
  if (value_w < 0) warning(sprintf("wjpr_grouped_bars: el diseño mide %.2f pt de ancho y la figura %.2f", g$W - value_w, g$W), call. = FALSE)
  WJPr::wjp_layout(label_width = g$L, label_gap = st$gap_label, track_width = st$track, value_gap = st$gap_value,
                   value_width = max(value_w, 0), row_height = st$pitch, bar_height = st$bar,
                   header_height = st$header, header_baseline = st$header_base, margins = c(top, 0, max(bottom, 0), 0))
}

# Entrada de wjp_groupbars: una fila por categoría (grp, lvl, v = value crudo, lab = df$text), los grupos y sus
# categorías en el orden del df (group_order, level_order: de arriba abajo en modo fila), los promedios (avg: National
# y EU Average, en ese orden) y la clave de cada fila del df para lo dibujado por fila (key = kind | grouping | label;
# en los promedios el grouping es "", el national_group_label de WJPr no cuenta)
wjpr_gb_input <- function(df) {
  avg <- df[df$kind == "avg", ]; rows <- df[df$kind == "row", ]
  groups <- unique(as.character(df$grouping[df$kind %in% c("header", "row")]))
  d <- tibble(grp = as.character(rows$grouping), lvl = as.character(rows$label), v = as.numeric(rows$value),
              lab = coalesce(as.character(rows$text), ""))
  keys <- tibble(key = wjpr_gb_key(df$kind, df$grouping, df$label), order = as.integer(df$order), kind = df$kind,
                 grouping = df$grouping, label = df$label)
  list(d = d, groups = groups, lev = setNames(lapply(groups, function(gr) d$lvl[d$grp == gr]), groups), avg = avg,
       keys = keys)
}
wjpr_gb_key <- function(kind, grouping, label) {
  kind <- as.character(kind)
  paste(kind, ifelse(kind == "avg", "", coalesce(as.character(grouping), "")), as.character(label), sep = "\r")
}

# Líneas del dibujo (b = ggplot_build del gráfico final), de arriba abajo: una por posición y de las etiquetas de la
# columna de etiquetas (las filas de texto con x < 0 de todas las capas de texto, se vean o no), con la línea de WJPr
# que la dibuja (kind, group, label: los datos de esa capa, fila a fila). Se asignan a la línea más cercana (a menos
# de media separación entre líneas) las barras del color principal (len = xmax − xmin, en % de la pista), las pistas
# (si se da su color) y los textos visibles (wjpr_drawn_texts) de la columna de etiquetas (x < 0: label_drawn) y de
# valores (x ≥ 0). Una barra o un valor lejos de toda etiqueta abre una línea propia sin etiqueta: no se pierde.
wjpr_gb_lines <- function(b, bar_fill, track_fill = NULL) {
  layers_of <- function(cls) which(vapply(b$plot$layers, function(l) inherits(l$geom, cls), logical(1)))
  ident <- function(i, n) {                             # la línea de WJPr de cada fila construida de la capa i
    s <- b$plot$layers[[i]]$data
    ok <- is.data.frame(s) && nrow(s) == n && all(c("kind", "group", "label") %in% names(s))
    na <- rep(NA_character_, n)
    tibble(w_kind = if (ok) as.character(s$kind) else na, w_group = if (ok) as.character(s$group) else na,
           w_label = if (ok) as.character(s$label) else na)
  }
  txt_rows <- bind_rows(lapply(layers_of(WJPR_TEXT_GEOMS), function(i) {
    d <- as.data.frame(b$data[[i]])
    if (!nrow(d) || !all(c("x", "y") %in% names(d))) return(NULL)
    bind_cols(tibble(layer = i, x = as.numeric(d$x), y = as.numeric(d$y)), ident(i, nrow(d)))
  }))
  rects <- bind_rows(lapply(layers_of(c("GeomRect", "GeomCol", "GeomBar", "GeomTile")), function(i) {
    d <- as.data.frame(b$data[[i]])
    if (!nrow(d) || !all(c("xmin", "xmax", "ymin", "ymax", "fill") %in% names(d))) return(NULL)
    bind_cols(tibble(layer = i, y = (as.numeric(d$ymin) + as.numeric(d$ymax)) / 2, xmin = as.numeric(d$xmin),
                     xmax = as.numeric(d$xmax), fill = as.character(d$fill)), ident(i, nrow(d)))
  }))
  if (!nrow(rects)) rects <- tibble(layer = integer(), y = numeric(), xmin = numeric(), xmax = numeric(), fill = character(),
                                    w_kind = character(), w_group = character(), w_label = character())
  labs <- txt_rows[is.finite(txt_rows$x) & txt_rows$x < 0 & is.finite(txt_rows$y), , drop = FALSE]
  labs <- labs[!duplicated(round(labs$y, 6)), , drop = FALSE]
  lines <- labs %>% transmute(y, w_kind, w_group, w_label, has_label = TRUE)
  bars <- rects[wjpr_same_color(rects$fill, bar_fill) & is.finite(rects$y), , drop = FALSE]
  tracks <- if (is.null(track_fill)) rects[0, ] else rects[wjpr_same_color(rects$fill, track_fill) & is.finite(rects$y), , drop = FALSE]
  tx <- wjpr_drawn_texts(b); tx <- tx[tx$source == "layer" & is.finite(tx$y), , drop = FALSE]
  vals <- tx[is.finite(tx$x) & tx$x >= 0, , drop = FALSE]
  tol <- if (nrow(lines) > 1) min(diff(sort(lines$y))) / 2 else 1
  near <- function(y) vapply(y, function(v) {
    if (!nrow(lines)) return(NA_integer_)
    d <- abs(lines$y - v); j <- which.min(d); if (d[j] < tol) j else NA_integer_
  }, NA_integer_)
  # barras y valores sin línea: una línea propia cada una (la identidad, la de su capa)
  for (src in list(bars, vals)) {
    lost <- src[is.na(near(src$y)), , drop = FALSE]
    if (nrow(lost)) {
      id <- if ("w_kind" %in% names(lost)) lost[, c("w_kind", "w_group", "w_label")] else
        tibble(w_kind = rep(NA_character_, nrow(lost)), w_group = NA_character_, w_label = NA_character_)
      lines <- bind_rows(lines, bind_cols(tibble(y = lost$y), id, tibble(has_label = FALSE)))
      lines <- lines[!duplicated(round(lines$y, 6)), , drop = FALSE]
    }
  }
  lines <- lines[order(-lines$y), , drop = FALSE]
  bars$line <- near(bars$y); tracks$line <- near(tracks$y); vals$line <- near(vals$y)
  lab_tx <- tx[is.finite(tx$x) & tx$x < 0, , drop = FALSE]; lab_tx$line <- near(lab_tx$y)
  list(lines = lines, bars = bars, tracks = tracks, vals = vals, labels = lab_tx)
}

# Lo dibujado por fila (contrato drawn de wjpr_meta; V3-01, V3-02 y contrato extendido de la revisión 4), leído de b:
# en cada línea del dibujo (wjpr_gb_lines, de arriba abajo = order) la fila del df de la línea de WJPr que la dibuja
# (sin equivalente: kind "" y la etiqueta de WJPr como label, una fila sobrante), frac = largo de las barras del
# color principal de esa línea / 100 (la pista es 0-100 en la escala de WJPr; con scale_max = 10 la 4 dibuja value /
# 10 · 100, así que frac = value / scale, el de la referencia), val = frac · scale (en las unidades de df$value: % o, en
# la 4, media 0-10), text = los valores que se ven en esa línea (un valor con alfa 0 o blanco sobre blanco no cuenta),
# label_drawn = la etiqueta impresa y visible en la columna de etiquetas ("" si ninguna) y grouping_drawn = la del
# último encabezado impreso por encima (la propia en un encabezado; "" en los promedios, que van antes del primero).
# Una barra o un valor en la línea equivocada, una barra más larga, un orden distinto, dos etiquetas intercambiadas,
# un encabezado cambiado o un valor oculto se ven aquí aunque el multiconjunto de textos y el parecido sean iguales.
# corregido (M1, 1.2.0): reescrito para el modo fila de 1.2.0 (un panel con y continua y las etiquetas como capa de
# texto); antes leía las posiciones del eje y discreto por faceta (wjpr_slots) y las etiquetas del eje.
wjpr_gb_drawn <- function(b, keys, bar_fill, scale = 100) {
  ln <- wjpr_gb_lines(b, bar_fill)
  L <- ln$lines; n <- nrow(L)
  k <- match(wjpr_gb_key(L$w_kind, L$w_group, L$w_label), keys$key)
  kind <- coalesce(keys$kind[k], "")
  pick <- function(t, s) paste(t$label[!is.na(t$line) & t$line == s], collapse = " ")
  lab_drawn <- vapply(seq_len(n), function(s) pick(ln$labels, s), "")
  hdr <- ""; grp_drawn <- character(n)
  for (i in seq_len(n)) {                               # las líneas van de arriba abajo
    if (kind[i] == "header") hdr <- lab_drawn[i]
    grp_drawn[i] <- hdr
  }
  frac <- vapply(seq_len(n), function(s) {
    l <- ln$bars$xmax[!is.na(ln$bars$line) & ln$bars$line == s] - ln$bars$xmin[!is.na(ln$bars$line) & ln$bars$line == s]
    if (length(l)) sum(l) / 100 else NA_real_
  }, 0)
  tibble(order = seq_len(n), kind = kind, grouping = coalesce(keys$grouping[k], L$w_group, ""),
         label = coalesce(keys$label[k], L$w_label, ""), frac = frac, frac2 = NA_real_,
         text = vapply(seq_len(n), function(s) pick(ln$vals, s), ""), text2 = "",
         label_drawn = lab_drawn, grouping_drawn = grp_drawn, val = frac * scale, val2 = NA_real_)
}
wjpr_gb_drawn_fun <- function(keys, bar_fill, scale = 100) {
  force(keys); force(bar_fill); force(scale)
  function(b) wjpr_gb_drawn(b, keys, bar_fill, scale)
}

# Marcas fijas del diseño (contrato marcas de wjpr_meta): con pista, cada barra lleva su pista 0-100 % en su línea
# (una que falte → estado "dibujo"); devuelve las que faltan ("pista 0-100 % de <label>")
# nuevo (M1): la pista no tiene fila en df y antes solo la medía el parecido local
wjpr_gb_marcas_fun <- function(bar_fill, track_fill) {
  force(bar_fill); force(track_fill)
  function(b) {
    ln <- wjpr_gb_lines(b, bar_fill, track_fill)
    full <- !is.na(ln$tracks$line) & ln$tracks$xmin <= 1e-6 & ln$tracks$xmax >= 100 - 1e-6
    miss <- setdiff(unique(ln$bars$line[!is.na(ln$bars$line)]), ln$tracks$line[full])
    if (!length(miss)) return(character())
    paste0("pista 0-100 % de ", coalesce(ln$lines$w_label[miss], "?"))
  }
}

wjpr_grouped_bars <- function(df, spec, complementos = wjpr_complementos(), ...) {
  wjpr_require()
  df <- df[order(df$order), ]
  g <- wjpr_gb_design(df, spec); st <- g$st
  inp <- wjpr_gb_input(df); avg <- inp$avg
  has_nat <- nrow(avg) > 0
  # corregido (revisión 4): con un promedio suprimido (NA) WJPr se detiene con «national_value must contain finite
  # numbers», sin decir qué figura ni por qué; el error legible sale antes de llamar a WJPr
  bad <- has_nat & !is.finite(suppressWarnings(as.numeric(avg$value)))
  if (any(bad)) {
    stop(sprintf("fig %02d: sin promedio (%s con value NA: suprimido por n < 30 o sin datos; wjp_groupbars necesita national_value)",
                 as.integer(spec$fig), paste(avg$label[bad], collapse = ", ")), call. = FALSE)
  }
  # nuevo (M1): con layout la columna de etiquetas es fija (L pt): una etiqueta más ancha se saldría del lienzo
  w_lab <- max(0, vapply(seq_len(nrow(df)), function(i) {
    if (df$kind[i] == "row") text_width(df$label[i], st$size_label) else text_width(df$label[i], st$size_label, 600, TRUE)
  }, 0))
  if (w_lab > g$L) warning(sprintf("wjpr_grouped_bars: la etiqueta más ancha mide %.1f pt y la columna publicada %s", w_lab, g$L), call. = FALSE)
  p <- WJPr::wjp_groupbars(
    data = inp$d, target = "v", grouping = "grp", levels = "lvl", labels = "lab",
    colors = c(st$col$main, st$col$track), show_complement = g$track, scale_max = g$scale,
    group_order = inp$groups, level_order = inp$lev,
    show_national = has_nat, national_style = "bar", national_position = "top",
    national_value = if (has_nat) setNames(as.numeric(avg$value), avg$label) else NULL,
    national_value_label = if (has_nat) as.character(avg$text) else NULL, na_label = "N/A",
    label_position = if (g$value_at_end) "end" else "column", value_gap = st$gap_value,
    value_size = st$size_value, value_fontface = "semibold.italic", value_colour = st$col$main,
    label_size = st$size_label, label_colour = st$col$text,
    highlight_fontface = "semibold.italic", highlight_colour = st$col$main,
    group_header = "row", header_fontface = "semibold.italic", header_colour = st$col$main, header_size = st$size_label,
    layout = wjpr_gb_layout(df, g), ptheme = WJPr::WJP_theme())
  # lo dibujado por fila (V3-02; revisión 4: label_drawn, grouping_drawn y val en unidades de df$value) y la pista de
  # cada barra como marca fija, también sin complementos
  drawn <- wjpr_gb_drawn_fun(inp$keys, st$col$main, g$scale)
  marcas <- if (g$track) wjpr_gb_marcas_fun(st$col$main, st$col$track) else NULL
  comp <- character()
  # complemento: con label_position = "end" (la media 0-10 de la 4) WJPr pone el texto de una celda sin valor (N/A) al
  # inicio de la pista (0 + value_gap); el diseño (chart_grouped con value_at_end) lo pone en la columna de valores,
  # value_gap tras la pista. Pasa en Polonia y Malta (la 4 de Hungría no tiene N/A). Se mueve solo esa x de la capa de
  # valores (propuesta: na_position = "column" en wjp_groupbars)
  if (complementos && g$value_at_end && any(is.na(inp$d$v) & nzchar(inp$d$lab))) {
    x_col <- 100 + st$gap_value / st$track * 100
    p$layers <- lapply(p$layers, function(l) {          # l no se reasigna: es el padre de la copia (ggproto)
      if (!inherits(l$geom, "GeomText") || !all(c("x", "value", "text") %in% names(l$data))) return(l)
      l2 <- ggplot2::ggproto(NULL, l)
      l2$data <- transform(l$data, x = ifelse(is.na(value), x_col, x))
      l2
    })
    comp <- c(comp, "N/A de la media (label_position end) en la columna de valores")
  }
  wjpr_meta(p, base = "WJPr::wjp_groupbars", complementos = comp, drawn = drawn, marcas = marcas, alto = wjpr_gb_alto(df, g))
}
## Prueba del bloque (M1, 2026-10-27/28, WJPr 1.2.0@1664a23; SCRATCH/fix5/M1/pruebas): galería --figs 1,4,9,12,17,
## 20-28,31,33,35,38-40 → 20/20 ok, textos iguales, f1 1.000 (p04_medidas.R: 1.0000 con cuatro decimales en las 20),
## local 0.000, por fila «n filas ok» (18, 17, 36, 36, 23, 23, 22, 23, 23, 22, 23, 23, 9, 18, 23, 18, 18, 24, 24, 24),
## 0 complementos (antes, 1.1.0@382aee9: 9-11 y los mismos f1 1.000 y local 0.000, pruebas/gal110); --sin-complementos:
## los 20 SVG idénticos (md5) a los completos. Tamaño definido del ggplot (ggplotGrob) = w_in × h_in del manifiesto en
## las 20 (6.0690 × 4.3190, …, 6.5000 × 3.0000). Frente a 1.1.0: barra en x0 = 155, valores a 411.98, etiquetas a la
## misma x y a ≤ 0.1 pt en y; la pista se dibuja entera bajo la barra (antes, el tramo que quedaba): 11-42 mil píxeles
## de 300 dpi distintos en el antialias (máx. 0.25-0.48), nada a 96 dpi. Inyecciones (pruebas/iny_gb.R, portadas a las
## capas de 1.2.0; iny_run.sh: --stage all --figs 01 --strict, una a la vez) → exit 1 en las 9: Male/Female, 65-100/
## 18-24 y Urban/Rural impresas intercambiadas (texto · label_drawn), encabezado Gender → Sex (texto · label_drawn +
## grouping_drawn de 4 filas), valor de Male con alfa 0 (texto · text Male: 65%→), Helvetica en los valores (texto · 14
## dudosos), barra de Male +5 pp (dibujo · val 65.3472→70.3472), sin la pista de Male (dibujo · marca fija ausente) y
## Male/Female cambiadas de línea (verify · orden Male: 4→5); restaurado (md5). main.R --stage all --figs 01,04,09
## --strict → 3/3 ok, exit 0; Polonia y Malta (01, 04, 09) ok en figures (la 4 con N/A: 1 complemento, el N/A en la
## columna de valores); National o EU Average NA → «fig 01: sin promedio (…)» antes de llamar a WJPr.


## Bloque wjpr dumbbell — figuras 10, 15 ----------------------------------------------------------------------------
## Base (WJPr 1.2.0): WJPr::wjp_dumbbells() con sus argumentos documentados y layout = wjp_layout(), como fid_dumbbell
## (dev/report-fidelity.R:234-275 del paquete) pero desde el df del contrato: data larga (dos filas por línea de datos,
## una por serie; value / value2 crudos, NA = sin punto ni texto), grouping = df$label, headers = el label del último
## encabezado del df por encima de cada fila ("" en el promedio: sin encabezado), order = df$order, cgroups = los dos
## textos de la leyenda (series[1] = value, series[2] = value2), labels = df$text / df$text2 tal cual,
## highlight_levels = las filas avg, value_position = "outside" (a dot_r + 3 pt del punto, por fuera del par; un valor
## solitario a la derecha: la 18-24 de la 10; ninguno: la 18-24 de la 15, solo el rótulo). Colores de STD$col (main,
## exp, track, text, axis) y medidas de ST_DUMB / LABEL_COL[["dumb"]] / chart_dumbbell() (05_charts.R): columna de
## etiquetas L, pista W − L − 6 − 5 − 27, fila 18 pt, encabezado 22 con base 17, líneas desde 3 pt bajo la línea del
## eje (49 pt), rótulos del eje con base 44, leyenda con líneas base 10 y 24 (ptheme = WJP_theme() + theme(…): márgenes
## y separaciones de la leyenda y del eje). Alto: inset inferior = h_in · 72 − 49 − 3 − alto de las líneas (4 pt en
## Hungría, el rows$y + 4 del diseño), así que la gtable mide w_in × h_in (468 × 378 pt en svglite); si las líneas no
## caben, error legible. complemento: ninguno (complementos = FALSE devuelve la misma llamada).
## Lo dibujado por fila (contrato drawn de wjpr_meta): wjpr_dumbbell_drawn, una fila por línea del df en su posición
## (wjpr_dumbbell_pos, la regla de wjp_dumbbells) con val/val2 = punto de cada serie (por su color y la leyenda
## impresa), frac = val/100, text/text2, label_drawn (capa de rótulos) y grouping_drawn (último encabezado impreso).
## corregido (WJPr 1.2.0): wjp_dumbbells cambió (eje continuo en pt, rótulos como capa de texto, sin labpos por fila
## de un solo valor); el adaptador de 1.1.0 (13 complementos sobre sus capas) fallaba con «Problem while computing
## aesthetics» y su drawn con «el eje x no es discreto».
## Prueba (WJPr 1.2.0@1664a23, 2026-10-27, SCRATCH/fix5/M3): galería --figs 10,15 → ok, textos iguales, 0 dudosos, f1
## 1.000, local 0.000, «17 filas ok» en las dos, 0 complementos (1.1.0@382aee9 con el adaptador anterior: 13 y 13, f1
## 1.000, local 0); igual con --sin-complementos. main.R --country hungary --stage all --figs 10 --strict → ok, verify
## 153/153, exit 0. Inyecciones portadas a las capas de 1.2.0 (pruebas/iny_g1_M3.R con iny_run_M3.sh, --stage all
## --figs 10 --strict): db_textos (texto · text Male 29%→25%), db_etiquetas (texto · label_drawn Urban→Rural),
## db_punto (dibujo · val Male 29.46→34.46), db_oculto (texto · text2 Female 62%→) y db_leyenda (texto + dibujo) →
## verify fallo y exit 1 las cinco; en la 15 desde la referencia (pruebas/t_iny_ref.R) las tres del banco, detectadas.
## Auxiliares de este bloque (los usa también el bloque hbars): unidades del diseño → ggplot2 y edición de capas;
## los de lo dibujado por fila con eje discreto (wjpr_pos_slots, …, infraestructura) ya no los usa este bloque.

# Conversión de las medidas del diseño (pt = 1/72 in, la unidad del SVG) a ggplot2, comprobada en el SVG de svglite:
# linewidth L → trazo de L·.pt·0.75 pt (lwd 1 = 1/96 in); punto de forma 19 con stroke 0 → radio 0.375·size·.pt;
# texto: size = pt / .pt (los 3.514598 mm de WJPr = 10 pt). margin() y unit() van en "bigpts" (1/72 in): el "pt" de
# grid es 1/72.27 in y correría 0.4 % cada medida (0.7 pt a 184 pt).
wjpr_pt2lw <- function(pt) pt / (ggplot2::.pt * 0.75)
wjpr_pt2size <- function(r_pt) r_pt / (0.375 * ggplot2::.pt)
wjpr_unit_bp <- function(x) grid::unit(x, "bigpts")
wjpr_margin_bp <- function(t = 0, r = 0, b = 0, l = 0) ggplot2::margin(t, r, b, l, unit = "bigpts")
# Alto de la caja de una línea de texto de ggplot2 (ascenso de "M" + descenso de "gjpqyQ") con Inter Tight, en em:
# medido con ggplotGrob() en svglite (8.496 pt a 9 pt, 9.439 a 10; no depende del texto). Con vjust = 0 la línea base
# queda en el borde inferior de la caja.
WJPR_TEXT_BOX_EM <- 0.944

# Centro de cada línea del df (pt desde abajo: el x de los datos de wjp_dumbbells con layout) con la regla de
# wjp_dumbbells (dumbbellsChart.R:416-424 de WJPr 1.2.0): encabezado = su línea base header_baseline menos 0.36 em
# (WJP_CAP_HALF_EM), fila = mitad de su paso, desde inset[1]. Es la identidad de cada posición del eje en lo dibujado
# por fila (qué fila del df debe ocupar cada línea); lo impreso, los puntos y los valores se leen del dibujo.
# origen: dumbbellsChart.R:416-424 (WJPr 1.2.0@1664a23)
wjpr_dumbbell_pos <- function(kind, lay, header_size) {
  h <- ifelse(kind == "header", lay$header_height, lay$row_height)
  top <- lay$inset[1] + c(0, cumsum(h)[-length(h)])
  ctr <- ifelse(kind == "header", top + lay$header_baseline - 0.36 * header_size, top + lay$row_height / 2)
  sum(h) + sum(lay$inset) - ctr
}

# Lo dibujado por fila del dumbbell (contrato drawn de wjpr_meta), leído de b = ggplot_build() del gráfico final: una
# fila por línea del df (lines: order, kind, grouping, label y pos = su centro, wjpr_dumbbell_pos) en orden visual de
# arriba abajo. Cada marca del dibujo cae en la línea cuyo centro está a menos de media separación entre líneas:
# val/val2 = la posición (0-100 %) del punto de cada serie en esa línea (serie por su color y la leyenda impresa;
# series[1] = value, series[2] = value2; sin leyenda, según cols) y frac/frac2 = val/100 (el frac de la referencia);
# text/text2 = los valores impresos en la línea (capas de texto de valores), cada uno de la serie del punto más cercano
# (van por fuera del par, a dot_r + 3 pt de su punto; sin punto en la línea, la de su color); label_drawn = el rótulo
# impreso en la columna de etiquetas en esa línea (capa de texto de las líneas de wjp_dumbbells: la que trae la columna
# kind) y grouping_drawn = el del último encabezado impreso por encima (el propio en un encabezado; "" en el promedio,
# antes del primero). Dos textos o dos rótulos intercambiados, un punto desplazado o un encabezado cambiado se ven aquí
# aunque el multiconjunto de textos sea igual.
# corregido (WJPr 1.2.0): con layout, wjp_dumbbells dibuja las líneas en un eje continuo (pt) y los rótulos como capa de
# texto, no como etiquetas de un eje discreto: wjpr_pos_slots() ya no sirve («el eje x no es discreto»)
wjpr_dumbbell_drawn <- function(b, lines, series, cols) {
  num <- function(d, nm) if (nm %in% names(d)) suppressWarnings(as.numeric(d[[nm]])) else rep(NA_real_, nrow(d))
  half <- if (nrow(lines) > 1) min(diff(sort(lines$pos))) / 2 else Inf
  line_of <- function(x) vapply(suppressWarnings(as.numeric(x)), function(v) {
    if (!is.finite(v)) return(NA_integer_)
    d <- abs(lines$pos - v); j <- which.min(d)
    if (d[j] < half) j else NA_integer_
  }, NA_integer_)
  lg <- wjpr_legend_drawn(b)
  # unname: ggplot2 4 nombra las capas y bind_rows leería una lista con nombre como columnas
  ip <- unname(which(vapply(b$plot$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))))
  pts <- bind_rows(tibble(line = integer(), y = numeric(), serie = character()), lapply(ip, function(i) {
    d <- as.data.frame(b$data[[i]])
    ok <- is.finite(num(d, "x")) & is.finite(num(d, "y")) & coalesce(num(d, "size"), 1) > 0 &
      coalesce(num(d, "alpha"), 1) > 0
    d <- d[ok, , drop = FALSE]
    tibble(line = line_of(d$x), y = as.numeric(d$y), serie = wjpr_series_of(d$colour, lg, "colour", series, cols))
  }))
  pts <- pts[!is.na(pts$line) & !is.na(pts$serie), , drop = FALSE]
  tx <- wjpr_drawn_texts(b)
  tx <- tx[tx$source == "layer" & !is.na(tx$label), , drop = FALSE]      # solo lo que se ve
  tx$line <- line_of(tx$x); tx <- tx[!is.na(tx$line), , drop = FALSE]
  is_lab <- vapply(b$plot$layers, function(l) is.data.frame(l$data) && "kind" %in% names(l$data), logical(1))
  lab <- tx[is_lab[tx$layer], , drop = FALSE]; val <- tx[!is_lab[tx$layer], , drop = FALSE]
  val$serie <- vapply(seq_len(nrow(val)), function(j) {
    p <- pts[pts$line == val$line[j], , drop = FALSE]
    if (nrow(p)) p$serie[which.min(abs(p$y - val$y[j]))] else wjpr_series_of(val$colour[j], lg, "colour", series, cols)
  }, NA_character_)
  idx <- seq_len(nrow(lines))
  lab_drawn <- vapply(idx, function(k) paste(lab$label[lab$line == k], collapse = " "), "")
  hdr <- ""; grp_drawn <- character(length(idx))
  for (k in order(-lines$pos)) {                                         # de arriba abajo
    if (lines$kind[k] == "header") hdr <- lab_drawn[k]
    grp_drawn[k] <- hdr
  }
  val_of <- function(k, se) { y <- pts$y[pts$line == k & pts$serie %in% se]; if (length(y)) y[1] else NA_real_ }
  txt_of <- function(k, se) paste(val$label[val$line == k & val$serie %in% se], collapse = " ")
  v1 <- vapply(idx, val_of, 0, se = series[1]); v2 <- vapply(idx, val_of, 0, se = series[2])
  out <- tibble(order = as.integer(rank(-lines$pos, ties.method = "first")), kind = lines$kind, grouping = lines$grouping,
                label = lines$label, frac = v1 / 100, frac2 = v2 / 100,
                text = vapply(idx, txt_of, "", se = series[1]), text2 = vapply(idx, txt_of, "", se = series[2]),
                label_drawn = lab_drawn, grouping_drawn = grp_drawn, val = v1, val2 = v2)
  out[order(out$order), , drop = FALSE]
}
wjpr_dumbbell_drawn_fun <- function(lines, series, cols) {
  force(lines); force(series); force(cols)
  function(b) wjpr_dumbbell_drawn(b, lines, series, cols)
}

wjpr_dumbbell <- function(df, spec, legend = c("General population: perception",
                                               "People with a non-trivial problem: experience"),
                          st = ST_DUMB, complementos = wjpr_complementos()) {
  wjpr_require()
  # corregido (WJPr 1.2.0): el adaptador usa layout, headers y value_position de wjp_dumbbells 1.2.0; con 1.1.0, error
  # legible en lugar de «argumento no usado»
  if (!wjpr_has_fun("wjp_layout")) stop("wjpr_dumbbell requiere WJPr >= 1.2.0 (wjp_layout)", call. = FALSE)
  df <- df[order(df$order), , drop = FALSE]
  geo <- list(leg_base = c(10, 24), leg_dot = 3.4, ax_base = 44, ax_line = 49, rows_gap = 3)  # chart_dumbbell()
  W <- spec$w_in * 72; H <- spec$h_in * 72
  L <- label_col_pt(spec, tibble(kind = df$kind, label = df$label), st)
  track <- W - L - st$gap_label - st$gap_value - st$value_w                     # la pista llena el ancho (fill_w)
  # encabezado de cada fila de datos: el label del último encabezado del df por encima ("" en el promedio)
  hdr <- character(nrow(df)); cur <- ""
  for (i in seq_len(nrow(df))) {
    if (df$kind[i] == "header") cur <- df$label[i]
    hdr[i] <- if (df$kind[i] == "avg") "" else cur
  }
  r <- df$kind != "header"
  # wjp_dumbbells identifica cada línea por su rótulo (grouping): dos filas con el mismo rótulo se fundirían
  dup <- unique(df$label[r][duplicated(df$label[r])])
  if (length(dup)) stop("wjpr_dumbbell: rótulos repetidos (wjp_dumbbells los fundiría en una línea): ",
                        paste(dup, collapse = ", "), call. = FALSE)
  t1 <- coalesce(as.character(df$text), ""); t2 <- coalesce(as.character(df$text2), "")
  long <- bind_rows(tibble(cat = df$label[r], grp = hdr[r], serie = legend[1], v = as.numeric(df$value[r]), lab = t1[r]),
                    tibble(cat = df$label[r], grp = hdr[r], serie = legend[2], v = as.numeric(df$value2[r]), lab = t2[r]))
  # alto: las líneas empiezan rows_gap pt bajo la línea del eje y el inset inferior lleva el dibujo hasta h_in (4 pt
  # en Hungría: el rows$y + 4 de chart_dumbbell); la gtable mide así w_in × h_in en svglite (el dispositivo del SVG)
  h_lines <- sum(ifelse(df$kind == "header", st$header, st$pitch))
  bottom <- H - geo$ax_line - geo$rows_gap - h_lines
  if (bottom < -0.01) {
    stop(sprintf("wjpr_dumbbell: las líneas miden %.1f pt y no caben en h_in = %.3f in (hace falta %.3f)", h_lines,
                 spec$h_in, (geo$ax_line + geo$rows_gap + h_lines) / 72), call. = FALSE)
  }
  lay <- WJPr::wjp_layout(label_width = L, label_gap = st$gap_label, track_width = track, value_gap = st$gap_value,
                          value_width = st$value_w, row_height = st$pitch, header_height = st$header,
                          header_baseline = st$header_base, inset = c(geo$rows_gap, max(bottom, 0)), margins = 0)
  # leyenda (tema): la clave de un punto mide su diámetro de dibujo (size/10 cm en ggplot2: 10.1 pt con r = 3.8) aunque
  # legend_key_size pida 2·dot_r; se corre a la izquierda para que el círculo quede en x0 + dot_r y el texto en
  # x0 + 2·dot_r + 5; claves de 14 pt (líneas base 10 y 24) desde top = −0.4 pt y el hueco hasta los rótulos del eje
  # (base 44 a 5 pt de la línea del eje, en 49) por legend.box.spacing
  key_w <- wjpr_pt2size(st$dot_r) / 10 / 2.54 * 72
  shift <- key_w / 2 - st$dot_r
  key_h <- diff(geo$leg_base)
  top <- geo$leg_base[1] - geo$leg_dot - key_h / 2
  ax_box <- WJPR_TEXT_BOX_EM * st$size_axis
  ptheme <- WJPr::WJP_theme() + ggplot2::theme(
    axis.text.x.top = ggplot2::element_text(size = st$size_axis, colour = st$col$text, vjust = 0,
                                            margin = wjpr_margin_bp(b = geo$ax_line - geo$ax_base)),
    legend.text = ggplot2::element_text(colour = st$col$text, margin = wjpr_margin_bp(l = 5 - shift)),
    legend.key.spacing.y = wjpr_unit_bp(0), legend.key.height = wjpr_unit_bp(key_h),
    legend.margin = wjpr_margin_bp(t = top, l = -shift), legend.box.margin = wjpr_margin_bp(),
    legend.box.spacing = wjpr_unit_bp(geo$ax_base - ax_box - (top + 2 * key_h)))
  p <- WJPr::wjp_dumbbells(
    long, target = "v", grouping = "cat", cgroups = legend, colors = "serie", labels = "lab",
    cvec = c(st$col$main, st$col$exp), order = stats::setNames(df$order[r], df$label[r]),
    legend_direction = "vertical", legend_text_size = st$size_legend, legend_key_size = 2 * st$dot_r,
    legend_fontface = "plain", headers = "grp", header_fontface = "semibold.italic", header_colour = st$col$main,
    highlight_levels = df$label[df$kind == "avg"], highlight_fontface = "semibold.italic",
    highlight_colour = st$col$main, label_align = "right", label_colour = st$col$text, label_size = st$size_label,
    breaks = seq(0, 100, 25), axis_line = TRUE, axis_line_color = st$col$axis, strips = FALSE, grid = FALSE,
    value_position = "outside", value_gap = 3, value_fontface = "semibold.italic", value_size = st$size_value,
    point_size = wjpr_pt2size(st$dot_r), point_stroke = 0, segment_color = st$col$track,
    segment_width = wjpr_pt2lw(st$segment), clip = "off", layout = lay, ptheme = ptheme)
  comp <- character()
  lines <- tibble(order = df$order, kind = df$kind, grouping = df$grouping, label = df$label,
                  pos = wjpr_dumbbell_pos(df$kind, list(header_height = st$header, row_height = st$pitch,
                                                        header_baseline = st$header_base,
                                                        inset = c(geo$rows_gap, max(bottom, 0))), st$size_label))
  wjpr_meta(p, base = "WJPr::wjp_dumbbells", complementos = if (isTRUE(complementos)) comp else character(),
            drawn = wjpr_dumbbell_drawn_fun(lines, legend, c(st$col$main, st$col$exp)))
}


## Bloque wjpr hbars — figuras 13, 14, 18, 19, 36 -------------------------------------------------------------------
## Base (revisión 5, 2026-10-27): WJPr::wjp_bars(direction = "horizontal") de WJPr 1.2.0 con sus argumentos, como
## fid_hbars (dev/report-fidelity.R del paquete) pero desde el df del contrato: una fila por categoría (sin
## encabezados) con value tal cual (en %, la escala de la pista) y text impreso tal cual, una serie en el color
## principal, order = df$order; value_position "outside" (el valor gap_value pt tras el final de la barra) con
## value_size, value_fontface "semibold.italic" y value_colors (color del texto del reporte); label_align "left",
## label_size y label_colour (con wjp_layout la columna de etiquetas es una capa de texto, aquí desde x = 2 pt);
## expand_discrete (la primera barra 2 pt bajo el arranque de la rejilla; la última fila llega al borde inferior) y
## layout = wjp_layout() con las medidas de ST_HBARS y LABEL_COL[["hbars"]] (chart_hbars_axis, report_charts.R:224-242:
## etiquetas desde x = 2, barras desde L + 12, pista de track pt, valores a gap_value pt y el resto de W para la columna
## de valores, filas de pitch pt con barras de bar pt desde 20 pt, rejilla desde 18 pt, rótulos del eje con la línea
## base en 12 pt); ptheme = wjpr_theme() + theme(axis.text.x.top: tamaño, color, vjust 0 y margen que deja la rejilla
## 6 pt bajo esa línea base). El dibujo mide W × H del manifiesto (spec$w_in × spec$h_in): ancho = margen + columna de
## etiquetas + pista + columna de valores; alto = margen + eje + filas hasta el borde inferior.
## Complementos: ninguno (complementos = FALSE devuelve la misma llamada). Se quitan las bases alternativas de la
## revisión 3 (wjp_lollipops, wjp_edgebars), que solo existían como complementos de la API de 1.1.0.
## Lo dibujado por fila (nuevo en la revisión 5; contrato drawn de wjpr_meta): wjpr_wb_drawn (bloque paired_bars),
## una fila por categoría de arriba abajo con val = largo de la barra (en %), frac = val / 100, text = el valor impreso
## en esa posición y label_drawn = la etiqueta impresa en la columna de etiquetas.
## Prueba: ver «## Prueba (revisión 5)» al final del bloque severity_bars (los cuatro bloques de wjp_bars).

# complementos: se conserva el argumento (lo pasan la galería y main por options(wjpr.complementos)), sin efecto: la
# figura es la llamada a WJPr sola
wjpr_hbars <- function(df, spec, st = ST_HBARS, complementos = wjpr_complementos()) {
  wjpr_require(); wjpr_wb_need()
  df <- df[order(df$order), , drop = FALSE]
  rows <- df[df$kind != "header", , drop = FALSE]
  W <- spec$w_in * 72; H <- spec$h_in * 72; n <- nrow(rows)
  L <- label_col_pt(spec, tibble(kind = rows$kind, label = rows$label), st)
  geo <- list(lab_x = 2, gap = 12, ax_base = 12, rows0 = 20, grid0 = 18)     # chart_hbars_axis()
  x0 <- L + geo$gap                                                        # arranque de las barras
  d <- tibble(order = rows$order, label = rows$label, value = as.numeric(rows$value),
              text = coalesce(as.character(rows$text), ""), serie = "main")
  # corregido (revisión 5): el alto sale de H y de las filas (la última llega al borde inferior), así el dibujo de
  # wjp_layout mide h_in; antes la escala discreta repartía el alto del lienzo
  bottom <- wjpr_wb_fill(H - geo$rows0, n, st$pitch, spec)
  p <- WJPr::wjp_bars(
    d, target = "value", grouping = "label", labels = "text", colors = "serie", cvec = c(main = st$col$main),
    direction = "horizontal", order = "order",
    # corregido (revisión 5): el valor al tamaño de las etiquetas (10 pt), como lo publicado (horizontal_bars.R:31-38:
    # geom_text de 5 mm = 14.2 pt junto a etiquetas de 14 pt) y el adaptador anterior (el tamaño por defecto de
    # wjp_bars); con size_value (9 pt) de STD, parecido local 0.10-0.16 en los valores
    value_position = "outside", value_offset = st$gap_value, value_size = st$size_label,
    value_fontface = "semibold.italic", value_colors = st$col$text,
    label_align = "left", label_size = st$size_label, label_colour = st$col$text,
    expand_discrete = c(0.5 + (geo$rows0 - geo$grid0) / st$pitch, bottom),
    layout = WJPr::wjp_layout(label_width = x0 - geo$gap - geo$lab_x, label_gap = geo$gap, track_width = st$track,
                              value_gap = st$gap_value, value_width = W - x0 - st$track - st$gap_value,
                              row_height = st$pitch, bar_height = st$bar,
                              margins = c(geo$ax_base - WJPR_WB_BOX_EM * st$size_label, 0, 0, geo$lab_x)),
    ptheme = wjpr_theme() + ggplot2::theme(axis.text.x.top = ggplot2::element_text(
      size = st$size_label, colour = st$col$text, vjust = 0, margin = wjpr_wb_margin(b = geo$grid0 - geo$ax_base))))
  # corregido (revisión 5): lo dibujado por fila (antes el adaptador no lo exponía: solo lo medían el parecido y el
  # multiconjunto de textos, que no ven dos etiquetas o dos valores intercambiados ni una barra más larga)
  drawn <- wjpr_wb_drawn_fun(tibble(level = rows$label, kind = rows$kind, grouping = rows$grouping, label = rows$label),
                             series = "main", fills = st$col$main, scale = 100)
  wjpr_meta(p, base = "WJPr::wjp_bars", drawn = drawn)
}


## Bloque wjpr paired_bars — figuras 2 ------------------------------------------------------------------------------
## Las barras pareadas (2), las columnas (30, 37), la severidad (7) y las barras horizontales (13, 14, 18, 19, 36) son,
## desde la revisión 5 (2026-10-27, WJPr 1.2.0), una llamada a WJPr::wjp_bars() con sus argumentos (position = "dodge",
## track, value_*, label_*, legend_*, limits, expand_*, ref_lines, baseline, layout = wjp_layout(), ptheme), como los
## fid_<tipo> de dev/report-fidelity.R del paquete pero desde el df del contrato (value crudo, text impreso tal cual)
## y con las medidas de STD/ST_*/LABEL_COL (04_svg.R) y de spec (w_in, h_in, base_pt, country). Este bloque define los
## auxiliares comunes (prefijo wjpr_wb_): la build mínima, el alto que llena H, los datos largos país | UE, unidades
## en pt de 1/72 in, la leyenda {country} | EU Average dentro del panel y lo dibujado por fila de barras con una o dos
## series por categoría, y el interlineado WJPR_WB_LH (lo usa también el bloque columns). corregido (integración,
## 2026-10-28): se quitan wjpr_wb_layer, wjpr_wb_is, wjpr_wb_font y wjpr_wb_font_comp, que solo usaba el bloque funnel
## de 1.1.0 (con wjp_funnel ya nadie los llama). Ya no hay ramas por build: con WJPr < 1.2.0 los adaptadores se detienen
## (wjpr_wb_need) en vez de dibujar con los complementos de 1.1.0.
## Fig 2 (ST_WIDE2): ancho 8.4 in con columna de etiquetas propia (la más ancha + 2 pt, label_column) y etiquetas a la
## derecha, dos barras de pair_bar pt (2 pt entre ellas: dodge_width) por fila de pair_pitch pt, cada una con su pista
## al 100 % (track), los valores en una columna a gap_value pt del final de la pista (value_position "column") en el
## color de su serie, sin eje de valores ni rejilla, la leyenda dentro del panel arriba a la izquierda y 20 pt sobre
## la primera fila (expand_discrete); la última fila llega al borde inferior (el dibujo mide W × H).
## Complementos: ninguno (bare / complementos = FALSE devuelve la misma llamada).
## Lo dibujado por fila (revisión 4; reescrito en la 5 para las capas de wjp_bars 1.2.0): wjpr_wb_drawn, una fila por
## categoría con val/frac (país) y val2/frac2 (UE) según el relleno de cada barra y la leyenda impresa, text/text2 de
## la barra más cercana y label_drawn = la categoría impresa (con wjp_layout, en la capa de texto de la columna de
## etiquetas, a la izquierda de la pista; antes, en el eje).

# Medidas de Inter Tight en el texto de grid (medidas en el SVG de svglite), en em: ascendente que apoya vjust = 1,
# alto de un texto de eje (ascendente + descendente de la fuente) y alto de la caja de una línea de ggplot2 (el
# WJP_TEXT_BOX_EM de WJPr, layout.R: con vjust = 0 la línea base queda en su borde inferior)
WJPR_WB_ASC <- 0.725
WJPR_WB_TEXT_H <- 0.94
WJPR_WB_BOX_EM <- 0.944
WJPR_WB_LH <- 1.22 / 1.2       # interlineado de las figuras propias (1.22 em) en lineheight de grid (1.2 em por línea)
# unit() y margin() en pt de 1/72 in ("bigpts"), la unidad del diseño y del SVG (el "pt" de grid es 1/72.27 in)
wjpr_wb_bp <- function(x) grid::unit(x, "bigpts")
wjpr_wb_margin <- function(t = 0, r = 0, b = 0, l = 0) ggplot2::margin(t, r, b, l, unit = "bigpts")

# corregido (revisión 5): los adaptadores de barras son la llamada a wjp_bars de WJPr 1.2.0 (position, layout,
# value_position, ref_lines…); con una build sin esos argumentos se detienen con un error legible (antes, ramas de
# complementos para 1.1.0 y main)
wjpr_wb_need <- function(args = c("position", "layout", "value_position", "track", "ref_lines", "baseline")) {
  falta <- args[!vapply(args, function(a) wjpr_has_arg("wjp_bars", a), logical(1))]
  if (length(falta)) {
    stop("WJPr ", wjpr_build(), ": wjp_bars no tiene ", paste(falta, collapse = ", "),
         " (los adaptadores de barras piden WJPr ≥ 1.2.0)", call. = FALSE)
  }
  invisible(TRUE)
}

# Expansión inferior (en filas, expand_discrete de wjp_bars) para que n filas de pitch pt que empiezan avail pt por
# encima del borde inferior lleguen a ese borde: (avail − n·pitch) / pitch + 0.5. Si no caben, error legible (con
# wjp_layout el dibujo mediría más que h_in y no llenaría el lienzo como la publicada)
wjpr_wb_fill <- function(avail, n, pitch, spec) {
  b <- (avail - n * pitch) / pitch + 0.5
  if (!is.finite(b) || b < 0) {
    stop(sprintf("fig %02d: %d filas de %s pt no caben en el alto del manifiesto (h_in = %s in)", as.integer(spec$fig),
                 as.integer(n), format(pitch), format(spec$h_in)), call. = FALSE)
  }
  b
}

# Datos largos para wjp_bars: una fila por categoría y serie (país primero), value tal cual y el texto impreso tal cual
# corregido (revisión 5): un valor suprimido entra NA (wjp_bars 1.2.0 no dibuja su barra, pone su pista a 0 y conserva
# el sitio de la otra serie); antes 0 con na = TRUE para la pista propia
wjpr_wb_long <- function(df, spec) {
  rows <- df %>% filter(kind == "row") %>% arrange(order)
  lv <- c(spec$country, "EU Average")
  bind_rows(rows %>% transmute(order, label, serie = lv[1], v = as.numeric(value), lab = text),
            rows %>% transmute(order, label, serie = lv[2], v = as.numeric(value2), lab = text2)) %>%
    mutate(serie = factor(serie, levels = lv), lab = coalesce(as.character(lab), ""))
}

# Leyenda {country} | EU Average dentro del panel, arriba a la izquierda (legend_position = c(0, 1), legend_key_size =
# 9 y legend_text_size de wjp_bars): texto del color del reporte a gap_key pt de su clave, gap_item pt entre elementos
# y top pt sobre la caja (chart_paired / chart_columns). Va por ptheme: wjp_bars no tiene argumento para el color del
# texto de la leyenda ni para sus márgenes (wjp_legend_theme fija #524F4C)
wjpr_wb_legend_theme <- function(st, top = 2, gap_key = 4, gap_item = 14) {
  ggplot2::theme(legend.text = ggplot2::element_text(colour = st$col$text, margin = wjpr_wb_margin(l = gap_key)),
                 legend.key.spacing.x = wjpr_wb_bp(gap_item), legend.margin = wjpr_wb_margin(top, 0, 0, 0))
}

# Lo dibujado por fila de las barras de wjp_bars con una o dos series por categoría (paired_bars, columns, hbars;
# contrato drawn de wjpr_meta), leído de b = ggplot_build() del gráfico final: una fila por categoría del eje
# (wjpr_pos_slots: en horizontal, con coord_flip, de arriba abajo; en vertical, de izquierda a derecha), con
# kind/grouping/label = la fila del df cuya categoría ocupa esa posición (keys$level = el nivel del eje). Cada barra
# es de la serie que la leyenda impresa da a su relleno (wjpr_legend_drawn; sin leyenda, según fills): series[1] →
# val = su largo (en las unidades de df$value) y frac = val / scale (el frac de la referencia: 100 en las barras con
# pista, la columna más alta en las columnas); series[2] → val2, frac2. La pista (relleno track_fill) no es serie; una
# barra sin largo finito (valor NA: wjp_bars no la dibuja) o de largo 0 con pista 0 o sin pista es un valor suprimido
# → val NA. text/text2 = los valores impresos en la posición (textos de capa en el lado de los valores, y ≥ 0), cada
# uno de la serie de la barra más cercana (van esquivados con ella); label_drawn = la categoría impresa en esa
# posición: la etiqueta del eje o, con wjp_layout en horizontal, la capa de texto de la columna de etiquetas (y < 0:
# a la izquierda del arranque de la pista). Dos textos o dos categorías intercambiados, una barra más larga o las
# series cambiadas en una fila se ven aquí aunque el multiconjunto de textos sea igual.
# origen: wjpr_pb_drawn (revisión 4) + la columna de etiquetas de wjp_layout y la escala de las columnas
wjpr_wb_drawn <- function(b, keys, series, fills, track_fill = NULL, scale = 100) {
  slots <- wjpr_pos_slots(b, "x")
  lg <- wjpr_legend_drawn(b)
  num <- function(d, nm) if (nm %in% names(d)) suppressWarnings(as.numeric(d[[nm]])) else rep(NA_real_, nrow(d))
  ib <- unname(which(vapply(b$plot$layers, function(l) inherits(l$geom, c("GeomCol", "GeomBar", "GeomRect")), logical(1))))
  bars <- bind_rows(tibble(x = numeric(), slot = integer(), len = numeric(), track = logical(), serie = character()),
                    lapply(ib, function(i) {
    d <- as.data.frame(b$data[[i]])
    if (!all(c("x", "fill") %in% names(d)) || !nrow(d)) return(NULL)
    tibble(x = num(d, "x"), slot = wjpr_pos_slot_of(d$x, slots), len = abs(num(d, "ymax") - num(d, "ymin")),
           track = if (is.null(track_fill)) rep(FALSE, nrow(d)) else wjpr_same_color(d$fill, track_fill),
           serie = wjpr_series_of(d$fill, lg, "fill", series, fills))
  }))
  bars <- bars[!is.na(bars$slot), , drop = FALSE]
  trk <- bars[bars$track, , drop = FALSE]
  ser <- bars[!bars$track & !is.na(bars$serie), , drop = FALSE]
  ser$val <- vapply(seq_len(nrow(ser)), function(j) {
    if (!is.finite(ser$len[j])) return(NA_real_)
    t <- trk$len[abs(trk$x - ser$x[j]) < 1e-6]
    if (ser$len[j] <= 1e-9 && (!length(t) || all(!is.finite(t) | t <= 1e-9))) NA_real_ else ser$len[j]
  }, 0)
  tx <- wjpr_drawn_texts(b)
  tx <- tx[!is.na(tx$label), , drop = FALSE]
  lay <- tx[tx$source == "layer", , drop = FALSE]
  in_col <- is.finite(lay$y) & lay$y < 0                 # columna de etiquetas de wjp_layout (a la izquierda de 0)
  txt <- lay[!in_col, , drop = FALSE]
  txt$slot <- wjpr_pos_slot_of(txt$x, slots); txt <- txt[!is.na(txt$slot), , drop = FALSE]
  txt$serie <- vapply(seq_len(nrow(txt)), function(j) {
    s <- ser[ser$slot == txt$slot[j], , drop = FALSE]
    if (nrow(s)) s$serie[which.min(abs(s$x - txt$x[j]))] else NA_character_
  }, NA_character_)
  cat <- bind_rows(tx[tx$source == "axis-x", , drop = FALSE], lay[in_col, , drop = FALSE])
  cat$slot <- wjpr_pos_slot_of(cat$x, slots)
  k <- match(slots$level, keys$level)
  val_of <- function(s, se) { v <- ser$val[ser$slot == s & ser$serie %in% se]; if (length(v)) v[1] else NA_real_ }
  txt_of <- function(s, se) paste(txt$label[txt$slot == s & txt$serie %in% se], collapse = " ")
  two <- length(series) > 1
  v1 <- vapply(slots$slot, val_of, 0, se = series[1])
  v2 <- if (two) vapply(slots$slot, val_of, 0, se = series[2]) else rep(NA_real_, nrow(slots))
  out <- tibble(order = slots$slot, kind = coalesce(keys$kind[k], ""), grouping = coalesce(keys$grouping[k], ""),
                label = coalesce(keys$label[k], slots$level), frac = v1 / scale, frac2 = v2 / scale,
                text = vapply(slots$slot, txt_of, "", se = series[1]),
                text2 = if (two) vapply(slots$slot, txt_of, "", se = series[2]) else "",
                label_drawn = vapply(slots$slot, function(s) paste(cat$label[cat$slot %in% s], collapse = " "), ""),
                grouping_drawn = NA_character_, val = v1, val2 = v2)
  out[order(out$order), , drop = FALSE]
}
wjpr_wb_drawn_fun <- function(keys, series, fills, track_fill = NULL, scale = 100) {
  force(keys); force(series); force(fills); force(track_fill); force(scale)
  function(b) wjpr_wb_drawn(b, keys, series, fills, track_fill, scale)
}

# bare (= complementos FALSE) se conserva por compatibilidad, sin efecto: no hay complementos
wjpr_paired_bars <- function(df, spec, bare = !wjpr_complementos(), ...) {
  wjpr_require(); wjpr_wb_need()
  st <- ST_WIDE2                                     # pista a toda la columna: pair_pitch 21, pair_bar 8
  W <- spec$w_in * 72; H <- spec$h_in * 72
  rows <- df %>% filter(kind == "row") %>% arrange(order)
  d <- wjpr_wb_long(df, spec); lv <- levels(d$serie)
  L <- label_column(list(rows), st)                  # columna de etiquetas propia (borde derecho = la más ancha + 2)
  track <- W - L - st$gap_label - st$gap_value - st$value_w     # la pista llega hasta la columna de valores
  top <- 20                                          # la leyenda va en los 20 pt sobre la primera fila
  p <- WJPr::wjp_bars(
    d, target = "v", grouping = "label", labels = "lab", colors = "serie", order = "order",
    cvec = stats::setNames(c(st$col$main, st$col$eu), lv), direction = "horizontal",
    position = "dodge", dodge_width = 2 * (st$pair_bar + 2) / st$pair_pitch,
    track = TRUE, track_color = st$col$track,
    value_position = "column", value_offset = st$gap_value, value_size = st$size_value,
    value_fontface = "semibold.italic", value_colors = stats::setNames(c(st$col$main, st$col$eu_text), lv),
    value_axis = FALSE, grid = FALSE,
    label_align = "right", label_size = st$size_label, label_colour = st$col$text,
    show_legend = TRUE, legend_position = c(0, 1), legend_text_size = st$size_legend, legend_key_size = 9,
    legend_fontface = "plain",
    expand_discrete = c(0.5 + top / st$pair_pitch, wjpr_wb_fill(H - top, nrow(rows), st$pair_pitch, spec)),
    layout = WJPr::wjp_layout(label_width = L, label_gap = st$gap_label, track_width = track,
                              value_gap = st$gap_value, value_width = st$value_w,
                              row_height = st$pair_pitch, bar_height = st$pair_bar),
    ptheme = wjpr_theme() + wjpr_wb_legend_theme(st, top = 2))
  drawn <- wjpr_wb_drawn_fun(tibble(level = rows$label, kind = rows$kind, grouping = rows$grouping, label = rows$label),
                             lv, c(st$col$main, st$col$eu), track_fill = st$col$track, scale = 100)
  wjpr_meta(p, base = "WJPr::wjp_bars", drawn = drawn)
}


## Bloque wjpr columns — figuras 30, 37 -----------------------------------------------------------------------------
## Base (revisión 5, 2026-10-27): WJPr::wjp_bars() vertical de WJPr 1.2.0 con sus argumentos, como fid_columns pero
## desde el df del contrato: país y UE lado a lado (position "dodge"; columnas de col_w pt con gap pt entre ellas:
## width y dodge_width en fracción del ancho de grupo gw = (W − 2·m) / n), alturas relativas al máximo de las dos
## series (limits = c(0, vmax · yb / col_h) y expand_mult 0: la más alta mide col_h pt y su cima queda a yb − col_h pt
## del borde superior), valores a 4 pt de la cima de su columna (value_position "outside", value_offset 4) en semibold
## cursiva del color de su serie, sin eje de valores, rejilla ni título (value_axis, grid, ylab = NULL), línea base
## (baseline, del color del eje; clip "off" para que no se recorte la mitad de su trazo), etiquetas debajo
## (label_size, label_colour, label_lineheight de 1.22 em) y la leyenda {country} | EU Average dentro del panel
## arriba a la izquierda (legend_*, wjpr_wb_legend_theme con 3 pt arriba); ptheme = wjpr_theme() + theme(): m pt a
## los lados y, abajo, lo que deja la línea base en yb pt (H − yb − margen de la etiqueta − alto de sus líneas), con la
## línea base de la etiqueta a 6 + 0.95·size pt bajo la de las columnas (chart_columns). Sin wjp_layout: el panel llena
## el lienzo W × H del manifiesto y esas medidas lo fijan.
## Complementos: ninguno, salvo que una etiqueta no quepa en su grupo (gw − 10 pt): entonces el ajuste de línea por
## ancho en pt (wrap_text), porque label_wrap de wjp_bars cuenta caracteres (en la 30 y la 37 no hace falta).
## Lo dibujado por fila (nuevo en la revisión 5): wjpr_wb_drawn con scale = vmax (frac = val / vmax, como el frac de la
## referencia: el alto relativo a la columna más alta), una fila por categoría de izquierda a derecha, label_drawn = la
## etiqueta impresa en el eje.

# bare (= complementos FALSE): sin el ajuste de línea propio (las etiquetas como vienen)
wjpr_columns <- function(df, spec, bare = !wjpr_complementos(), ...) {
  wjpr_require(); wjpr_wb_need()
  st <- ST_COLS                                      # col_w 46, col_h 180
  W <- spec$w_in * 72; H <- spec$h_in * 72
  rows <- df %>% filter(kind == "row") %>% arrange(order)
  d <- wjpr_wb_long(df, spec); lv <- levels(d$serie)
  n <- nrow(rows); m <- 6; gap <- 4
  gw <- (W - 2 * m) / n                              # ancho de cada grupo
  vmax <- max(d$v, na.rm = TRUE)
  yb <- 24 + st$size_value + 6 + st$col_h            # línea base (pt desde arriba): la más alta llega a 39 pt
  labs <- rows$label
  comp <- character()
  wrapped <- vapply(labs, function(s) paste(wrap_text(s, st$size_label, gw - 10), collapse = "\n"), "", USE.NAMES = FALSE)
  if (!bare && any(wrapped != labs)) {
    # complemento: label_wrap de wjp_bars corta por número de caracteres (strwrap) y el diseño por ancho (gw − 10 pt,
    # wrap_text); las etiquetas entran con el salto ya puesto (wjp_bars conserva los saltos existentes). propuesta:
    # label_wrap_width = <pt> (ancho máximo de línea en puntos, medido con la fuente)
    d$label <- wrapped[match(d$label, labs)]
    labs <- wrapped
    comp <- "etiquetas: ajuste de línea por ancho en pt"
  }
  nl <- max(lengths(strsplit(labs, "\n", fixed = TRUE)))
  mt <- 6 + 0.95 * st$size_label - WJPR_WB_ASC * st$size_label   # grid apoya en el margen el tope de la ascendente
  text_h <- (WJPR_WB_TEXT_H + (nl - 1) * 1.22) * st$size_label
  p <- WJPr::wjp_bars(
    d, target = "v", grouping = "label", labels = "lab", colors = "serie", order = "order",
    cvec = stats::setNames(c(st$col$main, st$col$eu), lv), direction = "vertical",
    position = "dodge", width = 2 * st$col_w / gw, dodge_width = 2 * (st$col_w + gap) / gw,
    value_position = "outside", value_offset = 4, value_size = st$size_value, value_fontface = "semibold.italic",
    value_colors = stats::setNames(c(st$col$main, st$col$eu_text), lv),
    limits = c(0, vmax * yb / st$col_h), expand_mult = 0, expand_discrete = 0.5,
    value_axis = FALSE, grid = FALSE, ylab = NULL, baseline = TRUE, baseline_color = st$col$axis, clip = "off",
    label_size = st$size_label, label_colour = st$col$text, label_lineheight = WJPR_WB_LH,
    show_legend = TRUE, legend_position = c(0, 1), legend_text_size = st$size_legend, legend_key_size = 9,
    legend_fontface = "plain",
    # el elemento axis.text.x de ptheme se aplica al final (WJPr 1.2.0) y, al heredar de WJP_theme() el gris #524F4C,
    # taparía label_colour: lleva también el color y el tamaño de las etiquetas
    ptheme = wjpr_theme() + wjpr_wb_legend_theme(st, top = 3) + ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = st$size_label, colour = st$col$text, vjust = 1,
                                          margin = wjpr_wb_margin(t = mt)),
      plot.margin = wjpr_wb_margin(0, m, max(0, H - yb - mt - text_h), m)))
  # corregido (revisión 5): lo dibujado por fila (antes el adaptador no lo exponía)
  drawn <- wjpr_wb_drawn_fun(tibble(level = labs, kind = rows$kind, grouping = rows$grouping, label = rows$label),
                             lv, c(st$col$main, st$col$eu), scale = vmax)
  wjpr_meta(p, base = "WJPr::wjp_bars", complementos = comp, drawn = drawn)
}


## Bloque wjpr severity_bars — figuras 7 ----------------------------------------------------------------------------
## Base (revisión 5, 2026-10-27): WJPr::wjp_bars() vertical de WJPr 1.2.0 con sus argumentos, como fid_severity pero
## desde el df del contrato: las dos series lado a lado por nivel de severidad 0-10 (position "dodge", width 0.75,
## dodge_width 0.8: geom_col dodge 0.8 width 0.75 de F03.R:28-45), expand_discrete = media barra + medio
## desplazamiento + 5 % del rango (el eje continuo del original), la línea de corte en 3.5 (entre 3 y 4) con ref_lines
## (discontinua, del color del texto, 0.4 mm × k), limits = c(0, NA) + expand_mult c(0, 0.08) + breaks automáticos
## (el eje y del original), ylab = NULL, la leyenda arriba y centrada (legend_* con texto regular de 13 × k pt y clave
## de 1.2 líneas = 17.28 × k pt), grid_color y ptheme = WJP_theme(base_size = 13 × k, rejilla "y" #eeeeee continua) +
## theme() con el resto del tema de F03 (theme_minimal base 11 con textos a 13) en pt finales: k = 10 / base_pt del
## manifiesto (13 = textos del original sobre el lienzo × 1.3; sin base_pt, 13). Sin wjp_layout: llena W × H.
## Complementos: ninguno (bare / complementos = FALSE devuelve la misma llamada).
## Lo dibujado por fila (revisión 4, G1; contrato drawn de wjpr_meta): wjpr_sev_drawn, una fila por barra con val =
## alto, label_drawn = etiqueta del eje y grouping_drawn = texto de la leyenda para su relleno. La línea de corte
## (diseño fijo, sin fila en df) se comprueba con wjpr_meta(…, marcas = wjpr_sev_marcas_fun()): sin ella, figures
## «dibujo · marca fija ausente: línea de corte entre 3 y 4».

WJPR_SEV_CUT <- 3.5   # umbral de no trivial (F03.R:28-45): la línea de corte va entre las severidades 3 y 4

# Lo dibujado por fila de la severidad (contrato drawn de wjpr_meta, revisión 4), leído de b = ggplot_build() del
# gráfico final: una fila por barra, de izquierda a derecha (nivel del eje, wjpr_pos_slots, y dentro de él la x de la
# barra esquivada). kind "bar", label = el nivel de severidad que ocupa esa posición del eje y grouping = la serie que
# ocupa ese lugar del par (series[1] a la izquierda: el país; series[2] la UE, el orden del dodge); val = el alto de la
# barra (en %, las unidades de df$value; una barra sin alto finito, valor NA que wjp_bars no dibuja, o de alto 0 no
# codifica nada: NA); label_drawn = la etiqueta impresa en el eje en esa posición; grouping_drawn = el texto de la
# leyenda impresa para el relleno de la barra ("" si la leyenda no lo nombra); text = los textos impresos en la
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
    if (!all(c("x", "fill") %in% names(d)) || !nrow(d)) return(NULL)
    tibble(x = num(d, "x"), slot = wjpr_pos_slot_of(d$x, slots), len = abs(num(d, "ymax") - num(d, "ymin")),
           fill = as.character(d$fill))
  }))
  # corregido (revisión 5): una barra de valor NA (wjp_bars 1.2.0 la deja en la capa sin alto y no la dibuja) sigue
  # ocupando su lugar del par (val NA); antes se descartaba y la otra serie de ese nivel pasaba a ser la primera
  bars <- bars[!is.na(bars$slot), , drop = FALSE]
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
         val = ifelse(!is.finite(bars$len) | bars$len <= 1e-9, NA_real_, bars$len), val2 = NA_real_)
}
wjpr_sev_drawn_fun <- function(series) {
  force(series)
  function(b) wjpr_sev_drawn(b, series)
}

# corregido (revisión 4): la línea de corte, marca fija del diseño sin fila en df, como marca de wjpr_meta(…, marcas =)
# (figures: estado "dibujo" si falta); antes su ausencia solo iba al log y la figura seguía ok (parecido_local 0.118,
# igual que sin error). cut = c(nivel, nivel) entre los que va: character() si hay una geom_vline visible (color
# distinto del fondo, ancho > 0, tipo de línea no blank) en la posición media de los dos niveles del eje; si no, su
# descripción. Desde la revisión 5 la línea es la de ref_lines de wjp_bars (una geom_vline por fila de ref_lines)
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

# bare (= complementos FALSE) se conserva por compatibilidad, sin efecto: no hay complementos
wjpr_severity_bars <- function(df, spec, bare = !wjpr_complementos(), ...) {
  wjpr_require(); wjpr_wb_need()
  # k sale de base_pt del manifiesto (13 = textos del original sobre el lienzo × 1.3), como en los adaptadores de 3, 6
  # y 29. Sin base_pt, 13 (lo publicado)
  st <- STD; k <- 10 / (if (is.null(spec$base_pt) || is.na(spec$base_pt)) 13 else spec$base_pt)
  lv <- c(spec$country, "EU Average")
  d <- df %>% filter(kind == "bar") %>%
    mutate(serie = factor(ifelse(grouping == lv[2], lv[2], lv[1]), levels = lv), sev = as.numeric(label),
           value = as.numeric(value), label = factor(label, levels = unique(label[order(sev)])))
  ext <- 0.8 / 4 + 0.75 / 4                          # media barra más medio desplazamiento: borde de las barras
  fs <- 13 * k                                       # textos de F03 (13 sobre el lienzo × 1.3)
  p <- WJPr::wjp_bars(
    d, target = "value", grouping = "label", colors = "serie", order = "sev",
    cvec = stats::setNames(c(st$col$main, st$col$eu), lv), direction = "vertical",
    position = "dodge", width = 0.75, dodge_width = 0.8,
    expand_discrete = ext + 0.05 * (max(d$sev) - min(d$sev) + 2 * ext),
    ref_lines = data.frame(axis = "category", position = WJPR_SEV_CUT - min(d$sev) + 1, colour = st$col$text,
                           linetype = "dashed", linewidth = 0.4 * k * ggplot2::.pt * 0.75),
    limits = c(0, NA), expand_mult = c(0, 0.08), breaks = ggplot2::waiver(), ylab = NULL,
    grid_color = "#eeeeee",
    show_legend = TRUE, legend_position = "top", legend_justification = "center", legend_text_size = fs,
    legend_key_size = 17.28 * k, legend_fontface = "plain",
    ptheme = WJPr::WJP_theme(base_size = fs, text_colour = st$col$text, grid = "y", grid_colour = "#eeeeee",
                             grid_linetype = "solid") +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_line(colour = "#eeeeee", linewidth = 0.5 * k, linetype = "solid"),
        axis.text.x = ggplot2::element_text(vjust = 1, margin = ggplot2::margin(t = 4.95 * k)),
        axis.text.y = ggplot2::element_text(hjust = 1, margin = ggplot2::margin(r = 4.95 * k)),
        legend.text = ggplot2::element_text(colour = st$col$text, margin = ggplot2::margin(l = 0.5 * fs)),
        legend.key.spacing.x = grid::unit(5.5 * k, "pt"),
        legend.margin = ggplot2::margin(5.5 * k, 5.5 * k, 5.5 * k, 5.5 * k), legend.box.margin = ggplot2::margin(0, 0, 0, 0),
        legend.box.spacing = grid::unit(11 * k, "pt"),
        plot.margin = ggplot2::margin(5.5 * k, 5.5 * k, 5.5 * k, 5.5 * k)))
  wjpr_meta(p, base = "WJPr::wjp_bars", drawn = wjpr_sev_drawn_fun(lv),
            marcas = wjpr_sev_marcas_fun(as.character(c(floor(WJPR_SEV_CUT), ceiling(WJPR_SEV_CUT)))))
}
## Prueba (revisión 5, 2026-10-27/28; los cuatro bloques de wjp_bars, WJPr 1.2.0@1664a23; SCRATCH/fix5/M2/pruebas):
## cd /tmp && Rscript tools/wjpr_gallery.R --country hungary (las 42, con y sin --sin-complementos) → 2, 7, 13, 14, 18,
## 19, 30, 36 y 37 ok con 0 complementos, textos iguales, 0 dudosos, f1 1.000 (la 7, 0.999) y local 0.000 (la 7, 0.118:
## los guiones de la línea de corte, como antes); por fila 12, 22, 8, 8, 7, 5 y 10 filas ok (2, 7, 13, 14, 18, 19, 36;
## hbars lo gana en esta revisión); 30 y 37 desde la referencia «≠ (val 36→100)» porque wjpr_ref_df les da value =
## frac·100 (con su escala propuesta, pendiente de la infraestructura: 4 y 3 filas ok); los SVG de las nueve son
## idénticos (md5) con y sin complementos; tamaño definido (svglite) = w_in × h_in en 2, 13, 14, 18, 19 y 36 (sin
## wjp_layout, 7, 30 y 37 llenan el lienzo); el resto de las 42, igual que antes (10 y 15 en error: bloque dumbbell).
## main.R --country hungary --stage all --figs 02,07 --strict → 2/2 verificadas, exit 0; Polonia y Malta (02, 07) ok
## en figures. Inyecciones (pruebas/iny_m2.R; las de iny_g1.R portadas a las capas de wjp_bars 1.2.0): 02 pb_textos,
## pb_oculto, pb_barra, pb_series, pb_series_todo, pb_etiquetas, pb_leyenda y 07 sev_barra, sev_leyenda, sev_ejes,
## sev_corte → figures texto o dibujo, verify fallo, exit 1 las once; 13 (hb_*) y 30/37 (col_*) sin data(), con
## save_figure + drawn_ref_rows (pruebas/iny_m2_sim.R): detectadas todas salvo col_series en la 37 (sus dos columnas
## de la categoría 1 miden 99 % y 99 %: el cambio queda dentro de la tolerancia). Con WJPr 1.1.0 (SRC_REPORTS_LIB) los
## adaptadores se detienen: «WJPr 1.1.0@382aee9: wjp_bars no tiene position, layout, …». Valores NA (pruebas/p04_na.R):
## figures ok, val NA en su fila y la otra serie en su sitio.

## Bloque wjpr funnel — figuras 8, 34 -------------------------------------------------------------------------------
## Base (WJPr 1.2.0): WJPr::wjp_funnel() con sus argumentos documentados, como fid_funnel (dev/report-fidelity-new.R:6-13
## del paquete) pero desde el df del contrato: una fila por etapa (kind stage) con value crudo (la barra más ancha =
## max(value) llena la zona de barras y las demás se centran a value / max), text impreso tal cual (labels) y el orden
## de df$order; colores de STD$col (main, eu para los conectores, text) y medidas de ST_FUNNEL / chart_funnel: barra 40
## pt, hueco 18, 4 pt arriba y abajo, valor 11 pt semibold cursiva blanco, etiquetas 10 pt a la derecha con su salto de
## línea (o ajustadas a funnel_label_w = 190 pt) e interlineado 1.22, a 16 pt de las barras. Columna de etiquetas como
## chart_funnel: su borde derecho en L = ceiling(ancho máximo) + 2 (text_width, 04_svg.R), así que el margen izquierdo
## de wjp_funnel es L − ancho máximo; width = w_in · 72: el gráfico tiene el tamaño del manifiesto (3 etapas: 6.5 ×
## 2.278 in). complemento: ninguno (complementos = FALSE devuelve la misma llamada).
## Lo dibujado por fila (nuevo con 1.2.0; wjpr_funnel_drawn): una fila por etapa con frac = ancho de su barra / 100 (el
## frac de la referencia, value / max), val = frac · max(df$value), text = el valor impreso dentro de la barra y
## label_drawn = las líneas de etiqueta impresas a su altura (una barra de ancho 0, la de un valor NA: frac y val NA).

# Centro de cada etapa en el eje y de wjp_funnel (pt desde arriba; la escala es inversa): la regla de wjp_funnel
# (funnelChart.R:207-208 de WJPr 1.2.0): etapa i desde (i − 1)·(bar + gap), centro a bar / 2. Es la identidad de cada
# posición en lo dibujado por fila; barras, valores y etiquetas se leen del dibujo. origen: funnelChart.R:207-208
wjpr_funnel_mid <- function(n, bar, gap) (seq_len(n) - 1) * (bar + gap) + bar / 2

# Lo dibujado por fila del embudo (contrato drawn de wjpr_meta), de b = ggplot_build(): stages = tibble(order, kind,
# grouping, label, mid) en el orden del df; cada barra (GeomRect), valor (GeomText) y línea de etiqueta (GeomLabel de
# wjp_funnel, sin caja) cae en la etapa cuyo centro está a menos de media separación entre etapas. frac = (xmax − xmin)
# / 100 de su barra, val = frac · vmax (la barra más ancha = vmax = max(df$value)), text = lo impreso dentro de la
# barra, label_drawn = las líneas de etiqueta de arriba abajo unidas con "\n" (se comparan sin espacios de más).
wjpr_funnel_drawn <- function(b, stages, vmax) {
  half <- if (nrow(stages) > 1) min(diff(sort(stages$mid))) / 2 else Inf
  stage_of <- function(y) vapply(-suppressWarnings(as.numeric(y)), function(v) {     # escala inversa: y construida = −y
    if (!is.finite(v)) return(NA_integer_)
    d <- abs(stages$mid - v); j <- which.min(d); if (d[j] < half) j else NA_integer_
  }, NA_integer_)
  ir <- unname(which(vapply(b$plot$layers, function(l) inherits(l$geom, "GeomRect"), logical(1))))
  bars <- bind_rows(tibble(stage = integer(), w = numeric()), lapply(ir, function(i) {
    d <- as.data.frame(b$data[[i]])
    tibble(stage = stage_of((d$ymin + d$ymax) / 2), w = abs(d$xmax - d$xmin))
  }))
  bars <- bars[!is.na(bars$stage), , drop = FALSE]
  tx <- wjpr_drawn_texts(b)
  tx <- tx[tx$source == "layer" & !is.na(tx$label), , drop = FALSE]
  tx$stage <- stage_of(tx$y)
  is_lab <- vapply(b$plot$layers, function(l) inherits(l$geom, "GeomLabel"), logical(1))
  lab <- tx[is_lab[tx$layer] & !is.na(tx$stage), , drop = FALSE]; lab <- lab[order(-lab$y), , drop = FALSE]
  val <- tx[!is_lab[tx$layer] & !is.na(tx$stage), , drop = FALSE]
  idx <- seq_len(nrow(stages))
  w <- vapply(idx, function(k) { v <- bars$w[bars$stage == k]; if (length(v)) v[1] else NA_real_ }, 0)
  w[w <= 0] <- NA_real_                # wjp_funnel dibuja un valor suprimido (NA) como barra de ancho 0: sin barra
  tibble(order = idx, kind = stages$kind, grouping = stages$grouping, label = stages$label, frac = w / 100,
         frac2 = NA_real_, text = vapply(idx, function(k) paste(val$label[val$stage == k], collapse = " "), ""),
         text2 = "",
         label_drawn = vapply(idx, function(k) paste(lab$label[lab$stage == k], collapse = "\n"), ""),
         grouping_drawn = NA_character_, val = w / 100 * vmax, val2 = NA_real_)
}

wjpr_funnel <- function(df, spec, st = ST_FUNNEL, complementos = wjpr_complementos(), ...) {
  wjpr_require()
  if (!wjpr_has_fun("wjp_funnel")) stop("wjpr_funnel requiere WJPr >= 1.2.0 (wjp_funnel)", call. = FALSE)
  s <- df %>% filter(kind == "stage") %>% arrange(order)
  # columna de etiquetas de chart_funnel: el salto de línea que trae la etapa o el ajuste a funnel_label_w; borde
  # derecho en L = ceiling(ancho máximo) + 2 (wjp_funnel la mide igual: margen izquierdo = L − ancho máximo)
  lines <- lapply(s$label, function(l) {
    if (grepl("\n", l)) strsplit(l, "\n", fixed = TRUE)[[1]] else wrap_text(l, st$size_label, st$funnel_label_w)
  })
  wmax <- max(vapply(unlist(lines), text_width, 0, size = st$size_label))
  L <- ceiling(wmax) + 2
  p <- WJPr::wjp_funnel(s, target = "value", grouping = "label", labels = "text", order = "order",
                        bar_height = st$funnel_bar, gap = st$funnel_gap, connectors = TRUE,
                        connector_color = st$col$eu, connector_alpha = 0.45, color = st$col$main,
                        value_face = "semibold.italic", value_size = st$funnel_value, value_color = "#FFFFFF",
                        label_size = st$size_label, label_colour = st$col$text, label_width = st$funnel_label_w,
                        label_gap = 16, label_lineheight = 1.22, width = spec$w_in * 72, margins = c(4, 4, 4, L - wmax))
  stages <- tibble(order = s$order, kind = s$kind, grouping = coalesce(s$grouping, ""), label = s$label,
                   mid = wjpr_funnel_mid(nrow(s), st$funnel_bar, st$funnel_gap))
  vmax <- suppressWarnings(max(as.numeric(s$value), na.rm = TRUE))
  wjpr_meta(p, base = "WJPr::wjp_funnel", complementos = character(),
            drawn = local({ force(stages); force(vmax); function(b) wjpr_funnel_drawn(b, stages, vmax) }))
}
## Prueba (WJPr 1.2.0@1664a23, 2026-10-27, SCRATCH/fix5/M3; 8 y 34 sin data(): desde la referencia): galería --figs 8,34
## → ok, textos iguales, 0 dudosos, f1 1.000, local 0.000, «3 filas ok», 0 complementos (1.1.0@382aee9 con el adaptador
## anterior sobre wjp_bars: 8 y 8, f1 1.000, local 0); igual con --sin-complementos. Inyecciones en el dibujo
## (pruebas/t_iny_ref.R, reglas de figures y verify): valores de las etapas 1 ↔ 2 (texto), 1.ª línea de etiqueta de la
## etapa 2 ↔ la de la 3 (texto · label_drawn), barra de la etapa 2 +5 % del ancho (dibujo · val 31.93→35.01, verify
## frac), valor de la etapa 3 del color de la barra (texto · faltan 1): detectadas las cuatro en las dos figuras.


## Bloque wjpr cards — figuras 32, 41, 42 ----------------------------------------------------------------------------
## Base (WJPr 1.2.0): WJPr::wjp_cards() con sus argumentos documentados, como fid_cards (dev/report-fidelity-new.R:15-25
## del paquete) pero desde el df del contrato: una fila por tarjeta (kind card), filas = df$grouping y columnas =
## df$label en el orden de df$order (row_order / col_order), target = value crudo (el color), labels = df$text y deltas
## = df$text2 tal cual; ref = la dirección de las tarjetas de referencia que trae df (text2 == ""): "col" si lo es la
## primera columna de cada fila (la 32: Fully resolved), "row" si lo es la primera fila de cada columna (41, 42: Without
## a non-trivial problem); limits = c(df$lo, df$hi) (la escala de color de la figura; en 41 y 42 la común de las dos,
## 29-55 en la publicada: la tarjeta 30 % de la 42 tiene t = 0.1535 = 0.12 + 0.88·(30 − 29)/26, medido en la PNG); sin
## lo/hi, el rango de los valores redondeados (lo que hace wjp_cards). Colores de STD$col (card_low → main, text, delta)
## y medidas de ST_CARDS / chart_scorecards: tarjetas de 48 pt cada 54, esquinas de 3 pt, columna de etiquetas de 170 pt
## a 12 pt de las tarjetas, valor 16 pt bold (blanco sobre t > 0.55), delta 9 pt cursiva, etiquetas 11 pt (interlineado
## 1.22) y encabezados 11 pt bold cursiva main (paso 1.2 em, a 7 pt de las tarjetas); width = w_in · 72: el gráfico tiene
## el tamaño del manifiesto (32: 6.5 × 4.072; 41: × 3.506; 42: × 2.756).
## complemento: encabezados en Bold Italic 700 (wjpr_cards_family): ver la función.
## Lo dibujado por fila (nuevo con 1.2.0; wjpr_cards_drawn): una fila por tarjeta (orden visual por filas) con text /
## text2 = el valor y el delta impresos en ella, label_drawn = el encabezado de su columna, grouping_drawn = la etiqueta
## de su fila y val = el valor que codifica su color (relleno → t de la mezcla card_low → main → lo + (t − 0.12) /
## 0.88 · (hi − lo); NA con el relleno de un valor NA, t = 0); frac NA (la referencia no trae barra).

# Familias que comparten otros bloques (matrix, scatter, upset, network, sankey): se conservan aquí
# Las capas que reemplazan una escala ya presente se añaden con suppressMessages() (ggplot2 avisa "Scale for … is
# already present"; el reemplazo es intencional)

# Familia con la cara Bold Italic (700) de Inter Tight: la familia "inter" de setup_fonts() registra bold.italic como
# SemiBoldItalic (600, la de las figuras ggplot publicadas), pero los encabezados de las figuras svg_r (tarjetas,
# sankey) son 700 cursiva. svglite escribe el peso real del archivo (font-weight: bold) y outline_svg_text() usa
# entonces InterTight-BoldItalic. Se registra una vez (systemfonts, global) y se aplica a las capas que la piden.
wjpr_family_bi700 <- function(name = "inter_bi700") {
  if (!name %in% systemfonts::registry_fonts()$family) {
    f <- font_file(700, TRUE)
    systemfonts::register_font(name = name, plain = f, bold = f, italic = f, bolditalic = f)
  }
  name
}

# complemento: wjp_cards dibuja los encabezados con header_fontface en la familia activa (wjp_font_family()), y la
# familia "inter" del proyecto (setup_fonts, 00_setup.R) registra bold.italic como SemiBoldItalic (600): con ella los
# encabezados salen 600 y los del diseño son 700 cursiva. Se llama a wjp_cards con options(wjpr.family) = una familia
# Inter Tight completa (Regular, Bold, Italic y BoldItalic 700: sus otros textos, plain / bold / italic, salen igual que
# con "inter"); WJPr la usa también para medir y partir los encabezados. Sobraría si setup_fonts registrara las caras de
# peso como las pide wjp_face() ("inter SemiBold" para "semibold.italic") y "inter" con BoldItalic 700 (pendiente de
# infraestructura).
wjpr_cards_family <- function(name = "inter_cards") {
  if (!name %in% systemfonts::registry_fonts()$family) {
    systemfonts::register_font(name = name, plain = font_file(400, FALSE), bold = font_file(700, FALSE),
                               italic = font_file(400, TRUE), bolditalic = font_file(700, TRUE))
  }
  name
}

# Dirección de las tarjetas de referencia (text2 == "") en la rejilla de df: "col" si es la primera columna de cada
# fila, "row" si es la primera fila de cada columna, "none" si ninguna tarjeta trae delta
wjpr_cards_ref <- function(d, rows, cols) {
  is_ref <- !nzchar(coalesce(d$text2, ""))
  if (all(is_ref)) return("none")
  first_col <- is_ref[d$label == cols[1]]; first_row <- is_ref[d$grouping == rows[1]]
  if (all(first_col) && !any(is_ref[d$label != cols[1]])) return("col")
  if (all(first_row) && !any(is_ref[d$grouping != rows[1]])) return("row")
  stop("wjpr_cards: las tarjetas sin delta (text2 \"\") no son la primera columna ni la primera fila de la rejilla",
       call. = FALSE)
}

# Lo dibujado por fila de las tarjetas (contrato drawn de wjpr_meta), de b = ggplot_build(): grid = tibble(order, kind,
# grouping, label, i, j) con la celda de cada tarjeta del df; geo = list(cw, h, gap, lo, hi, min_shade, low, high).
# Cada polígono (una tarjeta) cae en la celda que contiene su centro; cada texto en la celda que lo contiene (valor y
# delta: de las capas de texto con algo dentro de una tarjeta, la primera es la de valores y la segunda la de deltas),
# en la columna sobre la rejilla (encabezados: y < 0 en datos) o en la fila a su izquierda (etiquetas: x < 0). val =
# lo + (t − min_shade) / (1 − min_shade) · (hi − lo), con t = el de la mezcla low → high más cercana al relleno
# (mínimos cuadrados en RGB).
wjpr_cards_drawn <- function(b, grid, geo) {
  nr <- max(grid$i); nc <- max(grid$j)
  cell_of <- function(x, y) {                         # y de datos hacia abajo (la escala es inversa: −y construida)
    j <- floor(x / (geo$cw + geo$gap)) + 1; i <- floor(y / (geo$h + geo$gap)) + 1
    inside <- is.finite(x) & is.finite(y) & j >= 1 & j <= nc & i >= 1 & i <= nr &
      x - (j - 1) * (geo$cw + geo$gap) <= geo$cw & y - (i - 1) * (geo$h + geo$gap) <= geo$h
    as.integer(ifelse(inside, (i - 1) * nc + j, NA_integer_))
  }
  key <- (grid$i - 1) * nc + grid$j
  lo <- grDevices::col2rgb(geo$low)[, 1]; hi <- grDevices::col2rgb(geo$high)[, 1]
  t_of <- function(fill) vapply(as.character(fill), function(f) {
    if (is.na(f)) return(NA_real_)
    v <- grDevices::col2rgb(f)[, 1]; sum((v - lo) * (hi - lo)) / sum((hi - lo)^2)
  }, 0, USE.NAMES = FALSE)
  ip <- unname(which(vapply(b$plot$layers, function(l) inherits(l$geom, "GeomPolygon"), logical(1))))
  cards <- bind_rows(tibble(cell = integer(), t = numeric()), lapply(ip, function(k) {
    d <- as.data.frame(b$data[[k]])
    if (!nrow(d)) return(NULL)
    g <- split(d, d$group)
    tibble(cell = vapply(g, function(s) cell_of(mean(range(s$x)), -mean(range(s$y))), 0L),
           t = t_of(vapply(g, function(s) as.character(s$fill[1]), "")))
  }))
  cards <- cards[!is.na(cards$cell), , drop = FALSE]
  tx <- wjpr_drawn_texts(b)
  tx <- tx[tx$source == "layer" & !is.na(tx$label), , drop = FALSE]
  tx$cell <- cell_of(tx$x, -tx$y)
  in_card <- tx[!is.na(tx$cell), , drop = FALSE]
  lay_card <- sort(unique(in_card$layer))              # 1.ª capa con textos en tarjetas = valores, 2.ª = deltas
  heads <- tx[is.na(tx$cell) & -tx$y < 0, , drop = FALSE]; heads <- heads[order(-heads$y), , drop = FALSE]
  heads$j <- floor(heads$x / (geo$cw + geo$gap)) + 1
  labs <- tx[is.na(tx$cell) & tx$x < 0 & -tx$y >= 0, , drop = FALSE]; labs <- labs[order(-labs$y), , drop = FALSE]
  labs$i <- floor(-labs$y / (geo$h + geo$gap)) + 1
  txt_of <- function(k, layer) {
    if (is.na(layer)) "" else paste(in_card$label[in_card$cell == k & in_card$layer == layer], collapse = " ")
  }
  tt <- vapply(key, function(k) { v <- cards$t[cards$cell == k]; if (length(v)) v[1] else NA_real_ }, 0)
  tt[tt < geo$min_shade / 2] <- NA_real_   # wjp_cards rellena un valor NA con el color bajo (t = 0): sin valor
  out <- tibble(order = as.integer(rank(key, ties.method = "first")), kind = grid$kind, grouping = grid$grouping,
                label = grid$label, frac = NA_real_, frac2 = NA_real_,
                text = vapply(key, txt_of, "", layer = lay_card[1]), text2 = vapply(key, txt_of, "", layer = lay_card[2]),
                label_drawn = vapply(grid$j, function(j) paste(heads$label[heads$j == j], collapse = "\n"), ""),
                grouping_drawn = vapply(grid$i, function(i) paste(labs$label[labs$i == i], collapse = "\n"), ""),
                val = geo$lo + (tt - geo$min_shade) / (1 - geo$min_shade) * geo$span, val2 = NA_real_)
  out[order(out$order), , drop = FALSE]
}

wjpr_cards <- function(df, spec, complementos = wjpr_complementos(), st = ST_CARDS) {
  wjpr_require()
  if (!wjpr_has_fun("wjp_cards")) stop("wjpr_cards requiere WJPr >= 1.2.0 (wjp_cards)", call. = FALSE)
  d <- df[df$kind == "card", , drop = FALSE]; d <- d[order(d$order), , drop = FALSE]
  rows <- unique(d$grouping); cols <- unique(d$label)
  ref <- wjpr_cards_ref(d, rows, cols)
  lim <- if (all(c("lo", "hi") %in% names(d)) && all(is.finite(c(d$lo[1], d$hi[1])))) c(d$lo[1], d$hi[1]) else NULL
  args <- list(d, rows = "grouping", cols = "label", target = "value", labels = "text", deltas = "text2", ref = ref,
               limits = lim, colors = c(st$col$card_low, st$col$main), card_height = st$card_h, card_gap = 6,
               radius = 3, label_width = st$card_label_w, value_size = st$card_value, delta_size = st$card_delta,
               width = spec$w_in * 72, label_gap = 12, label_size = st$card_label, label_colour = st$col$text,
               label_lineheight = 1.22, header_size = st$card_label, header_fontface = "bold.italic",
               header_colour = st$col$main, header_gap = 7, header_lineheight = 1.2, value_fontface = "bold",
               value_color = st$col$text, delta_fontface = "italic", delta_color = st$col$delta, min_shade = 0.12,
               dark_threshold = 0.55, row_order = rows, col_order = cols)
  comp <- character()
  p <- if (isTRUE(complementos)) {
    old <- options(wjpr.family = wjpr_cards_family()); on.exit(options(old), add = TRUE)
    comp <- "encabezados 700 cursiva (familia inter_cards en wjp_cards)"
    do.call(WJPr::wjp_cards, args)
  } else do.call(WJPr::wjp_cards, args)
  # rejilla del diseño, la de wjp_cards (cardsChart.R:282-312 de WJPr 1.2.0): tarjetas de ancho (W − 170 − 12 −
  # 6·(nc − 1)) / nc desde x = 0 e y = 0 (arriba), una cada ancho + 6 y cada 48 + 6
  rv <- round(d$value)
  lo <- if (is.null(lim)) min(rv, na.rm = TRUE) else lim[1]; hi <- if (is.null(lim)) max(rv, na.rm = TRUE) else lim[2]
  geo <- list(cw = (spec$w_in * 72 - st$card_label_w - 12 - 6 * (length(cols) - 1)) / length(cols), h = st$card_h,
              gap = 6, lo = lo, span = if (hi > lo) hi - lo else 1, min_shade = 0.12, low = st$col$card_low,
              high = st$col$main)
  grid <- tibble(order = d$order, kind = d$kind, grouping = d$grouping, label = d$label,
                 i = match(d$grouping, rows), j = match(d$label, cols))
  wjpr_meta(p, base = "WJPr::wjp_cards", complementos = comp,
            drawn = local({ force(grid); force(geo); function(b) wjpr_cards_drawn(b, grid, geo) }))
}
## Prueba (WJPr 1.2.0@1664a23, 2026-10-27, SCRATCH/fix5/M3; 32, 41 y 42 sin data(): desde la referencia): galería
## --figs 32,41,42 → ok, textos iguales, 0 dudosos, f1 1.000, «10 / 12 / 9 filas ok», 1 complemento (familia 700);
## local 0.000 / 0.000 / 0.708 (42: la referencia trae lo = 30 y la publicada usa la escala común 29-55; con lo = 29,
## pruebas/t_42_lo.R → local 0.000 y el 42 % en blanco, como la publicada). 1.1.0@382aee9 con el adaptador anterior
## (ggplot genérico): 7 complementos, f1 1.000, local 0 / 0 / 0.715. --sin-complementos (encabezados 600): f1 1.000, local
## 0 / 0.014 / 0.708. Inyecciones en el dibujo (pruebas/t_iny_ref.R): dos valores intercambiados (texto), dos
## etiquetas de fila intercambiadas (texto · grouping_drawn), encabezados invertidos (texto · label_drawn), relleno de
## una tarjeta con el de otra (dibujo · val, p. ej. 40.18→62 en la 32), un delta del color de su tarjeta (texto ·
## faltan 1): detectadas las cinco en las tres figuras.


## Bloque wjpr matrix — figura 6 -----------------------------------------------------------------------------------
## corregido (revisión 5, 2026-10-27, pedido del usuario: las figuras con WJPr 1.2.0): la base es WJPr::wjp_heatmap()
## con sus argumentos (antes, en 1.1.0 sin mapa de calor, geom_tile + geom_text sobre WJP_theme() y 5 complementos):
## x = df$label (columnas, en el orden de df$order: x_order), y = df$grouping (filas de arriba abajo: y_order), fill =
## la banda (df$text2 como factor con los niveles de bands; "" en la diagonal y en las N/A → na_fill blanco), labels =
## df$text (impreso tal cual: "62%", "N/A", "–"), cvec = cols_band con los nombres de bands (cada banda toma su color
## por NOMBRE, F01.R:26-27, y la leyenda muestra las presentes en ese orden aunque falte alguna), borde blanco
## (border_color, border_width), texto por contraste (label_color = "auto", label_dark = STD$col$text: blanco sobre la
## banda oscura), columnas arriba a 40° (x_position, x_angle), títulos bold cursiva main (x_title, y_title, title_*),
## leyenda arriba centrada (legend_*), márgenes del lienzo original (margins) y la base theme_minimal del diseño
## (ptheme = WJP_theme(base_theme = theme_minimal(11 / k), text_colour)). bands = los labels del cut() de fig_06
## (F01.R:17), que la figura pasa; una banda de text2 que no está en bands detiene la figura. Los tamaños son los de
## run_F01 (src/hungary/F01.R:24-41) en su lienzo k = base_pt / 10 veces mayor (render_figure.R:29-38), divididos por
## k porque figures exporta al tamaño final (la clave de la leyenda, 1.2 líneas de grid = 17.28 pt en el original,
## también); colores de STD$col. Sin cell_size el panel llena el lienzo: el SVG sale a w_in × h_in del manifiesto.
## Complemento (solo cuando hace falta): el texto blanco va por NOMBRE en la banda alta (F01.R:25); wjp_heatmap lo da
## por contraste del relleno, que con cols_band por defecto coincide (blanco solo sobre #575796): con otros colores
## que no coincidan se corrige en los datos del gráfico. compara = "text": text2 (la banda) se dibuja como color y
## leyenda, no como texto de la celda.
## Lo dibujado por celda (revisión 4, contrato extendido; wjpr_matrix_drawn, la misma con la estructura de capas de
## wjp_heatmap: geom_tile con el relleno de la escala manual + geom_text con el color en I()): una fila por posición de
## la rejilla con la celda que la ocupa según los ejes, lo impreso en ella, la banda que dice su color (decodificada
## con cols_band) y las etiquetas de eje impresas en su fila y su columna; igual con complementos y sin ellos.

# corregido (revisión 4): lo dibujado por celda, porque el multiconjunto de textos y el parecido no veían errores que
# conservan los textos (parecido_local con dos textos de celda intercambiados 0.083, con dos etiquetas de fila
# intercambiadas 0.431: ok). b = ggplot_build() del gráfico final; fill_map = color de cada banda en el orden de bands
# (el de la escala del adaptador: cols_band, con complementos y sin ellos). Una fila por posición de la rejilla:
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
  if (!wjpr_has_fun("wjp_heatmap")) {
    stop("wjpr_matrix: la build de WJPr cargada (", wjpr_build(), ") no tiene wjp_heatmap() (WJPr >= 1.2.0)", call. = FALSE)
  }
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
  d <- df %>% mutate(band = factor(if_else(has_band, text2, NA_character_), levels = bands))
  k <- (if (is.null(spec$base_pt) || is.na(spec$base_pt)) 10 else spec$base_pt) / 10   # base_pt vacío llega como NA
  if (is.na(k)) k <- 1
  # unidades de WJPr: textos y líneas en pt, márgenes y claves en bigpts (1/72 in); el diseño, en mm de ggplot
  # (size, linewidth) y pt de grid (margin): size·.pt = pt de texto, linewidth·.pt·0.75 = pt de línea (inversa de
  # wjp_pt_line), pt · 72 / 72.27 = bigpts
  bp <- 72 / 72.27
  mg <- c(10, 45, 10, 10) / k * bp                                  # márgenes del lienzo original (F01.R:39), en bigpts
  th <- WJPr::WJP_theme(base_theme = ggplot2::theme_minimal(base_size = 11 / k), text_colour = STD$col$text)
  comp <- character()
  # complemento: wjp_heatmap fija el margen entre los títulos y las etiquetas de eje en title_size / 4 bigpts (3 / k);
  # el diseño usa el de theme_minimal, 11 / 4 / k pt (half_line / 2): sin compensar, el panel cae 0.24 pt más abajo y a
  # la derecha (parecido_local 0.104, mad 0.0084 frente a 0.000 y 0.0005 de 1.1.0). Se compensa con argumentos: el
  # margen izquierdo (margins) y el espacio entre la leyenda y el título de x (legend.box.spacing de ptheme, que
  # wjp_heatmap no cambia) se reducen en la diferencia, así el panel y la leyenda quedan donde en lo publicado y solo
  # los títulos se mueven 0.24 pt. Propuesta para WJPr: title_margin (pt) en wjp_heatmap
  dm <- 12 / k / 4 - 11 / 4 / k * bp
  if (isTRUE(complementos) && dm > 0) {
    mg[4] <- mg[4] - dm
    th <- th + ggplot2::theme(legend.box.spacing = grid::unit(2 * 11 / 2 / k - dm / bp, "pt"))
    comp <- c(comp, "margen de los títulos del diseño (compensado en margins y legend.box.spacing)")
  }
  # corregido (revisión 5): la función de WJPr del tipo con sus argumentos, en lugar de geom_tile + geom_text y los
  # complementos de 1.1.0 (escala ordinal, borde, texto blanco, tema base, tema del diseño, fuente). origen: F01.R:24-41
  # (clave de la leyenda: 1.2 líneas de grid = 17.28 pt del original entre k, medida en el SVG: 13.65 como 1.1.0)
  p <- WJPr::wjp_heatmap(
    d, x = "label", y = "grouping", fill = "band", labels = "text", x_order = cols, y_order = rows,
    cvec = stats::setNames(cols_band, bands), na_fill = "#FFFFFF", border_color = "#FFFFFF",
    border_width = 0.8 / k * ggplot2::.pt * 0.75,
    label_color = "auto", label_dark = STD$col$text, label_size = 3.6 / k * ggplot2::.pt, label_fontface = "plain",
    x_position = "top", x_angle = 40, axis_text_size = 11 / k, axis_text_colour = STD$col$text,
    x_title = x_title, y_title = y_title, title_size = 12 / k, title_fontface = "bold.italic", title_colour = STD$col$main,
    legend_position = "top", legend_text_size = 11 / k, legend_key_size = 1.2 * 12 * 1.2 / k,
    legend_fontface = "plain", legend_justification = "center", margins = mg, ptheme = th)
  base <- "WJPr::wjp_heatmap"
  drawn <- wjpr_matrix_drawn_fun(bands, cols_band)
  # text2 = banda (color y leyenda, no texto de la celda)
  if (!isTRUE(complementos)) return(wjpr_meta(p, base = base, compara = "text", drawn = drawn))

  # complemento: wjp_heatmap da el color del texto por contraste con el relleno (label_color = "auto") o uno fijo; el
  # diseño lo da por nombre de banda: blanco en la última de bands y STD$col$text en el resto (F01.R:25). Con
  # cols_band por defecto coinciden y no se aplica; con otros colores que no coincidan, se corrige el color del texto
  # en los datos del gráfico (la capa geom_text lo lee con I()). Propuesta para WJPr: label_colors = c(<valor de
  # fill> = <color>) (color del texto por valor de fill, por encima de "auto")
  want <- if_else(as.character(p$data$fill_value) %in% tail(bands, 1), "#FFFFFF", STD$col$text)
  if (!all(wjpr_same_color(p$data$label_colour, want) | (is.na(p$data$label_colour) & is.na(want)))) {
    p$data$label_colour <- want
    comp <- c(comp, "texto blanco en la banda alta por nombre (no por contraste)")
  }
  wjpr_meta(p, base = base, complementos = comp, compara = "text", drawn = drawn)
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
## Prueba del bloque (revisión 5, 2026-10-27, M4; WJPr 1.2.0@1664a23; SCRATCH/fix5/M4/pruebas/): galería --figs 6 →
## ok, textos iguales, 144 filas ok, f1 1.0000, local 0.000, mad 0.0010 (antes, 1.1.0 y 1.2.0 con los 5 complementos
## de 1.1.0: f1 1.0000, local 0.000, mad 0.0005; SVG idénticos entre builds); 1 complemento (margen de los títulos);
## sin él (--sin-complementos) local 0.104 y mad 0.0084: el panel 0.24 pt abajo y a la derecha; con él panel
## (101.65, 117.60, 462.40 × 417.66) y claves (13.65) donde en 1.1.0 y solo los títulos movidos 0.24 pt. main.R
## --stage all --figs 03,05,06 --strict: ok, 707/707, exit 0. Inyecciones portadas a wjp_heatmap (pruebas/
## iny_g2_m4.R, iny_run_m4.sh), cada una --strict → exit 1, fallo en verify: mx_textos (texto · text Consumer:
## 62%→64%), mx_filas (texto · grouping_drawn, 24 filas), mx_traspuesta (texto, 236), mx_banda (texto · text2 50% or
## more→25% to 50%). Poland y Malta (config de bancos/, --stage all --figs 03,05,06): ok, textos iguales, 144 filas;
## frente al adaptador de 1.1.0 en esa misma copia, solo los títulos de eje (513 píxeles a 96 dpi, mad 0.0006).

## Bloque wjpr scatter — figura 3 ----------------------------------------------------------------------------------
## corregido (revisión 5, 2026-10-27, pedido del usuario: las figuras con WJPr 1.2.0): la base es WJPr::wjp_scatter()
## con sus argumentos (antes, en 1.1.0 sin dispersión x-y, geom_smooth + geom_point + ggrepel sobre WJP_theme() y 6
## complementos): x = df$value (prevalencia %), y = df$value2 (severidad 0-10; NA = sin punto: wjp_scatter no dibuja
## los puntos sin x o y, como F02.R:38 na.rm), facet = df$grouping en el orden de df$order (facet_nrow 1), labels =
## df$label, highlight = las categorías con df$series == "destacada" (data() las toma de config/<pais>.yml, figures:
## fig03: highlight; las de lo publicado son el top_cats de F02.R:33, una elección editorial; el argumento highlight
## del adaptador solo vale para un df sin series), hollow = df$text2 == WJPR_SCATTER_HUECO (el «n < 30» que data()
## pone a los puntos dibujables con n < 30), colors = main y el gris de WJPr (#c4c4c4 = F02.R:41), trend = "lm"
## (discontinua #9aa0a6, la de WJPr = F02.R:37), rótulos repelidos con seed 7 y los rellenos de ggrepel del diseño
## (repel_args), ejes 0-50 % y 4-8 ampliados si un punto cae fuera (xlim, ylim, y_breaks, x_labels), facetas bold
## cursiva main (strip_*), rejilla #eeeeee (grid_color) y la base theme_minimal del diseño con sus márgenes (ptheme =
## WJP_theme(base_size, text_colour, base_theme = theme_minimal(11 / k)) + theme(márgenes)). Tamaños de run_F02
## (src/hungary/F02.R:33-50) en su lienzo k = base_pt / 10 mayor (render_figure.R:29-38), divididos por k; colores de
## STD$col. Sin layout el panel llena el lienzo: el SVG sale a w_in × h_in del manifiesto.
## Un punto suprimido no se dibuja: el log cuenta cuántos faltan por faceta. Complemento (solo cuando hace falta): una
## faceta con menos de min_points puntos dibujables se quita del dibujo con aviso (wjp_scatter la dibujaría con un
## punto y sin tendencia); sin ninguna que llegue, error. El contrato del scatter no imprime valores (text = ""): los
## rótulos de categoría (destacadas con punto dibujable) van en rotulos y entran en la comparación de textos.
## Lo dibujado por punto (revisión 4, contrato extendido; wjpr_scatter_drawn, la misma con la estructura de capas de
## wjp_scatter: geom_smooth + geom_point con color y forma en I() + geom_text_repel): una fila por punto dibujado, con
## la categoría que ocupa esa posición, x e y decodificados (val, val2), el rótulo anclado en él (label_drawn), la
## faceta impresa (grouping_drawn) y si es hueco (text2 = WJPR_SCATTER_HUECO); df$text2 no es texto impreso: compara =
## "text". Igual con complementos y sin ellos.

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
                         x_title = "Prevalence", y_title = "Mean seriousness (0-10)", min_points = 2, seed = 7) {
  wjpr_require()
  if (!wjpr_has_fun("wjp_scatter")) {
    stop("wjpr_scatter: la build de WJPr cargada (", wjpr_build(), ") no tiene wjp_scatter() (WJPr >= 1.2.0)", call. = FALSE)
  }
  df <- df[order(df$order), ]
  tag <- sprintf("fig %02d", as.integer(spec$fig %||% 3L))
  # corregido: las destacadas vienen del df (series = "destacada", de config/<pais>.yml) y no de la lista fija de
  # lo publicado en el adaptador, que en otro país rotulaba en silencio las mismas 6 (revisión V2-03)
  dest <- if ("series" %in% names(df)) coalesce(df$series == "destacada", FALSE) else df$label %in% highlight
  # corregido (revisión 5): hueco = la marca que pone data() (text2 = WJPR_SCATTER_HUECO, punto dibujable con n < 30) y
  # no n < 30 recalculado aquí: plot() no recalcula (contrato §4.4) y es la marca que compara lo dibujado por punto
  d <- df %>% mutate(facet = factor(grouping, levels = unique(grouping)), destacada = dest,
                     hueco = coalesce(text2 == WJPR_SCATTER_HUECO, FALSE))
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
  comp <- character()
  if (length(fuera) > 0 && isTRUE(complementos)) {
    # complemento: wjp_scatter dibuja toda faceta con algún punto dibujable (con uno, sin tendencia: stat_smooth no
    # ajusta); el diseño no dibuja una faceta con menos de min_points (revisión V4-03: Malta y Bulgaria, Rural con 0-1
    # puntos). Propuesta para WJPr: min_points = 2 (las facetas con menos puntos dibujables se omiten con aviso)
    log_msg(tag, " aviso: faceta ", paste(fuera, collapse = ", "), " sin dibujar: menos de ", min_points,
            " puntos con prevalencia y severidad (", cuenta, ")")
    d <- d %>% filter(!facet %in% fuera) %>% mutate(facet = droplevels(facet))
    comp <- c(comp, paste0("facetas con menos de ", min_points, " puntos fuera del dibujo"))
  } else if (length(fuera) > 0) {
    log_msg(tag, " aviso: faceta ", paste(fuera, collapse = ", "), " con menos de ", min_points,
            " puntos (sin complementos se dibuja: ", cuenta, ")")
  }
  fuera_d <- if (isTRUE(complementos)) fuera else character()
  # corregido (revisión 4): lo dibujado por punto (wjpr_scatter_drawn), con complementos y sin ellos; las claves son
  # las de df entero (también las de una faceta que no se dibuje: quedan como filas sin punto). text2_sin_punto: en una
  # faceta que el diseño no dibuja (fuera, con aviso en el log) la marca de hueco de df no se compara (sin punto no
  # hay hueco que ver); en las demás, una clave sin punto con la marca es un punto esperado que falta
  drawn <- wjpr_scatter_drawn_fun(tibble(order = df$order, kind = df$kind, grouping = df$grouping, label = df$label,
                                         value = df$value, value2 = df$value2, destacada = dest,
                                         text2_sin_punto = if_else(df$grouping %in% fuera_d, df$text2, "")))
  # corregido (V3-07): las categorías rotuladas entran en la comparación de textos (attr(p, "wjpr")$rotulos); antes
  # figures y verify no las miraban y la 3 quedaba «sin textos que comparar»
  rot <- as.character(d$label[d$destacada & is.finite(d$value) & is.finite(d$value2)])
  k <- (if (is.null(spec$base_pt) || is.na(spec$base_pt)) 10 else spec$base_pt) / 10   # base_pt vacío llega como NA
  if (is.na(k)) k <- 1
  # unidades de WJPr: textos y líneas en pt; el diseño, en mm de ggplot (size, linewidth, stroke): size·.pt = pt de
  # texto, linewidth·.pt·0.75 = pt de línea (inversa de wjp_pt_line); el punto de wjp_scatter es su diámetro con el
  # contorno (point_size = size·.pt·0.75 + point_stroke, point_stroke = stroke·0.75·.stroke / 2: así size y stroke
  # de ggplot quedan los del diseño, 3.4 / k y 1.1 / k)
  stroke <- 1.1 / k * 0.75 * ggplot2::.stroke / 2
  # ejes del diseño (0-50 % con sufijo, 4-8 cada 1); si un punto cae fuera (otro país: p. ej. severidad 8.10) el eje
  # se amplía en vez de descartarlo en silencio
  xl <- c(0, max(50, ceiling(max(d$value, na.rm = TRUE) / 10) * 10))
  yl <- c(min(4, floor(min(d$value2, na.rm = TRUE))), max(8, ceiling(max(d$value2, na.rm = TRUE))))
  gris <- eval(formals(WJPr::wjp_scatter)$colors)[2]                 # no destacados: el gris de WJPr (#c4c4c4, F02.R:41)
  # corregido (revisión 5): la función de WJPr del tipo con sus argumentos, en lugar de geom_smooth + geom_point +
  # ggrepel y los complementos de 1.1.0 (tendencia, destacadas y huecos, rótulos, escalas, tema, fuente). Los rellenos
  # de ggrepel van divididos por k como en la revisión V3-06 (box.padding, min.segment.length, point.size,
  # segment.size). origen: F02.R:33-50
  p <- WJPr::wjp_scatter(
    d, x = "value", y = "value2", labels = "label", facet = "facet",
    highlight = unique(as.character(d$label[d$destacada])), hollow = "hueco",
    colors = c(STD$col$main, gris), point_size = 3.4 / k * ggplot2::.pt * 0.75 + stroke, point_stroke = stroke,
    trend = "lm", trend_width = 0.5 / k * ggplot2::.pt * 0.75, repel = TRUE, seed = seed,
    repel_args = list(box.padding = 0.25 / k, min.segment.length = 0.3 / k, point.size = 1 / k, segment.size = 0.5 / k),
    label_size = 4 / k * ggplot2::.pt, label_fontface = "plain", label_colour = STD$col$main,
    xlim = xl, ylim = yl, y_breaks = seq(yl[1], yl[2]), x_labels = function(x) paste0(x, "%"),
    x_title = x_title, y_title = y_title, facet_nrow = 1,
    strip_size = 14 / k, strip_fontface = "bold.italic", strip_colour = STD$col$main,
    ptheme = WJPr::WJP_theme(base_size = 12 / k, text_colour = STD$col$text,
                             base_theme = ggplot2::theme_minimal(base_size = 11 / k)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 11 / 4 / k)),
                     axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 11 / 4 / k)),
                     plot.margin = ggplot2::margin(11 / 2 / k, 11 / 2 / k, 11 / 2 / k, 11 / 2 / k)))
  base <- "WJPr::wjp_scatter"
  # corregido (revisión 4): compara = "text" (df$text2 marca el punto hueco, WJPR_SCATTER_HUECO: no se imprime) y
  # lo dibujado por punto, con complementos y sin ellos
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
## Prueba del bloque (revisión 5, 2026-10-27, M4; WJPr 1.2.0@1664a23; SCRATCH/fix5/M4/pruebas/): galería --figs 3 →
## ok, textos iguales, 24 filas ok, f1 1.0000, local 0.000, mad 0.0013 (antes, 1.1.0 y 1.2.0 con los 6 complementos
## de 1.1.0: f1 1.0000, local 0.000, mad 0.0007); 0 complementos en Hungría y --sin-complementos con el SVG idéntico
## (byte a byte); frente al dibujo de 1.1.0, 0 píxeles con |Δ| > 0.2 a 96 dpi. fid_scatter de dev/report-fidelity-
## new.R (su arnés: wjp_save, ragg, Inter Tight de Google Fonts y la referencia): 0.992 tal cual; con repel_args =
## los rellenos de ggrepel del diseño ÷ k (box.padding 0.25, min.segment.length 0.3, point.size 1, segment.size 0.5;
## revisión V3-06), 0.995 (verify 0.9948): lo que falta hasta 1 es el arnés (rasterizado ragg con otra copia de la
## fuente), no la llamada, que en este pipeline da 1.0000 (pruebas/fid_scatter.R). main.R --stage all --figs
## 03,05,06 --strict: ok, 157/157. Inyecciones portadas a wjp_scatter (pruebas/iny_g2_m4.R), cada una --strict → exit
## 1: sc_x3 (dibujo · val Accidental 23.4324→26.4324), sc_hueco (texto · text2 Consumer: →n < 30), sc_rotulos y
## sc_rotulo_otro (texto · label_drawn Community→Consumer, Accidental→Community). Poland (0 complementos) y Malta (1:
## Rural sin puntos dibujables fuera, con aviso): ok, textos iguales, 24 filas; frente al adaptador de 1.1.0, 0
## píxeles con |Δ| > 0.2 en los dos.

## Bloque wjpr upset — figura 29 -----------------------------------------------------------------------------------
## WJPr 1.2.0: WJPr::wjp_upset() (upsetChart.R) dibuja el UpSet como UN ggplot en coordenadas de la figura (pt, y hacia
## abajo; lienzo width × height): barras de intersección (0.9 del paso, alto = value / (máx · bar_headroom) del panel),
## matriz de conjuntos (puntos activos e inactivos, segmento entre el primero y el último activo, rejilla), barras de
## tamaño de conjunto a la izquierda (set_width_ratio del ancho disponible, la mayor a 1 / 1.7), nombres de conjunto,
## valores y título girado del panel de barras. Lo publicado salió de ComplexUpset::upset() (render_figure.R:53-75:
## width_ratio 0.22, expansión 0.25 arriba y 0.7 en los tamaños, texto 12.8, valores 3.6 mm en vertical) sobre un
## lienzo k = base_pt / 10 veces mayor (manifiesto: base_pt 10.24, lienzo 6.5 × 5.4 × 1.024), así que cada medida de
## ComplexUpset va dividida por k: márgenes 2 · 5.48 / k (ggplot + patchwork), nombres 0.8 · 12.8 / k a 4.43 / k de la
## matriz, título 11 / k, valores y tamaños 3.6 mm / k, puntos 3.5 mm + trazo 0.5 / k, segmento y rejilla 0.5 / k.
## Colores de STD$col (barras main, puntos activos text, inactivos track); rejilla #EBEBEB (la de ComplexUpset, el
## defecto de wjp_upset). Filas: kind row, label "A + B" (conjuntos de df kind set), de izquierda a derecha en el orden
## de df$order; conjuntos: kind set de arriba abajo en el orden de df$order; text tal cual.
## origen: las medidas del bloque upset anterior (revisión 4: geometría de ComplexUpset en un ggplot propio) y
## dev/report-fidelity-new.R:88-105 (fid_upset de WJPr 1.2.0), con los colores de STD$col en lugar de wjp_colors().
## corregido (1.2.0): antes WJPr no tenía UpSet y el adaptador dibujaba la geometría con capas propias sobre
## WJP_theme() + wjp_palette() más 5 complementos (colores, valores verticales, rejilla, tema de nombres y título,
## fuente); ahora es la llamada a wjp_upset() sola y complementos (TRUE o FALSE) no cambia nada.
## Complementos: ninguno. Lo que wjp_upset no reproduce de lo publicado (y no se suple: parecido local 0.354, dentro de
## verify$max_local): los puntos de ComplexUpset llevan relleno black / grey85 (#000000 / #D9D9D9) con contorno text /
## track (intersection_matrix(outline_color = …)) y wjp_upset los rellena con dot_colors sin contorno (aquí text / track,
## los de STD); y las franjas blancas de ComplexUpset (stripes = "white") tapan la rejilla horizontal y la vertical a la
## altura de cada fila, que wjp_upset dibuja enteras. Propuestas: wjp_upset(dot_outline = c(activo, inactivo)) y
## wjp_upset(stripes = c(…)) (o grid = "x" | "y" | "both"); con relleno #000000 / #D9D9D9 el local baja a 0.278.
## Lo dibujado por fila (wjpr_up_drawn; contrato extendido): una fila por barra de intersección y una por barra de
## conjunto, leídas de la estructura de capas de wjp_upset (antes no se exponía: solo el parecido medía el dibujo).

# Capas de wjp_upset() por su estructura (upsetChart.R:316-384): rejilla (2 GeomSegment), barras de intersección
# (1.er GeomRect), segmentos de la matriz (GeomSegment), puntos (GeomPoint), barras de conjunto (2.º GeomRect) y textos
# (GeomText): nombres de conjunto (label = label, horizontal), valores de las intersecciones (label = text, con las
# columnas de su barra), tamaños de conjunto (label = text, las de la suya) y título (label = label, girado). Cada capa
# de texto se reconoce por sus datos y no por su puesto (las de valores o título faltan si no hay textos)
wjpr_up_layers <- function(layers) {
  is_g <- function(cls) which(vapply(layers, function(l) inherits(l$geom, cls), logical(1)))
  rect <- is_g("GeomRect"); pts <- is_g("GeomPoint"); txt <- is_g("GeomText")
  if (length(rect) != 2 || length(pts) != 1) {
    stop("wjpr_upset: capas de wjp_upset() no reconocidas (", length(rect), " de barras y ", length(pts),
         " de puntos: se esperaban 2 y 1)", call. = FALSE)
  }
  kind <- vapply(txt, function(i) {
    l <- layers[[i]]; lab <- rlang::as_label(l$mapping$label %||% quote(NULL))
    if (identical(lab, "label")) return(if (isTRUE(abs((l$aes_params$angle %||% 0) %% 180) > 1e-8)) "title" else "names")
    if (identical(lab, "text")) return(if (isTRUE(all(l$data$xmin %in% layers[[rect[1]]]$data$xmin))) "val" else "setval")
    ""
  }, "")
  pick <- function(k) { i <- txt[kind == k]; if (length(i)) i[1] else NA_integer_ }
  list(bars = rect[1], sets = rect[2], dots = pts, names = pick("names"), val = pick("val"), setval = pick("setval"),
       title = pick("title"))
}

# Lo dibujado por fila del UpSet (contrato drawn de wjpr_meta, extendido), leído de b = ggplot_build() del gráfico final
# (escala y invertida: y construida = −pt). Conjuntos (kind set): una fila por barra de conjunto, de arriba abajo; la
# fila es el conjunto de df en ese puesto (como el nivel de un eje discreto); label_drawn = el nombre impreso a su altura
# (capa de nombres), text = el tamaño impreso a su altura (capa de tamaños), val = ancho · máx · 1.7 / ancho del panel
# de tamaños (wjp_upset: la mayor mide 1 / 1.7 del panel, que va de spacing al borde derecho común de las barras).
# Intersecciones (kind row): una fila por barra de intersección, de izquierda a derecha; la fila es la intersección que
# dibuja la matriz en esa columna: los conjuntos (por su puesto, los de df) con punto activo (color dots[1]) en ella,
# como la fila de df con esos conjuntos (si ninguna, "A + B" en el orden de df de abajo arriba: una fila sobrante);
# label_drawn = la misma lectura con los nombres IMPRESOS a la altura de cada punto activo; text = el valor impreso
# sobre la barra (capa de valores, a menos de medio paso de su centro); val = alto · máx · bar_headroom / alto del
# panel de barras (de spacing a la base común de las barras). order = puesto visual: intersecciones de izquierda a
# derecha y luego conjuntos de arriba abajo (el orden de df). Dos valores o dos nombres intercambiados, una barra más
# alta o un punto de la matriz en otra fila se ven aquí aunque el multiconjunto de textos sea el mismo.
wjpr_up_drawn <- function(b, rows, sets, dot_on, spacing, headroom) {
  ly <- wjpr_up_layers(b$plot$layers)
  pt <- function(d) {                                    # coordenadas construidas → pt (y hacia abajo)
    for (nm in intersect(c("y", "ymin", "ymax", "yend"), names(d))) d[[nm]] <- -d[[nm]]
    d
  }
  bi <- pt(as.data.frame(b$data[[ly$bars]])); bs <- pt(as.data.frame(b$data[[ly$sets]])); dt <- pt(as.data.frame(b$data[[ly$dots]]))
  bi <- bi[order((bi$xmin + bi$xmax) / 2), , drop = FALSE]
  bs <- bs[order((bs$ymin + bs$ymax) / 2), , drop = FALSE]
  xc <- (bi$xmin + bi$xmax) / 2; yr <- (bs$ymin + bs$ymax) / 2
  pitch <- if (length(xc) > 1) min(diff(xc)) else 1; rpitch <- if (length(yr) > 1) min(diff(yr)) else 1
  tx <- wjpr_drawn_texts(b); tx$y <- -tx$y
  txt_of <- function(layer, at, axis, half) {
    t <- tx[tx$source == "layer" & !is.na(tx$layer) & tx$layer %in% layer & abs(tx[[axis]] - at) < half, , drop = FALSE]
    wjpr_norm_text(paste(t$label, collapse = " "))
  }
  set_names <- vapply(yr, function(y) txt_of(ly$names, y, "y", rpitch / 2), "")
  # intersección de cada columna: conjuntos con punto activo, por puesto (df) y por nombre impreso
  on <- dt[wjpr_same_color(dt$colour, dot_on), , drop = FALSE]
  memb_at <- function(x) {
    k <- on[abs(on$x - x) < pitch / 2, , drop = FALSE]
    j <- vapply(k$y, function(y) { d <- abs(yr - y); if (min(d) < rpitch / 2) which.min(d) else NA_integer_ }, 0L)
    sort(unique(stats::na.omit(j)))
  }
  memb_rows <- lapply(strsplit(rows$label, " + ", fixed = TRUE), trimws)
  as_label <- function(nm) {                             # la fila de df con esos conjuntos; si no, "A + B" de abajo arriba
    if (!length(nm)) return("")
    hit <- which(vapply(memb_rows, function(m) setequal(m, nm), logical(1)))
    if (length(hit)) rows$label[hit[1]] else paste(rev(nm[order(match(nm, sets$label))]), collapse = " + ")
  }
  ns <- nrow(sets)
  int_lab <- vapply(xc, function(x) { j <- memb_at(x); as_label(sets$label[j[j <= ns]]) }, "")
  int_drawn <- vapply(xc, function(x) as_label(set_names[memb_at(x)][nzchar(set_names[memb_at(x)])]), "")
  hb <- bi$ymax - bi$ymin; bh <- max(bi$ymax) - spacing
  vmax <- max(rows$value, na.rm = TRUE)
  hit <- int_lab %in% rows$label
  r_int <- tibble(order = seq_along(xc), kind = ifelse(hit, "row", ""), grouping = "",
                  label = ifelse(nzchar(int_lab), int_lab, sprintf("columna en x = %.1f", xc)),
                  frac = hb / bh, frac2 = NA_real_,
                  text = vapply(xc, function(x) txt_of(ly$val, x, "x", pitch / 2), ""), text2 = "",
                  label_drawn = int_drawn, grouping_drawn = NA_character_,
                  val = hb * vmax * headroom / bh, val2 = NA_real_)
  ws <- bs$xmax - bs$xmin; sw <- max(bs$xmax) - spacing; smax <- max(sets$value, na.rm = TRUE)
  r_set <- tibble(order = nrow(rows) + seq_along(yr),
                  kind = ifelse(seq_along(yr) <= ns, "set", ""), grouping = "",
                  label = ifelse(seq_along(yr) <= ns, sets$label[pmin(seq_along(yr), ns)], sprintf("conjunto en y = %.1f", yr)),
                  frac = ws / sw, frac2 = NA_real_,
                  text = vapply(yr, function(y) txt_of(ly$setval, y, "y", rpitch / 2), ""), text2 = "",
                  label_drawn = set_names, grouping_drawn = NA_character_,
                  val = ws * smax * 1.7 / sw, val2 = NA_real_)
  bind_rows(r_int, r_set)
}
wjpr_up_drawn_fun <- function(rows, sets, dot_on, spacing, headroom) {
  force(rows); force(sets); force(dot_on); force(spacing); force(headroom)
  function(b) wjpr_up_drawn(b, rows, sets, dot_on, spacing, headroom)
}

wjpr_upset <- function(df, spec, complementos = wjpr_complementos(),
                       y_title = "% of people within\nthe justice gap", width_ratio = 0.22, st = STD) {
  wjpr_require()
  df <- df[order(df$order), ]
  inter <- df[df$kind == "row", ]; sets <- df[df$kind == "set", ]
  memb <- lapply(strsplit(inter$label, " + ", fixed = TRUE), trimws)
  bad <- setdiff(unlist(memb), sets$label)
  if (length(bad)) stop("wjpr_upset: conjuntos sin fila set en df: ", paste(unique(bad), collapse = ", "), call. = FALSE)
  k <- (if (is.null(spec$base_pt) || is.na(spec$base_pt)) 10 else spec$base_pt) / 10   # base_pt vacío llega como NA
  if (is.na(k)) k <- 1
  m <- 2 * 5.48 / k                                   # margen de ggplot + margen de patchwork (5.5 pt TeX = 5.48 bp)
  headroom <- 1.25                                    # expansión 0.25 arriba de las barras (ComplexUpset)
  lw <- 0.5 / k * ggplot2::.pt * 0.75                 # linewidth 0.5 / k (mm de ggplot) en pt
  # colores literales = los de ComplexUpset (y los defectos de wjp_upset): título negro, nombres grey30 (#4D4D4D de
  # theme_minimal), segmento negro, rejilla grey92 (#EBEBEB); los demás de STD$col
  # corregido (1.2.0): antes, capas propias con la geometría de ComplexUpset; ahora wjp_upset() con estos argumentos
  p <- WJPr::wjp_upset(
    inter, intersections = "label", target = "value", labels = "text", order = "order",
    sets = data.frame(set = sets$label, size = sets$value, text = sets$text, stringsAsFactors = FALSE),
    set_target = "size", set_labels = "text",
    bar_color = st$col$main, dot_colors = c(st$col$text, st$col$track), label_angle = 90,
    width = spec$w_in * 72, height = spec$h_in * 72, spacing = m, set_width_ratio = width_ratio,
    bar_ratio = 2 / 3, bar_headroom = headroom,
    y_title = y_title, title_size = 11 / k, title_colour = "#000000",
    label_size = 0.8 * 12.8 / k, label_colour = "#4D4D4D", label_gap = 4.43 / k,
    value_size = 3.6 * ggplot2::.pt / k, value_colour = st$col$main, value_fontface = "plain",
    set_value_size = 3.6 * ggplot2::.pt / k, set_value_colour = st$col$text,
    dot_radius = 0.375 * (3.5 / k * ggplot2::.pt + 0.5 / k * ggplot2::.stroke / 2),
    segment_color = "#000000", segment_width = lw, grid_color = "#EBEBEB", grid_width = lw,
    ptheme = WJPr::WJP_theme())
  # corregido (1.2.0): lo dibujado por fila (antes no se exponía) y los nombres de conjunto como rótulos del diseño
  # (cuentan en el multiconjunto de textos de figures, como los nombres de nodo de la 5)
  drawn <- wjpr_up_drawn_fun(inter, sets, st$col$text, m, headroom)
  wjpr_meta(p, base = "WJPr::wjp_upset", complementos = character(), drawn = drawn, rotulos = sets$label)
}
## Prueba del bloque (1.2.0@1664a23, 2026-10-27; SCRATCH/fix5/M5/pruebas/proto.R, up_var.R, iny_m5.R, iny_fig.R): la 29
## no tiene data(): todo desde la referencia (wjpr_ref_df). Galería --figs 29 → f1 0.997, local 0.354 (5.98, 4.59 in:
## puntos inactivos de la matriz), textos iguales, «19 filas ok», 0 dudosos, 1950×1620 px (6.5 × 5.4 in); con
## --sin-complementos lo mismo (antes, el adaptador anterior con 1.1.0@382aee9 y 5 complementos: 0.996 / 0.389, por
## fila no expuesto; sin ellos, 0.926 / 1.000). Lo dibujado por fila (t_val.R): 15 intersecciones y 4 conjuntos,
## label_drawn = label, text = df$text, val = df$value (máx. |Δ| 1.8e-15), con y sin complementos. Inyecciones (una a la vez, iny_fig.R: save_figure de 06_export.R + la regla de verify
## drawn_ref_rows y texto exacto frente a la referencia; exit 1 si no ok): valores 16.9 % ↔ 12.5 % (texto · text),
## nombres Problem Resolved ↔ Resolution Process (texto · label_drawn en 10 filas), 2.ª barra +5 pp (texto: su valor
## queda bajo la barra, del mismo color, y dibujo · val 12.63→17.63), barra del 1.er conjunto ×1.2 (dibujo · val
## 36.5→43.8), punto activo de la 3.ª columna en otra fila (texto + dibujo; verify: orden), valores con alfa 0 (texto ·
## faltan 15): todas detectadas; sin inyección, ok y 0 de 39 fallan. Variantes medidas (no adoptadas, up_var.R): franjas
## blancas sobre la rejilla → 0.354 (no cambia); relleno de puntos #000000 / #D9D9D9 → 0.278.

## Bloque wjpr network — figura 5 ----------------------------------------------------------------------------------
## corregido (revisión 5, 2026-10-27, pedido del usuario: las figuras con WJPr 1.2.0): la base es WJPr::wjp_network()
## con sus argumentos (antes, en 1.1.0 sin grafo de red, geom_segment + geom_polygon + geom_text sobre WJP_theme() y 5
## complementos), sobre las coordenadas del layout que ya calcula data() (df$x, df$y de los nodos: network_graph.R:
## 148-199, sin renormalizar): nodos = las filas node (ids = label, labels = el nombre en sus líneas, value_labels =
## df$text "(40%)"), aristas = las filas edge con series "sig" (lift > 1.2 y n >= 10, edges_sig de network_graph.R:
## 111-112; sin columna series, como en la referencia, todas), weight = lift (df$value). Diseño de charts5.py:196-226
## network() (la publicada): lienzo = caja de los nodos ± (radio 35 + margen 6) en unidades del SVG fuente, que mide
## 518.03 unidades por unidad de layout (node_radius, padding), escalado al ancho de la figura (k_s = W / (518.03·Δx +
## 82)); aristas con ancho (0.8 → 4.2)·k_s con gamma 0.8 según el lift (edge_width, edge_gamma; t = 0.5 si todas
## tienen el mismo) y opacidad min(0.85, (lift − 1)·0.35 + 0.2) (edge_alpha, network_graph.R:213), de la más fina a
## la más gruesa; nodos con relleno por centralidad #A8A8C8 → #39396B (colors, los de WJPr) y borde #2C3E50
## 1.2·k_s (border_width); rótulos bold 10.5·k_s (label_size), interlineado 1.15, blanco si la luminancia del relleno <
## 0.45 (label_colour = "auto", los de WJPr). Nombres en sus líneas = name_lines (los cortes de network_graph.R:219-225,
## los mismos para todo país: son las categorías del reporte). El alto que pide el layout es k_s·(518.03·Δy + 82) pt
## (coord_fixed): el h_in del manifiesto es el de lo publicado (5.869) y, si difiere en más de h_tol, el log da el valor
## para manifest/figures_<slug>.csv; el SVG sale siempre a w_in × h_in (con otro alto, franjas a los lados o arriba).
## Complemento (solo cuando hace falta): la centralidad es la suma de los lifts de TODAS las parejas del nodo, sig y
## no (network_graph.R:116-126); wjp_network la calcula con las aristas que dibuja. Si difieren (alguna pareja "no"
## con lift ≠ 0), fill = "value" con la de todas las parejas en node_value.
## wjp_network imprime nombre y valor en UN texto por nodo ("Public\nServices\n(14%)"): el texto esperado de cada nodo
## es esa unión (rotulos = nombre en sus líneas + "\n" + df$text; compara vacío), que la comparación de textos busca
## tal cual.
## Lo dibujado por fila (revisión 4, contrato extendido; wjpr_net_drawn, reescrita para la estructura de capas de
## wjp_network: geom_segment, geom_polygon y un geom_text por nodo): la red no tiene eje, así que la clave de cada fila
## es la POSICIÓN: un nodo dibujado es la fila node del df cuyo x, y está en su centro y una arista, la fila sig cuyos
## dos nodos están en sus extremos; nombre (label_drawn: las líneas del texto del nodo menos la última) y porcentaje
## (text: la última línea) impresos en cada nodo, lift decodificado del ancho de cada arista y los nodos y aristas
## dibujados sin fila en el df como sobrantes.

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
# - nodos: el centro de cada nodo dibujado (media de los vértices de su polígono, o el punto si fuera un geom_point)
#   es la fila node del df cuyo x, y está a ≤ pos_tol; sin ninguna, una fila sobrante (kind "", label "nodo en (x, y)").
#   corregido (revisión 5): wjp_network imprime nombre y valor en un solo texto por nodo (nombre\nvalor); antes había
#   una capa para cada uno (lay_name, lay_val). Ahora, con los textos visibles de las capas de texto a ≤ r_node de su
#   centro (de arriba abajo) partidos en líneas: text = la última línea (el porcentaje) y label_drawn = las anteriores
#   (el nombre, con los cortes de línea como espacio); val NA: el nodo, de radio fijo, no codifica df$value (su relleno
#   es la centralidad, que df no trae).
# - aristas: una fila por arista significativa del df (series "sig"): si hay un segmento con un extremo en cada uno de
#   sus dos nodos (a ≤ pos_tol de su x, y del df), frac = 1 y val = el lift decodificado del ancho dibujado (decode: la
#   inversa del ancho del diseño); si no, frac = 0 y val = 0 (no hay arista que codifique el lift: figures lo ve como
#   dibujo ≠ df, porque una arista sig tiene lift > 1.2). Cada segmento que no es el de una arista sig del df (otra
#   pareja, una "no", uno repetido o con un extremo fuera de los nodos) es una fila sobrante (kind "", label "A|B" o
#   sus extremos) con su val, que figures da como dibujo en una posición sin fila.
# order = el de la fila del df que ocupa la posición (la red no tiene orden de lectura); las sobrantes, detrás. Unos
# nombres o porcentajes intercambiados, un nodo desplazado o una arista quitada se ven aquí aunque el multiconjunto de
# textos sea el mismo y el parecido local no llegue al umbral.
wjpr_net_drawn <- function(b, nodes, edges, pos_tol, r_node, decode) {
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
  tx <- tx[tx$source == "layer", , drop = FALSE]
  tn <- wjpr_net_nearest(tx$x, tx$y, ctr$cx, ctr$cy)
  tx$node <- ifelse(tn$d <= r_node, tn$i, NA_integer_)
  # líneas impresas en el nodo k, de arriba abajo (cada texto partido en sus líneas)
  lines_of <- function(k) {
    t <- tx[!is.na(tx$node) & tx$node == k, , drop = FALSE]
    unlist(strsplit(t$label[order(-t$y)], "\n", fixed = TRUE))
  }
  pr <- lapply(seq_len(nrow(ctr)), lines_of)
  val_txt <- vapply(pr, function(l) if (length(l)) wjpr_norm_text(l[length(l)]) else "", "")
  name_txt <- vapply(pr, function(l) wjpr_norm_text(paste(utils::head(l, -1), collapse = " ")), "")
  ki <- ifelse(hit, m$i, NA_integer_)
  node_rows <- tibble(order = as.integer(nodes$order[ki]), kind = ifelse(hit, nodes$kind[ki], ""),
                      grouping = ifelse(hit, nodes$grouping[ki], ""),
                      label = ifelse(hit, nodes$label[ki], sprintf("nodo en (%.3f, %.3f)", ctr$cx, ctr$cy)),
                      frac = NA_real_, text = val_txt, label_drawn = name_txt, val = NA_real_)
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
wjpr_net_drawn_fun <- function(nodes, edges, pos_tol, r_node, decode) {
  force(nodes); force(edges); force(pos_tol); force(r_node); force(decode)
  function(b) wjpr_net_drawn(b, nodes, edges, pos_tol, r_node, decode)
}

# origen: network_graph.R:219-225 (case_when de label_name): los cortes de línea de los nombres de nodo, los mismos en
# todo país (son las categorías del reporte)
wjpr_network <- function(df, spec, complementos = wjpr_complementos(),
                         name_lines = c("Public Services" = "Public\nServices", "Citizenship & ID" = "Citizenship\n& ID",
                                        "Money & Debt" = "Money &\nDebt", "Law Enforcement" = "Law\nEnforcement"),
                         src_scale = 518.03, h_tol = 0.01) {
  wjpr_require()
  if (!wjpr_has_fun("wjp_network")) {
    stop("wjpr_network: la build de WJPr cargada (", wjpr_build(), ") no tiene wjp_network() (WJPr >= 1.2.0)", call. = FALSE)
  }
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
  W <- spec$w_in * 72
  dx <- diff(range(nodes$x)); ks <- W / (src_scale * dx + 82)      # pt de la figura por unidad del SVG fuente
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
  # centralidad (network_graph.R:116-126, strength sobre g_full = edges_all): suma de los lifts de TODAS las parejas
  # del nodo; la de wjp_network (fill = "centrality") suma las aristas que dibuja
  strength <- vapply(seq_len(nrow(nodes)), function(i) sum(e_all$lift[e_all$i == i | e_all$j == i], na.rm = TRUE), 0)
  strength_sig <- vapply(seq_len(nrow(nodes)), function(i) sum(e$lift[e$i == i | e$j == i], na.rm = TRUE), 0)
  comp <- character()
  value_fill <- isTRUE(complementos) && !isTRUE(all.equal(strength, strength_sig))
  # complemento: wjp_network calcula la centralidad con las aristas que dibuja (edges); el diseño, con todas las
  # parejas, también las que no se dibujan (series "no"). Cuando difieren, el relleno va por fill = "value" con la de
  # todas las parejas en node_value (el mismo reescalado 0-1 y la misma mezcla de colors). En lo publicado las 66
  # parejas se dibujan: no se aplica. Propuesta para WJPr: centrality_edges = (pares que cuentan para la centralidad,
  # por defecto edges) o una columna lógica draw en edges
  if (value_fill) comp <- c(comp, "centralidad con todas las parejas (no solo las dibujadas)")
  nd <- tibble(label = nodes$label, name = nm, text = nodes$text, x = nodes$x, y = nodes$y, strength = strength)
  ed <- tibble(from = e$from, to = e$to, lift = e$lift)
  # corregido (revisión 5): la función de WJPr del tipo con sus argumentos, en lugar de geom_segment + geom_polygon +
  # geom_text y los complementos de 1.1.0 (aristas, nodos, rótulos, lienzo, fuente). Colores de nodo (#A8A8C8 →
  # #39396B), borde #2C3E50, arista #5D6D7E, gamma 0.8, rótulos bold con interlineado 1.15 y color por contraste
  # (umbral 0.45, #1A1A1A = STD$col$text): los valores por defecto de wjp_network, que son los del diseño.
  # origen: charts5.py:196-226 (medidas) y network_graph.R:206-228 (opacidad, cortes de nombre)
  p <- WJPr::wjp_network(
    nd, ed, x = "x", y = "y", labels = "name", ids = "label", value_labels = "text", weight = "lift",
    from = "from", to = "to", node_radius = 35 / src_scale, padding = (35 + 6) / src_scale,
    fill = if (value_fill) "value" else "centrality", node_value = if (value_fill) "strength" else NULL,
    border_width = 1.2 * ks, edge_width = c(0.8, 4.2) * ks, edge_gamma = 0.8,
    edge_alpha = function(w) pmin(0.85, (w - 1) * 0.35 + 0.2), label_size = 10.5 * ks, label_fontface = "bold",
    label_colour = "auto", label_lineheight = 1.15)
  base <- "WJPr::wjp_network"
  # corregido (revisión 4): lo dibujado por fila (wjpr_net_drawn), también sin complementos. Antes la red no lo exponía
  # y pasaban como ok dos nombres o dos porcentajes de nodo intercambiados (mismo multiconjunto de textos; parecido
  # local 0.181 y 0.021) y la arista más gruesa o la más fina quitadas (0.444, 0.035). Claves: los nodos del df (x, y)
  # y las aristas sig con los índices de sus dos nodos; tolerancia de posición 0.5 unidades del SVG fuente (≈ 0.4 pt) y
  # textos del nodo a ≤ su radio (35 unidades). El lift de cada segmento sale de su ancho invirtiendo el de
  # wjp_network (linewidth = wjp_pt_line(ancho), ancho = k_s·(0.8 + 3.4·t^0.8), t = (lift − mín) / rango de las
  # dibujadas; con un solo lift, el mínimo)
  rl <- if (nrow(e) > 0) range(e$lift) else c(0, 0)
  dec <- function(lwd) {
    w <- lwd * 0.75 * ggplot2::.pt / ks
    if (diff(rl) > 0) rl[1] + (pmax(w - 0.8, 0) / (4.2 - 0.8))^(1 / 0.8) * diff(rl) else rep(rl[1], length(lwd))
  }
  net_nodes <- nodes %>% select(order, kind, grouping, label, x, y)
  net_edges <- tibble(order = edges$order[sig], kind = edges$kind[sig], grouping = edges$grouping[sig],
                      label = edges$label[sig], i = e$i, j = e$j)
  drawn <- wjpr_net_drawn_fun(net_nodes, net_edges, 0.5 / src_scale, 35 / src_scale, dec)
  # corregido (V3-07, revisión 5): el texto de cada nodo entra en la comparación de textos (attr(p, "wjpr")$rotulos)
  # como lo imprime wjp_network, nombre en sus líneas y valor en un solo texto; df$text no se compara aparte
  rot <- ifelse(nzchar(coalesce(nodes$text, "")), paste(nm, nodes$text, sep = "\n"), nm)
  wjpr_meta(p, base = base, complementos = comp, compara = character(), rotulos = rot, drawn = drawn)
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
## Prueba del bloque (revisión 5, 2026-10-27, M4; WJPr 1.2.0@1664a23; SCRATCH/fix5/M4/pruebas/): galería --figs 5 →
## ok, textos iguales (los 12 «nombre\n(NN%)»), 78 filas ok, f1 0.9999, local 0.000, mad 0.0008 (antes, 1.1.0 y 1.2.0
## con los 5 complementos de 1.1.0: 0.9999, 0.000, 0.0007); 0 complementos en Hungría (las 66 parejas son sig) y
## --sin-complementos con el SVG idéntico; frente al dibujo de 1.1.0, 0 píxeles con |Δ| > 0.2 a 96 dpi. main.R
## --stage all --figs 03,05,06 --strict: ok, 202/202. Poland y Malta (config de bancos/): ok, textos iguales, 1
## complemento (centralidad con todas las parejas), aviso del alto (5.512 y 7.017 in, como antes); frente al
## adaptador de 1.1.0, Poland 0 píxeles con |Δ| > 0.2 y Malta solo el texto de los nodos (2707 píxeles): con el
## lienzo más bajo que el layout, wjp_network lo centra en el nodo y el de 1.1.0 lo desplazaba con la escala del
## ancho (mismos nodos, aristas y rellenos). Inyecciones
## portadas a wjp_network (pruebas/iny_g3_m4.R), cada una --strict → exit 1: red_nombres (texto · label_drawn
## Consumer→Land), red_porcentajes (texto · text (40%)→(18%)), red_gruesa (dibujo · val Law Enforcement|Citizenship &
## ID 11.7788→0), red_fina (dibujo · val Consumer|Public Services 1.57238→0), red_nodo (texto · text Education: (9%)→
## y dibujo en 22 filas); en las dos primeras también el multiconjunto (faltan 2: el texto del nodo es uno solo).

## Bloque wjpr sankey — figuras 11, 16 -----------------------------------------------------------------------------
## WJPr 1.2.0: WJPr::wjp_sankey() (sankeyChart.R) dibuja el sankey de tres columnas del diseño (chart_sankey de
## 05_charts.R ← charts5.py:79): flujos de Bézier (opacidad 0.30 hacia la columna 2 y 0.45 hacia la 3), barras de
## nodo, cajas redondeadas con etiqueta, valor y nota, y encabezados de columna, en un lienzo de width × (flow_top +
## flow_height + flow_bottom) pt. wjpr_sankey() pasa el df del contrato (kind node; grouping = columna "1" | "2" | "3";
## label = clave del nodo de sankey_def(); value; text) a una fila por nodo en el orden del diseño (order1, order2,
## order3) con los textos fijos de sankey_def(which) (LAB1-3 → labels, NOTE3 → notes, headers; C2 → parent, M →
## outcome); which (assistance | drm) se deduce de los nodos de la columna 2 ("turned" = drm). Medidas: ST_SANKEY
## (sankey_top, sankey_h, sankey_margin, sankey_head, sankey_note, size_label) y las literales de chart_sankey (GAP 9,
## BAR 11; caja w = texto + 14, h = líneas · 1.22 + 6, esquina 2.5, borde 0.6; encabezados con la primera línea base a
## 14 pt e interlineado 1.25); colores de STD$col (acceso main, sin acceso eu, excluidos excl, nodos de la columna 1
## node, valor de excluidos eu_text, cajas box, notas note). Ancho = spec$w_in · 72 y flow_bottom = spec$h_in · 72 −
## flow_top − flow_height: el lienzo de WJPr es el del manifiesto (372 pt en lo publicado).
## origen: medidas y textos de chart_sankey (05_charts.R:366-453) y sankey_def (05_charts.R:299-332); llamada como
## fid_sankey de WJPr 1.2.0 (dev/report-fidelity-new.R:107-138), pero con el df del contrato, ST_SANKEY y STD$col.
## corregido (1.2.0): antes WJPr no tenía sankey y el adaptador dibujaba la geometría de chart_sankey con capas propias
## sobre WJP_theme() + wjp_palette() más 6 complementos (colores y opacidades, cajas redondeadas, textos de caja,
## encabezados como eje x superior, fuente, encabezados 700); ahora es wjp_sankey() con argumentos y 2 complementos.
## Complementos (lo que 1.2.0 aún no cubre; '# complemento:' en el código): la escala del 100 % (wjp_sankey llena
## flow_height con la columna más alta y el diseño con el 100 %: con partes a 1 decimal que suman 100.1, como en Poland
## y Malta, las barras salían 0.1 % más bajas; en Hungría no se activa) y los encabezados en Bold Italic 700 (con la
## familia "inter" de setup_fonts, bold.italic es SemiBoldItalic 600). complementos = FALSE: la llamada a wjp_sankey
## sola (Hungría: f1 1.000 y local 0.104, en los encabezados).
## Lo dibujado por fila (wjpr_sk_drawn; contrato extendido): una fila por caja de nodo, leída de la estructura de capas
## de wjp_sankey (flujos, barras, cajas, etiquetas, valores, notas, encabezados); wjp_sankey no dibuja las barras de alto
## 0 y la caja de ese nodo da alto 0 (con una fila por barra, como antes, un nodo 0 correría los puestos de su columna).
## Alto del lienzo: si la caja o el texto más bajo pasa del alto del manifiesto, el log da el h_in que lo contiene.

# Capas de wjp_sankey() por su estructura (sankeyChart.R:404-463, en este orden y cada una si tiene datos): flujos
# (GeomPolygon con fill mapeado), barras (GeomRect), cajas (GeomPolygon sin fill mapeado), etiquetas, valores, notas
# y encabezados (GeomText). Con labels, value_labels, notes y headers dados, las 7 existen; si no, error legible (un
# drawn que no reconoce las capas no puede comparar nada)
wjpr_sk_layers <- function(layers) {
  is_g <- function(cls) which(vapply(layers, function(l) inherits(l$geom, cls), logical(1)))
  poly <- is_g("GeomPolygon"); rect <- is_g("GeomRect"); txt <- is_g("GeomText")
  mapped_fill <- vapply(layers[poly], function(l) "fill" %in% names(l$mapping), logical(1))
  out <- list(bands = poly[mapped_fill], bars = rect, boxes = poly[!mapped_fill], text = txt)
  if (length(out$bands) != 1 || length(out$bars) != 1 || length(out$boxes) != 1 || length(out$text) != 4) {
    stop("wjpr_sankey: capas de wjp_sankey() no reconocidas (", length(layers), " capas: se esperaban flujos, barras, ",
         "cajas y 4 de texto: etiquetas, valores, notas y encabezados)", call. = FALSE)
  }
  c(out[c("bands", "bars", "boxes")], list(lab = out$text[1], val = out$text[2], note = out$text[3], head = out$text[4]))
}

# La misma capa con otros parámetros fijos (...: family, fontface, …): rehecha con ggplot2::layer() con su geom, stat,
# posición, datos y mapeo (cambiar un campo de ggproto(NULL, capa) desborda la pila al construir en ggplot2 4)
wjpr_sk_relayer <- function(l, ...) {
  pr <- c(l$geom_params, l$stat_params, l$aes_params); pr <- pr[!duplicated(names(pr))]
  pr <- utils::modifyList(pr, list(...))
  ggplot2::layer(geom = l$geom, stat = l$stat, position = l$position, data = l$data,
                 mapping = ggplot2::aes(!!!as.list(l$mapping)), params = pr, inherit.aes = l$inherit.aes,
                 show.legend = FALSE)
}

# Límites de las cajas de una capa construida (polígono: una por grupo, en orden de aparición; rect: una por fila), en
# el orden de las columnas de wjpr_marks_at()
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

# Lo dibujado por fila del sankey (contrato drawn de wjpr_meta, extendido), leído de b = ggplot_build() del gráfico
# final (la escala y va invertida: y construida = −pt, la mayor es la de arriba). Una fila por caja de nodo (capa de
# cajas): su columna es la de la x de su centro (X + BAR / 2, X = X1, X2, X3) y su puesto, el de arriba abajo entre las
# cajas de esa columna; la fila es el nodo del diseño en ese puesto (sankey_def: order1, order2, order3) — la posición,
# no el df en orden — y order su puesto visual (columnas de izquierda a derecha, cada una de arriba abajo: el del df);
# sin nodo del diseño en esa posición, una fila sobrante (kind ""). La barra de la caja es la de su columna (ancho BAR)
# que contiene el centro de la caja con el centro más cercano (wjp_sankey centra cada caja en su barra y no dibuja las
# de alto 0: sin barra, alto 0); una barra que no es la de ninguna caja es otra fila sobrante. frac = alto / alto del
# 100 % (100 · sc; como el drawn de chart_sankey) y val = alto / sc (el % que codifica): en las columnas 1 y 2 el alto
# es df$value · sc; en la 3 val es NA, porque su alto es la suma de los nodos de la columna 2 que llegan (s$M) y
# df$value es otra cifra (access, no_access: % de quienes lo necesitaban; excluded: la suma redondeada). text = el valor
# impreso y visible en la caja (capa de valores), label_drawn = la clave del nodo cuya etiqueta del diseño (LAB1-3:
# primero las de su columna) es la impresa en ella (capa de etiquetas, líneas de arriba abajo; si no es ninguna, lo
# impreso tal cual; "" si nada) y grouping_drawn = la columna ("1", "2", "3") cuyo encabezado del diseño (s$headers)
# es el impreso sobre la caja (capa de encabezados, líneas a menos de BAR de su centro; si no es ninguno, lo impreso;
# "" si nada). Dos valores o dos etiquetas intercambiados o una barra con el alto de otra se ven aquí aunque el
# multiconjunto de textos sea el mismo. sc = la escala que usa wjp_sankey (pt por unidad de value).
# corregido (1.2.0): una fila por caja (antes por barra) y las capas por su estructura en wjp_sankey (antes por número,
# lay_box = 3, lay_lab = 4, lay_val = 5); los encabezados son una capa de texto (antes el eje x superior)
wjpr_sk_drawn <- function(b, s, keys, X, BAR, sc) {
  ly <- wjpr_sk_layers(b$plot$layers)
  db <- as.data.frame(b$data[[ly$boxes]]); gb <- b$plot$layers[[ly$boxes]]$geom
  bx <- wjpr_sk_box_bounds(db)
  ords <- list(s$order1, s$order2, s$order3); off <- cumsum(c(0, lengths(ords)))
  cx <- (bx$x0 + bx$x1) / 2; cy <- (bx$y0 + bx$y1) / 2
  bx$col <- vapply(cx, function(x) { k <- which(abs(x - (X + BAR / 2)) < 0.5); if (length(k)) k[1] else NA_integer_ }, 0L)
  bx$rank <- NA_integer_
  for (cl in unique(stats::na.omit(bx$col))) {
    k <- which(bx$col %in% cl); bx$rank[k] <- as.integer(rank(-cy[k], ties.method = "first"))
  }
  key <- vapply(seq_len(nrow(bx)), function(k) {
    cl <- bx$col[k]; r <- bx$rank[k]
    if (is.na(cl) || r > length(ords[[cl]])) NA_character_ else ords[[cl]][r]
  }, "")
  ki <- ifelse(is.na(key), NA_integer_, match(paste(bx$col, key), paste(keys$grouping, keys$label)))
  hit <- !is.na(ki)
  # barra de cada caja: de ancho BAR en su columna, conteniendo el centro de la caja; la de centro más cercano
  br <- wjpr_sk_box_bounds(as.data.frame(b$data[[ly$bars]]))
  br <- br[abs((br$x1 - br$x0) - BAR) < 0.01, , drop = FALSE]
  br$col <- vapply(br$x0, function(x) { k <- which(abs(x - X) < 0.5); if (length(k)) k[1] else NA_integer_ }, 0L)
  bar_of <- vapply(seq_len(nrow(bx)), function(k) {
    c <- which(br$col %in% bx$col[k] & br$y0 <= cy[k] & br$y1 >= cy[k])
    if (!length(c)) return(NA_integer_)
    c[which.min(abs((br$y0[c] + br$y1[c]) / 2 - cy[k]))]
  }, 0L)
  hgt <- ifelse(is.na(bar_of), 0, br$y1[bar_of] - br$y0[bar_of])
  # textos visibles de cada caja (etiquetas y valores) y encabezados
  tx <- wjpr_drawn_texts(b)
  tl <- tx[tx$source == "layer" & tx$layer %in% c(ly$lab, ly$val), , drop = FALSE]
  mt <- if (nrow(db) && nrow(tl)) wjpr_marks_at(db, gb, tl$x, tl$y) else NULL
  tl$box <- if (is.null(mt)) rep(NA_integer_, nrow(tl)) else
    apply(mt$inside, 1, function(r) { h <- which(r); if (length(h)) h[1] else NA_integer_ })
  in_box <- function(k, lay) {
    t <- tl[!is.na(tl$box) & tl$box == k & tl$layer == lay, , drop = FALSE]
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
  hd <- tx[tx$source == "layer" & tx$layer == ly$head, , drop = FALSE]
  hdict <- setNames(as.character(seq_along(s$headers)), wjpr_norm_text(vapply(s$headers, paste, "", collapse = " ")))
  head_of <- function(k) {
    a <- hd[is.finite(hd$x) & abs(hd$x - cx[k]) <= BAR, , drop = FALSE]
    if (!nrow(a)) return("")
    h <- wjpr_norm_text(paste(a$label[order(-a$y)], collapse = " "))
    if (h %in% names(hdict)) unname(hdict[[h]]) else h
  }
  n0 <- max(c(off[length(off)], keys$order, 0L), na.rm = TRUE)
  ord <- ifelse(hit, off[pmax(1L, coalesce(bx$col, 1L))] + bx$rank, NA_integer_)
  ord[!hit] <- as.integer(n0 + seq_len(sum(!hit)))
  out <- tibble(order = as.integer(ord), kind = ifelse(hit, keys$kind[ki], ""), grouping = ifelse(hit, keys$grouping[ki], ""),
                label = as.character(ifelse(hit, keys$label[ki], sprintf("caja en x = %.1f, puesto %s", cx, bx$rank))),
                frac = hgt / (100 * sc), frac2 = NA_real_,
                text = vapply(seq_len(nrow(bx)), in_box, "", lay = ly$val), text2 = "",
                label_drawn = vapply(seq_len(nrow(bx)), function(k) lab_key(in_box(k, ly$lab), bx$col[k]), ""),
                grouping_drawn = vapply(seq_len(nrow(bx)), head_of, ""),
                val = ifelse(!hit | bx$col %in% 1:2, hgt / sc, NA_real_), val2 = NA_real_)
  # barras que no son la de ninguna caja: filas sobrantes (con su alto)
  free <- setdiff(seq_len(nrow(br)), bar_of)
  if (length(free)) {
    h <- br$y1[free] - br$y0[free]
    out <- bind_rows(out, tibble(order = as.integer(max(out$order, n0) + seq_along(free)), kind = "", grouping = "",
                                 label = sprintf("barra en x = %.1f, y = %.1f", br$x0[free], br$y1[free]),
                                 frac = h / (100 * sc), frac2 = NA_real_, text = "", text2 = "", label_drawn = "",
                                 grouping_drawn = "", val = h / sc, val2 = NA_real_))
  }
  arrange(out, order)
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
  ord <- list(s$order1, s$order2, s$order3)
  node_vals <- function(g, keys, col) {
    r <- df[df$grouping == g, ]; i <- match(keys, r$label)
    if (anyNA(i)) stop("wjpr_sankey: faltan en df los nodos ", paste(keys[is.na(i)], collapse = ", "), call. = FALSE)
    setNames(r[[col]][i], keys)
  }
  vals <- lapply(1:3, function(j) node_vals(as.character(j), ord[[j]], "value"))
  if (anyNA(c(vals[[1]], vals[[2]]))) stop("wjpr_sankey: nodo sin valor en las columnas 1-2", call. = FALSE)
  txt <- lapply(1:3, function(j) node_vals(as.character(j), ord[[j]], "text"))
  join <- function(x) vapply(x, paste, "", collapse = "\n")
  nodes <- tibble(column = rep(1:3, lengths(ord)), node = unlist(ord), value = unname(unlist(vals)),
                  text = unname(unlist(txt)),
                  label = unname(c(join(s$LAB1)[s$order1], join(s$LAB2)[s$order2], join(s$LAB3)[s$order3])),
                  parent = c(rep(NA_character_, length(s$order1)), unname(unlist(s$C2[s$order2])), rep(NA_character_, length(s$order3))),
                  outcome = c(rep(NA_character_, length(s$order1)), unname(unlist(s$M[s$order2])), rep(NA_character_, length(s$order3))),
                  note = c(rep(NA_character_, length(s$order1) + length(s$order2)),
                           unname(vapply(s$order3, function(o) paste(s$NOTE3[[o]] %||% character(), collapse = "\n"), ""))))
  nodes$note[!nzchar(coalesce(nodes$note, ""))] <- NA_character_
  W <- spec$w_in * 72; H <- spec$h_in * 72
  GAP <- 9; BAR <- 11                                   # origen: 05_charts.R:379 (chart_sankey)
  X1 <- st$sankey_margin; X3 <- W - st$sankey_margin - BAR; X2 <- (X1 + X3) / 2
  n_max <- max(lengths(ord))
  tot <- max(sum(vals[[1]]), sum(vals[[2]]))           # columna 3: la suma de la 2 (wjp_sankey: value_flow)
  top <- st$sankey_top; fh <- st$sankey_h
  comp <- character()
  if (isTRUE(complementos)) {
    # complemento: escala del 100 %. wjp_sankey llena flow_height con la columna más alta (sc = (flow_height − gap ·
    # (n − 1)) / máx. de las sumas de columna) y el diseño con el 100 % (chart_sankey: sc = (sankey_h − GAP · (n − 1)) /
    # 100): con las partes de la columna 2 a 1 decimal que suman 100.1 las barras salían un 0.1 % más bajas. Se pide a
    # WJPr un flow_height que da la escala del diseño y se sube flow_top la mitad de lo añadido (el centrado de las
    # columnas, flow_top + (flow_height − usado) / 2, queda el del diseño). Propuesta: wjp_sankey(total = 100), el valor
    # que llena flow_height (NULL = la columna más alta)
    sc0 <- (st$sankey_h - GAP * (length(s$order2) - 1)) / 100
    fh <- sc0 * tot + GAP * (n_max - 1); top <- st$sankey_top - (fh - st$sankey_h) / 2
    if (abs(fh - st$sankey_h) > 1e-9) comp <- c(comp, "escala del 100 % (flow_height y flow_top)")
  }
  sc <- (fh - GAP * (n_max - 1)) / tot                  # la escala de wjp_sankey con estos argumentos
  # corregido (1.2.0): wjp_sankey avisa "The second-column nodes of … add up to more than their parent" cuando las partes
  # de la columna 2 suman más que su nodo de la columna 1; con el método publicado (partes a 1 decimal, not_contacted =
  # 100 − contacted) pasa por redondeo en Poland y Malta (100.1) y el diseño lo dibuja igual (chart_sankey no avisa). Se
  # calla solo si el exceso de cada nodo cabe en el redondeo (0.05 por parte y por el nodo); uno mayor sigue avisando
  par_of <- nodes$parent[nodes$column == 2]; v2 <- nodes$value[nodes$column == 2]
  over <- vapply(s$order1, function(k) sum(v2[par_of %in% k]) - vals[[1]][[k]], 0)
  tol <- 0.05 * (vapply(s$order1, function(k) sum(par_of %in% k), 0) + 1) + 1e-9
  rounding <- all(over <= tol)
  p <- withCallingHandlers(WJPr::wjp_sankey(
    nodes, column = "column", node = "node", target = "value", value_labels = "text", parent = "parent",
    outcome = "outcome", labels = "label", headers = unname(join(s$headers)), notes = "note",
    colors = c(access = st$col$main, no_access = st$col$eu, excluded = st$col$excl), alpha = c(0.30, 0.45),
    bar_width = BAR, gap = GAP, width = W, flow_height = fh, flow_top = top, flow_bottom = H - top - fh,
    side_margin = st$sankey_margin, node_color = st$col$node,
    header_size = st$sankey_head, header_fontface = "bold.italic", header_colour = st$col$main,
    header_lineheight = 1.25, header_baseline = 14,
    label_size = st$size_label, label_colour = st$col$text, value_fontface = "semibold.italic",
    value_colors = stats::setNames(c(st$col$main, st$col$eu_text), c("", "excluded")),
    note_size = st$sankey_note, note_colour = st$col$note, box_fill = st$col$box, box_border = st$col$text,
    box_border_width = 0.6, box_radius = 2.5, box_padding = c(3, 7), line_height = 1.22,
    ptheme = WJPr::WJP_theme()),
    warning = function(w) if (rounding && grepl("add up to more than their parent", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning"))
  base <- "WJPr::wjp_sankey"
  ly <- wjpr_sk_layers(p$layers)
  drawn <- wjpr_sk_drawn_fun(s, df %>% select(order, kind, grouping, label), c(X1, X2, X3), BAR, sc)
  if (isTRUE(complementos)) {
    # complemento: encabezados en Bold Italic 700 como el diseño (chart_sankey: text_path(…, 700, TRUE)). wjp_sankey
    # resuelve header_fontface = "bold.italic" con la familia de la sesión ("inter" de setup_fonts, cuya bold.italic es
    # SemiBoldItalic 600) y no tiene argumento de familia. Se cambia la familia de la capa de encabezados (copia de la
    # capa, rehecha con ggplot2::layer(): un campo cambiado en ggproto(NULL, capa) desborda la pila al construir) por
    # inter_bi700 (wjpr_family_bi700, bloque cards). Propuesta: wjp_sankey(header_family = ) o que
    # wjp_face("bold.italic") use "<familia> Bold" cuando exista, como hace con SemiBold y Medium
    p$layers[[ly$head]] <- wjpr_sk_relayer(p$layers[[ly$head]], family = wjpr_family_bi700(), fontface = "italic")
    comp <- c(comp, "encabezados 700 cursiva (familia inter_bi700)")
  }
  # corregido (revisión 4): el lienzo mide el h_in del manifiesto (el de lo publicado, 372 pt) y cada caja se centra en
  # su nodo, así que con nodos bajos al fondo la caja más baja (o su texto) pasaba del borde inferior y se cortaba sin
  # aviso (hasta 5.3 pt en 12 de 26 países). Si pasa, el log da el alto que la contiene con 3 pt de margen (redondeado
  # hacia arriba a 3 decimales) para manifest/figures_<slug>.csv; el dibujo no cambia (con ese alto wjp_sankey solo
  # alarga flow_bottom). corregido (1.2.0): el fondo se lee de las capas de wjp_sankey (cajas: y + la mitad del borde de
  # 0.6 pt; textos: línea base + 0.25 em)
  tag <- sprintf("fig %02d", as.integer(spec$fig %||% 11L))
  pt_of <- function(k) (p$layers[[k]]$aes_params$size %||% ggplot2::GeomText$default_aes$size) * ggplot2::.pt
  bottom <- max(c(p$layers[[ly$boxes]]$data$y + 0.6 / 2,
                  unlist(lapply(c(ly$lab, ly$val, ly$note), function(k) p$layers[[k]]$data$y + 0.25 * pt_of(k)))))
  if (is.finite(bottom) && bottom > H + 1e-6) {
    h_need <- ceiling((bottom + 3) / 72 * 1000) / 1000
    log_msg(tag, " aviso: el layout de ", spec$country %||% "este país", " pide h_in = ", sprintf("%.3f", h_need),
            " in y el manifiesto da ", sprintf("%.3f", spec$h_in), " (la caja o el texto más bajo acaba a ",
            sprintf("%.1f", bottom), " pt y el lienzo mide ", sprintf("%.1f", H), ": se corta por abajo): fíjelo en ",
            "manifest/figures_<slug>.csv con la fila «", spec$fig, ",", sprintf("%.3f", h_need), "» (columnas fig,h_in)")
  }
  wjpr_meta(p, base = base, complementos = comp, drawn = drawn)
}
## Prueba del bloque (1.2.0@1664a23, 2026-10-27/28; SCRATCH/fix5/M5/pruebas/proto.R, sk_pais.R, t_cero.R, t_alto.R,
## t_val.R, iny_m5.R, iny_run_m5.sh): galería --figs 11,16 → 11 (data()) f1 1.000, local 0.000, «10 filas ok»; 16
## (referencia) 0.997, 0.243 (7.03, 2.31 in: la referencia trae valores enteros; con rows_sankey(journey(ctx), "drm")
## 1.000 y 0.000); textos iguales, 0 dudosos, 2520×1550 px; --sin-complementos: 11 → 1.000 / 0.104 (encabezados 600 en
## vez de 700), 16 → 0.997 / 0.243. Antes (adaptador anterior, 1.1.0@382aee9, 6 complementos): 1.000 / 0.000 y 0.997 /
## 0.243; frente a él, el PNG nuevo difiere en bordes de caja de 1 px (4836 píxeles a 300 dpi con |Δ| > 0.1; 5 con |Δ| >
## 0.25 en Poland). Lo dibujado por fila (t_val.R), con y sin complementos: text, label_drawn = df, val = df$value en las
## columnas 1-2 (máx. |Δ| 1.4e-14; NA en la 3). main.R --country hungary --stage all --figs 11 --strict → ok, 57/57,
## exit 0. Inyecciones (iny_run_m5.sh 11; bancos/iny_g3.R sk_* con bancos/iny_run.sh dan lo mismo): valores
## appropriate/needed_no intercambiados (texto · text 21%→38%), etiquetas Appropriate/Non-appropriate adviser (texto ·
## label_drawn), barra de appropriate con el alto de needed_no (dibujo · val 21.4→37.8), encabezados de las columnas 1 y
## 2 intercambiados (texto · grouping_drawn en 7 filas), valor 8 % del color de la caja (texto · faltan 1): todas exit 1
## y «fallo · figures: …». Poland y Malta (--stage all --figs 11): figures ok, 10 filas, escala del 100 % activa (la
## columna 2 suma 100.1), sin el aviso de redondeo de wjp_sankey; aviso de alto: Poland 375.0 pt → «11,5.250», Malta
## 377.6 → «11,5.287» (los mismos que con el adaptador anterior); con h_in = 5.287 (t_alto.R) el SVG mide 8.400 × 5.287
## in, la caja cabe con 3.0 pt y no hay aviso. Nodo de valor 0 (t_cero.R; Malta: unknown 0 %): wjp_sankey no dibuja su
## barra y lo dibujado por fila da val 0 y ninguna diferencia con df.
## Antes (revisión 4): lo dibujado por fila por barra (capas 3, 4 y 5 por número, encabezados del eje x superior);
## verify de la 11 56/56; avisos de alto en Malta, Portugal y Greece, ninguno en Hungría.


## Prueba
## Migración a WJPr 1.2.0@1664a23 (2026-10-27/28; agentes M1-M5 en SCRATCH/fix5, integración en fix5/INT): cada
## adaptador es la función de WJPr del tipo con sus argumentos (ver la «Prueba» de cada bloque). Complementos en
## Hungría: 0 en 36 figuras; 1 en la 6 (margen del título), 11 y 16 (encabezados 700) y 32, 41 y 42 (encabezados 700);
## antes, con 1.1.0@382aee9, 5-13 por figura (367 en total). Aceptación de la integración: main.R --stage all --figs
## 01-19 --strict → 19/19 verificadas (f1 1.000; local 0.000 salvo la 7, 0.118, los guiones de la línea de corte);
## tools/wjpr_gallery.R (las 42) → 41 ok y la 42 «local» 0.708 (diferencia conocida, ver el bloque cards); 53
## inyecciones por main.R (fig 01, 02, 03, 05, 06, 07, 10, 11, 13) y 43 simuladas desde la referencia (8, 15, 29, 30,
## 32, 34, 37, 41, 42) detectadas en figures y verify, salvo col_series en la 37 (sus dos series valen 99 %).
## Historia: el 23-oct (1.1.0@382aee9 y main a46c831, con complementos) la galería dio f1 0.996-1.000 en las 42.
