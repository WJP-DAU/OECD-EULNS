## 04_svg.R — texto en trazos (Inter Tight), medidas y primitivas SVG, utilidades de archivos SVG -------------------
## Origen (copias literales; # origen: junto a cada función):
##   src/hungary/finales/svg_text.R:16-157         font_file, glyph_contours, shape_ref, text_width, fmt, text_d,
##                                                 text_path, css_prop, text_prop, outline_svg_text
##   src/hungary/finales/report_charts.R:22-124    STD, with_std, mid_base, r2, rect_el, circle_el, line_el, path_el,
##                                                 svg_doc, chart_out, wrap_text, lines_els, label_column, label_el,
##                                                 value_el, layout_rows
##   src/hungary/finales/standard_figures.R:38-45  ST_DUMB … ST_CARDS; 403-429 svg_viewbox, set_size, live_texts
## Sin el source(config.R) ni el FONT_DIR de svg_text.R:10-12: FONT_DIR lo define setup_fonts() (00_setup.R) = ROOT/fonts.
## Contrato §5.C (nombres obligatorios): chart_out(W, H, els, drawn, text_pt) → objeto de clase "chart_out" (§4.5);
## svg_size(path); outline_svg_text(path_in, path_out = path_in); set_size(path, width_in, height_in = NULL);
## count_live_text(path); LABEL_COL. Unidades: puntos; el <svg> declara width/height en pulgadas (pt / 72) y el
## viewBox en pt, de modo que las medidas de STD son las impresas con la figura colocada al 100 %.
## Solo define funciones y constantes: nada se ejecuta al cargar.


## Fuentes y texto en trazos ---------------------------------------------------------------------------------------
## Cada cadena se convierte en un <path> con los contornos de sus glifos (systemfonts + textshaping): la figura se ve
## igual en máquinas sin Inter Tight. text_path(): cadena → <path>; text_width(): avance de una cadena (maquetación).

# origen: svg_text.R:14-23. Archivos estáticos de Inter Tight por peso. El pipeline registra bold.italic como
# SemiBoldItalic (setup.R y utils.R) y svglite escribe esa cara como font-weight 600. FONT_DIR: setup_fonts(), que ya
# comprueba los 8 archivos (el mensaje de error original queda tal cual).
font_file <- function(weight = 400, italic = FALSE) {
  weight <- if (is.character(weight)) c(normal = 400, bold = 700, bolder = 700)[[weight]] else weight
  face <- if (weight >= 650) "Bold" else if (weight >= 550) "SemiBold" else if (weight >= 450) "Medium" else ""
  name <- paste0("InterTight-", if (face == "" && !italic) "Regular" else paste0(face, if (italic) "Italic"), ".ttf")
  path <- file.path(FONT_DIR, name)
  if (!file.exists(path)) stop("No existe la fuente ", path, " (revise fonts/ y setup_fonts())", call. = FALSE)   # corregido: mensaje
  path
}

# origen: svg_text.R:25-41. Los contornos se calculan una vez por fuente y glifo a tamaño 100 y se escalan después
.glyph_cache <- new.env(parent = emptyenv())
.REF_SIZE <- 100
glyph_contours <- function(path, index) {
  key <- paste(basename(path), index)
  hit <- .glyph_cache[[key]]
  if (!is.null(hit)) return(hit)
  g <- systemfonts::glyph_outline(index, path = path, size = .REF_SIZE, tolerance = 1)
  out <- if (nrow(g) == 0) list() else lapply(split(g, g$contour), function(d) cbind(d$x, d$y))
  .glyph_cache[[key]] <- out
  out
}

shape_ref <- function(string, path) {
  s <- textshaping::shape_text(string, path = path, size = .REF_SIZE, res = 72)
  list(index = s$shape$index, x = s$shape$x_offset, width = s$metrics$width[1])
}

# origen: svg_text.R:43-53. Ancho de una cadena (pt) y formato numérico de los atributos SVG (2 decimales, sin ceros)
text_width <- function(string, size, weight = 400, italic = FALSE) {
  if (!nzchar(string)) return(0)
  shape_ref(string, font_file(weight, italic))$width * size / .REF_SIZE
}

fmt <- function(v) {
  out <- sprintf("%.2f", v)
  out <- sub("\\.?0+$", "", out)
  out[out == "-0"] <- "0"
  out
}

# origen: svg_text.R:55-87. Datos de trazado de una cadena con la línea base en (x, y). anchor: start | middle | end.
# stretch: factor horizontal alrededor del ancla (el textLength de svglite).
text_d <- function(string, x, y, size, weight = 400, italic = FALSE, anchor = "start", stretch = 1) {
  if (!nzchar(trimws(string))) return("")
  path <- font_file(weight, italic)
  sh <- shape_ref(string, path)
  k <- size / .REF_SIZE
  w <- sh$width * k * stretch
  x0 <- x - switch(anchor, start = 0, middle = w / 2, end = w)
  parts <- character()
  for (i in seq_along(sh$index)) {
    contours <- glyph_contours(path, sh$index[i])
    if (length(contours) == 0) next
    gx <- x0 + sh$x[i] * k * stretch
    for (m in contours) {
      # coordenadas absolutas redondeadas a 0.01, escritas como movimientos relativos (exacto y la mitad de tamaño)
      px <- round(gx + m[, 1] * k * stretch, 2)
      py <- round(y - m[, 2] * k, 2)
      parts <- c(parts, paste0("M", fmt(px[1]), " ", fmt(py[1]),
                               if (length(px) > 1) paste0("l", paste(fmt(diff(px)), fmt(diff(py)), collapse = " ")), "z"))
    }
  }
  paste(parts, collapse = "")
}

text_path <- function(string, x, y, size, weight = 400, italic = FALSE, anchor = "start", fill = "#1A1A1A",
                      transform = NULL) {
  d <- text_d(string, x, y, size, weight, italic, anchor)
  if (!nzchar(d)) return("")
  # estilo en línea: svglite y otros generadores dan estilo CSS a todo path (stroke, fill: none)
  sprintf("<path d=\"%s\" style=\"fill:%s;stroke:none\"%s/>", d, fill,
          if (is.null(transform)) "" else sprintf(" transform=\"%s\"", transform))
}


## Contornear el texto de un SVG existente (salida de svglite / ggplot) --------------------------------------------

# origen: svg_text.R:92-107. Propiedad de un <text>: CSS primero (svglite escribe style=...), luego atributo de
# presentación, luego heredada de los ancestros, el más cercano primero (<g fill=...>)
css_prop <- function(style, prop) {
  if (is.na(style)) return(NA_character_)
  m <- regmatches(style, regexec(paste0("(?:^|;)\\s*", prop, "\\s*:\\s*([^;]+)"), style, perl = TRUE))[[1]]
  if (length(m) == 2) trimws(m[2]) else NA_character_
}

text_prop <- function(node, prop) {
  for (nd in c(list(node), as.list(xml2::xml_parents(node)))) {
    v <- css_prop(xml2::xml_attr(nd, "style"), prop)
    if (is.na(v)) v <- xml2::xml_attr(nd, prop)
    if (!is.na(v)) return(v)
  }
  NA_character_
}

# origen: svg_text.R:109-157 (cuerpo literal). Firma del contrato §5.C: path_in, path_out = path_in (el original:
# svg_in, svg_out). Sustituye cada <text> por su contorno; error si hay <tspan> o si al terminar queda algún <text>.
# svglite da el ancho que midió (textLength): se respeta, como haría un navegador, cuando coincide con Inter Tight
# (±max_stretch); una diferencia mayor indica que el SVG se midió con otra fuente y el texto se marca doubtful en el
# log devuelto (tibble text, size, weight, italic, family, textlength_ratio, doubtful), igual que toda familia que no
# sea Inter. Devuelve el log de forma invisible.
outline_svg_text <- function(path_in, path_out = path_in, max_stretch = 0.03) {
  svg_in <- path_in; svg_out <- path_out
  doc <- xml2::read_xml(svg_in)
  texts <- xml2::xml_find_all(doc, "//*[local-name()='text']")
  log <- vector("list", length(texts))
  for (i in seq_along(texts)) {
    node <- texts[[i]]
    if (length(xml2::xml_find_all(node, "./*[local-name()='tspan']")) > 0) {
      stop("outline_svg_text: <tspan> no soportado en ", svg_in, call. = FALSE)
    }
    string <- xml2::xml_text(node)
    num <- function(v, default) if (is.na(v)) default else as.numeric(sub("px$", "", v))
    x <- num(xml2::xml_attr(node, "x"), 0)
    y <- num(xml2::xml_attr(node, "y"), 0)
    size <- num(text_prop(node, "font-size"), 16)
    weight <- text_prop(node, "font-weight")
    weight <- if (is.na(weight)) 400 else if (grepl("^[0-9]+$", weight)) as.numeric(weight) else weight
    italic <- identical(text_prop(node, "font-style"), "italic")
    anchor <- dplyr::coalesce(text_prop(node, "text-anchor"), "start")
    fill <- dplyr::coalesce(text_prop(node, "fill"), "#000000")
    opacity <- text_prop(node, "fill-opacity")
    tr <- xml2::xml_attr(node, "transform")
    stretch <- 1
    w_inter <- text_width(string, size, weight, italic)
    tl <- xml2::xml_attr(node, "textLength")
    ratio <- if (!is.na(tl) && w_inter > 0) as.numeric(sub("px$", "", tl)) / w_inter else NA_real_
    if (!is.na(ratio) && abs(ratio - 1) <= max_stretch) stretch <- ratio
    d <- text_d(string, x, y, size, weight, italic, anchor, stretch)
    family <- text_prop(node, "font-family")
    log[[i]] <- tibble::tibble(text = string, size = size, weight = as.character(weight), italic = italic,
                               family = family, textlength_ratio = ratio,
                               doubtful = (!is.na(ratio) && abs(ratio - 1) > max_stretch) ||
                                 (!is.na(family) && !grepl("inter", family, ignore.case = TRUE)))
    if (!nzchar(d)) {
      xml2::xml_remove(node)
      next
    }
    p <- xml2::xml_add_sibling(node, "path", .where = "before")
    xml2::xml_set_attr(p, "d", d)
    xml2::xml_set_attr(p, "style", paste0("fill:", fill, ";stroke:none",
                                          if (!is.na(opacity)) paste0(";fill-opacity:", opacity)))
    if (!is.na(tr)) xml2::xml_set_attr(p, "transform", tr)
    xml2::xml_remove(node)
  }
  xml2::write_xml(doc, svg_out, options = c("format", "no_declaration"))
  n_left <- count_live_text(svg_out)                  # comprobación final (contrato §5.C): texto sin contornear
  if (n_left > 0) stop("outline_svg_text: quedan ", n_left, " <text> sin contornear en ", svg_out, call. = FALSE)
  invisible(dplyr::bind_rows(log))
}


## Medidas y colores estándar ---------------------------------------------------------------------------------------

# origen: report_charts.R:20-43. Proporciones de gen_grouped_bars() (etiquetas 16, valores 14.23, fila 27, barra 24.4
# unidades en la figura 31 del reporte), a un tamaño base 11 % mayor que esa figura. Cada chart_* recibe st (copia
# de STD con las medidas que cambia una figura: with_std()).
STD <- list(
  size_label  = 10,     # etiquetas de categoría, encabezados de grupo, National / EU Average
  size_value  = 9,      # valores al final de las barras
  size_legend = 9.5,
  size_axis   = 9,
  pitch       = 15.5,   # una fila de barra
  bar         = 13.5,   # alto de la barra (barras agrupadas)
  header      = 22,     # bloque de un encabezado de grupo
  header_base = 17,     # línea base del encabezado dentro de su bloque
  gap_label   = 6,      # etiqueta -> barra
  track       = 250,    # barra llena (100 %)
  gap_value   = 5,      # fin de la barra -> valor
  value_w     = 27,     # sitio para el valor ("100%", "N/A")
  pair_bar    = 9,      # barras pareadas: alto de cada barra (2 pt entre las dos)
  pair_pitch  = 26,
  dot_r       = 3.8,
  segment     = 7,
  col = list(main = "#575796", track = "#E5E8E8", text = "#1A1A1A", eu = "#A8A8C8", eu_text = "#7D7DA8",
             exp = "#B200AA", axis = "#1A1A1A", grid = "#D0D1D3", node = "#8E9393", excl = "#C4C8C8",
             box = "#F2F3F3", note = "#7D7DA8", delta = "#4B4B7A", card_low = "#ECECF4")
)
with_std <- function(...) utils::modifyList(STD, list(...))

# origen: standard_figures.R:37-45. Medidas de las figuras que difieren de STD
ST_DUMB   <- with_std(pitch = 18)                                         # pista: llena W_CALLOUT
ST_A01    <- with_std(pitch = 22, bar = 18, header = 26, header_base = 20) # pista: llena W_CALLOUT
ST_HBARS  <- with_std(pitch = 22, bar = 18, track = 300, value_w = 34)
ST_COLS   <- with_std(col_w = 46, col_h = 180)
ST_WIDE2  <- with_std(pair_pitch = 21, pair_bar = 8)                      # pista: llena la columna completa
ST_FUNNEL <- with_std(funnel_label_w = 190, funnel_bar = 40, funnel_gap = 18, funnel_value = 11)
ST_SANKEY <- with_std(sankey_top = 62, sankey_h = 300, sankey_margin = 92, sankey_head = 10.8, sankey_note = 8.75)
ST_CARDS  <- with_std(card_label = 11, card_label_w = 170, card_h = 48, card_value = 16, card_delta = 9)

# origen: report_charts.R:45-46. Línea base que centra dígitos / mayúsculas en un y dado (altura de mayúsculas de
# Inter Tight = 0.727 em)
mid_base <- function(cy, size) cy + 0.36 * size


## Primitivas SVG y documento ----------------------------------------------------------------------------------------

# origen: report_charts.R:48-70. Elementos con estilo en línea; svg_doc(): documento con width/height en pulgadas,
# viewBox en pt y fondo blanco
r2 <- function(v) fmt(round(v, 2))
rect_el <- function(x, y, w, h, fill, rx = 0, stroke = NULL, sw = 0.6, opacity = 1) {
  if (w <= 0 || h <= 0) return("")
  sprintf("<rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\"%s style=\"fill:%s;%s%s\"/>", r2(x), r2(y), r2(w), r2(h),
          if (rx > 0) sprintf(" rx=\"%s\"", r2(rx)) else "", fill,
          if (is.null(stroke)) "stroke:none" else sprintf("stroke:%s;stroke-width:%s", stroke, r2(sw)),
          if (opacity < 1) sprintf(";fill-opacity:%s", r2(opacity)) else "")
}
circle_el <- function(cx, cy, r, fill) {
  sprintf("<circle cx=\"%s\" cy=\"%s\" r=\"%s\" style=\"fill:%s;stroke:none\"/>", r2(cx), r2(cy), r2(r), fill)
}
line_el <- function(x1, y1, x2, y2, col, width, dash = NULL) {
  sprintf("<line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" style=\"stroke:%s;stroke-width:%s;fill:none%s\"/>",
          r2(x1), r2(y1), r2(x2), r2(y2), col, r2(width), if (is.null(dash)) "" else paste0(";stroke-dasharray:", dash))
}
path_el <- function(d, fill, opacity = 1) {
  sprintf("<path d=\"%s\" style=\"fill:%s;stroke:none%s\"/>", d, fill, if (opacity < 1) sprintf(";fill-opacity:%s", r2(opacity)) else "")
}
svg_doc <- function(W, H, body) {
  paste0(sprintf("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%.3fin\" height=\"%.3fin\" viewBox=\"0 0 %s %s\">",
                 W / 72, H / 72, r2(W), r2(H)),
         rect_el(0, 0, W, H, "#FFFFFF"), paste(body, collapse = ""), "</svg>")
}

# origen: report_charts.R:71 (devolvía list(svg, width_pt, height_pt, text_pt)). Contrato §4.5: W, H en pt; drawn =
# tibble(order, kind, grouping, label, frac, frac2, text, text2) con lo efectivamente dibujado (frac NA si no hay
# barra; text/text2 = df$text/df$text2). Lo devuelven las chart_<tipo>() de 05_charts.R.
chart_out <- function(W, H, els, drawn, text_pt = 10) {
  need <- c("order", "kind", "grouping", "label", "frac", "frac2", "text", "text2")
  if (!is.data.frame(drawn) || !all(need %in% names(drawn))) {
    stop("chart_out: drawn debe ser un tibble con columnas ", paste(need, collapse = ", "), call. = FALSE)
  }
  structure(list(svg = svg_doc(W, H, els), width_in = W / 72, height_in = H / 72, text_pt = text_pt,
                 drawn = tibble::as_tibble(drawn)), class = "chart_out")
}


## Texto en las figuras: ajuste de línea, columna de etiquetas, filas -----------------------------------------------

# origen: report_charts.R:73-82. Ajuste de línea voraz a un ancho máximo (pt)
wrap_text <- function(s, size, max_w, weight = 400, italic = FALSE) {
  words <- strsplit(s, " ", fixed = TRUE)[[1]]
  lines <- character(); cur <- ""
  for (w in words) {
    cand <- if (cur == "") w else paste(cur, w)
    if (cur != "" && text_width(cand, size, weight, italic) > max_w) { lines <- c(lines, cur); cur <- w } else cur <- cand
  }
  c(lines, cur)
}
# origen: report_charts.R:83-88. Líneas centradas verticalmente en yc
lines_els <- function(lines, x, yc, size, anchor = "end", weight = 400, italic = FALSE, fill = STD$col$text, lh = 1.22) {
  step <- size * lh
  y0 <- yc - step * (length(lines) - 1) / 2 + 0.36 * size
  vapply(seq_along(lines), function(i) text_path(lines[i], x, y0 + (i - 1) * step, size, weight, italic, anchor, fill), "")
}

# origen: report_charts.R:90-98. Columna de etiquetas: borde derecho = etiqueta más ancha de las specs dadas (lista de
# tibbles con kind y label; los encabezados y promedios van en 600 cursiva)
label_column <- function(specs, st = STD) {
  s <- dplyr::bind_rows(specs)
  if (nrow(s) == 0) return(NA_real_)
  w <- mapply(function(label, kind) {
    if (kind == "row") text_width(label, st$size_label) else text_width(label, st$size_label, 600, TRUE)
  }, s$label, s$kind)
  ceiling(max(w)) + 2
}

# origen: report_charts.R:100-106. Etiqueta de una fila (row: regular; header / avg: 600 cursiva en el color principal)
# y valor al final de la barra (600 cursiva)
label_el <- function(r, L, base, st = STD) {
  if (r$kind == "row") text_path(r$label, L, base, st$size_label, anchor = "end", fill = st$col$text)
  else text_path(r$label, L, base, st$size_label, 600, TRUE, anchor = "end", fill = st$col$main)
}
value_el <- function(txt, x, base, fill, anchor = "start", st = STD) {
  text_path(txt, x, base, st$size_value, 600, TRUE, anchor = anchor, fill = fill)
}

# origen: report_charts.R:108-124. Filas y encabezados comunes a las barras. spec: tibble con una fila por línea de la
# figura (kind avg | header | row, label, v1_frac, v1_text, …). draw_row(r, y) devuelve los elementos de una línea de
# datos y su alto; el encabezado solo lleva su etiqueta.
layout_rows <- function(spec, L, y0, draw_row, st = STD) {
  y <- y0; els <- character()
  for (i in seq_len(nrow(spec))) {
    r <- spec[i, ]
    if (r$kind == "header") {
      els <- c(els, label_el(r, L, y + st$header_base, st))
      y <- y + st$header
    } else {
      out <- draw_row(r, y)
      els <- c(els, out$els)
      y <- y + out$h
    }
  }
  list(els = els, y = y)
}

# Columna de etiquetas común (pt) por familia de figuras, medida con las etiquetas publicadas: std = barras agrupadas
# y pares, dumb = dumbbells (10, 15), hbars = barras horizontales (13, 14, 18, 19, 36). origen: standard_figures.R:456-460
# (L_col, allí calculado con label_column(); aquí fijo por contrato §5.C). label_column() permite recalcularlo.
LABEL_COL <- c(std = 149, dumb = 84, hbars = 172)


## Archivos SVG: tamaño físico, viewBox, texto vivo -----------------------------------------------------------------

# origen: standard_figures.R:403-408. viewBox (x, y, w, h) del <svg>; sin viewBox, width/height en pt
svg_viewbox <- function(path) {
  root <- xml2::xml_root(xml2::read_xml(path))
  vb <- xml2::xml_attr(root, "viewBox")
  if (is.na(vb)) vb <- paste(0, 0, sub("pt$", "", xml2::xml_attr(root, "width")), sub("pt$", "", xml2::xml_attr(root, "height")))
  as.numeric(strsplit(trimws(vb), "[ ,]+")[[1]])
}

# origen: standard_figures.R:410-424 (cuerpo literal). Firma del contrato §5.C: set_size(path, width_in, height_in =
# NULL): el mismo dibujo a un nuevo tamaño físico (pulgadas) manteniendo el viewBox; alto proporcional si height_in
# es NULL. Como el original, añade un fondo blanco sobre todo el viewBox; pad = c(izq, der) ensancha el viewBox.
# Escribe en path_out (= path) y devuelve c(width_in, height_in).
set_size <- function(path, width_in, height_in = NULL, path_out = path, pad = c(0, 0)) {
  svg_in <- path; svg_out <- path_out
  doc <- xml2::read_xml(svg_in)
  root <- xml2::xml_root(doc)
  vb <- svg_viewbox(svg_in) + c(-pad[1], 0, sum(pad), 0)
  if (is.null(height_in)) height_in <- width_in * vb[4] / vb[3]
  xml2::xml_set_attr(root, "viewBox", paste(fmt(vb), collapse = " "))
  xml2::xml_set_attr(root, "width", sprintf("%.3fin", width_in))
  xml2::xml_set_attr(root, "height", sprintf("%.3fin", height_in))
  first <- xml2::xml_child(root, 1)
  bg <- xml2::xml_add_sibling(first, "rect", .where = "before")
  xml2::xml_set_attrs(bg, c(x = fmt(vb[1]), y = fmt(vb[2]), width = fmt(vb[3]), height = fmt(vb[4]), style = "fill:#FFFFFF;stroke:none"))
  xml2::write_xml(doc, svg_out, options = c("format", "no_declaration"))
  c(width_in = width_in, height_in = height_in)
}

# origen: standard_figures.R:426-429. Textos vivos (<text>) de un SVG, sin espacios sobrantes
live_texts <- function(path) {
  doc <- xml2::read_xml(path)
  str_squish(xml2::xml_text(xml2::xml_find_all(doc, "//*[local-name()='text']")))
}

# Contrato §5.C. Tamaño físico declarado en el <svg>, en pulgadas: acepta "in" (svg_doc, set_size), "pt" (svglite),
# px o sin unidad (96 por pulgada), cm, mm y pc; sin width/height (o en %) usa el viewBox en pt.
svg_size <- function(path) {
  root <- xml2::xml_root(xml2::read_xml(path))
  to_in <- function(v) {
    if (is.na(v) || grepl("%", v, fixed = TRUE)) return(NA_real_)
    v <- trimws(v)
    num <- as.numeric(sub("[A-Za-z]*$", "", v))
    unit <- tolower(sub("^[^A-Za-z]*", "", v))
    k <- c("in" = 1, "pt" = 1 / 72, "px" = 1 / 96, "cm" = 1 / 2.54, "mm" = 1 / 25.4, "pc" = 1 / 6)
    if (unit == "") return(num / 96)
    if (!unit %in% names(k)) stop("svg_size: unidad '", unit, "' no reconocida en ", path, call. = FALSE)
    num * k[[unit]]
  }
  w <- to_in(xml2::xml_attr(root, "width")); h <- to_in(xml2::xml_attr(root, "height"))
  if (is.na(w) || is.na(h)) {
    vb <- svg_viewbox(path)
    if (is.na(w)) w <- vb[3] / 72
    if (is.na(h)) h <- vb[4] / 72
  }
  list(width_in = w, height_in = h)
}

# Contrato §5.C. Número de <text> (texto vivo) de un SVG: debe ser 0 en toda figura del reporte
count_live_text <- function(path) {
  length(xml2::xml_find_all(xml2::read_xml(path), "//*[local-name()='text']"))
}


## Prueba -----------------------------------------------------------------------------------------------------------
## cd /tmp && Rscript --vanilla SCRATCH/agente_C/prueba.R (SCRATCH = scratchpad de la sesión; carga 00_setup.R, 01_load.R,
## 04_svg.R y 05_charts.R, cfg real y setup_fonts(cfg)). Resultado: 29/29 comprobaciones ok.
##  - Figura 9 dibujada con chart_grouped() desde finales/datos/Figure_09_access_information.csv: svg_size() = 6.069 x
##    8.375 in; count_live_text() = 0; sus 53 <rect> (fondo aparte) y sus 66 <path> de texto son idénticos (dif 0 pt,
##    mismos datos d) a los de output/finales/Hungria/figuras/Figure_09_access_information.svg (mismo código de origen);
##    el fondo mide 436.97 pt (W = w_in * 72) frente a 437 (xv + value_w del original). Contra src/hungary/finales/svg/
##    Figure_09.svg (fuente ggplot de la publicada) las 26 fracciones de barra coinciden con max |dif| = 0.00002.
##  - svglite (ggplot de 4 x 3 in con 11 <text> en "inter"): outline_svg_text(path) → 0 <text>, 0 dudosos (familia
##    "Inter Tight"); set_size(path, 6.069) → 6.069 x 4.552 in con el viewBox 0 0 288 216 intacto; set_size(path,
##    6.069, 4) → 6.069 x 4.000 in; path_out distinto no toca el original; <tspan> → error "no soportado"; svg_size()
##    acepta in, pt y px / sin unidad (8.268 in para width="793.707").
