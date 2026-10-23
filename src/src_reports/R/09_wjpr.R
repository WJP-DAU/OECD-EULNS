## 09_wjpr.R — figuras con WJPr + complementos de código ---------------------------------------------------------
## Origen: nuevo (2026-10-23, pedido del usuario: usar WJPr en el mayor número de figuras y complementarlo con código
## hasta el diseño publicado). Para cada tipo de figura, wjpr_<tipo>(df, spec, ...) parte de la función de WJPr del
## tipo (o de WJP_theme()/wjp_palette() cuando WJPr no tiene una) y la complementa hasta acercarse a lo publicado.
## Recibe el df del contrato (§4.4: order, kind, grouping, label, value, value2, text, text2 + extras) y la spec,
## igual que chart_<tipo>(), y devuelve un ggplot con attr(p, "wjpr") = list(base, complementos). La exporta y la
## mide tools/wjpr_gallery.R; main.R todavía no la usa (las figuras siguen con su plot()).
## Cada complemento lleva "# complemento: <qué le falta a WJPr>": es la fuente del documento de brechas.
## Solo funciones: nada se ejecuta al cargar; WJPr solo se pide al llamar (deps.csv lo declara opcional).

WJPR_FAMILY <- "inter"   # familia registrada por setup_fonts() (Inter Tight); la de las figuras publicadas


## Utilidades comunes ----------------------------------------------------------------------------------------------

wjpr_require <- function() {
  if (!requireNamespace("WJPr", quietly = TRUE)) stop("WJPr no está instalado (deps.csv: opcional)", call. = FALSE)
  invisible(TRUE)
}

# Marca el ggplot con la función de WJPr de partida y los complementos aplicados (los lee la galería)
# compara = columnas del df cuyos textos se imprimen tal cual (la galería los busca en el dibujo); p. ej. en la matriz
# text2 es la banda de color, no un texto
wjpr_meta <- function(p, base, complementos = character(), compara = c("text", "text2")) {
  attr(p, "wjpr") <- list(base = base, complementos = unique(complementos), compara = compara)
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

# Textos efectivamente dibujados por las capas de texto (posición finita), sin marcas de texto enriquecido
wjpr_texts <- function(p) {
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  is_text <- vapply(b$plot$layers, function(l) inherits(l$geom, c("GeomText", "GeomLabel", "GeomRichText", "GeomTextRepel")), logical(1))
  x <- unlist(lapply(b$data[is_text], function(d) {
    if (!"label" %in% names(d)) return(NULL)
    ok <- (if ("x" %in% names(d)) is.finite(d$x) else TRUE) & (if ("y" %in% names(d)) is.finite(d$y) else TRUE)
    as.character(d$label[ok])
  }))
  trimws(gsub("<[^>]+>", "", x[!is.na(x) & nzchar(trimws(x))]))
}

# Multiconjunto de textos: lo que falta (esperado y no dibujado) y lo que sobra (dibujado y no esperado)
wjpr_text_diff <- function(drawn, expected) {
  expected <- expected[!is.na(expected) & nzchar(expected)]
  k <- union(drawn, expected)
  n1 <- as.integer(table(factor(drawn, levels = k))); n2 <- as.integer(table(factor(expected, levels = k)))
  list(faltan = rep(k, pmax(n2 - n1, 0)), sobran = rep(k, pmax(n1 - n2, 0)))
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
wjpr_similarity <- function(png_new, png_pub, r = 2L, dpi_new = NA) {
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
  list(precision = prec, recall = rec, f1 = if (is.na(prec) || is.na(rec) || prec + rec == 0) NA_real_ else 2 * prec * rec / (prec + rec),
       mad = mean(abs(ga - gb)))
}

# df del contrato desde la referencia publicada, para dibujar figuras cuyo data() aún no existe. En los tipos con
# barra relativa a 100 % (agrupadas, dumbbell, hbars, pareadas) value/value2 = frac·100 (la barra publicada exacta);
# en el resto, value/value2 tal cual (en columns frac es value/vmax). Extras (x, y, n, lo, hi) se conservan.
wjpr_ref_df <- function(spec, cfg) {
  f <- file.path(cfg$paths$reference, paste0(figure_stem(spec), ".csv"))
  if (!file.exists(f)) stop("sin referencia: ", basename(f), call. = FALSE)
  d <- read_csv(f, col_types = cols(.default = col_character()), na = character())
  num <- intersect(c("order", "value", "value2", "frac", "frac2", "x", "y", "n", "lo", "hi"), names(d))
  d <- d %>% mutate(across(all_of(num), ~ suppressWarnings(as.numeric(.x))))
  if (spec$tipo %in% c("grouped_bars", "dumbbell", "hbars", "paired_bars")) {
    scale <- if (spec$tipo == "grouped_bars" && !any(grepl("%", d$text))) 10 else 100      # fig 4: media 0-10
    d <- d %>% mutate(value = coalesce(frac * scale, value), value2 = coalesce(frac2 * scale, value2))
  }
  d %>% select(-any_of(c("frac", "frac2", "tol", "tol2", "tol_xy"))) %>% mutate(order = as.integer(order))
}

## Bloques por tipo ---------------------------------------------------------------------------------------------------
## (cada bloque: encabezado "## Bloque wjpr <tipo> — figuras …", la función wjpr_<tipo>(df, spec, ...) y sus
## auxiliares; se añaden debajo)

## Bloque wjpr grouped_bars — figuras 1, 4, 9, 12, 17, 20-28, 31, 33, 35, 38-40 --------------------------------------
## Base: WJPr::wjp_groupbars() con argumentos documentados: colors (principal y pista; en la 4 la pista transparente),
## labels (= df$text, impreso tal cual), group_order, level_order, show_national + national_style "bar" +
## national_position "top" + national_label + national_value (fila National Average; la 27 no la lleva),
## national_group_label, label_position "end", bar_width (barra / paso de fila del diseño), show_axis FALSE,
## strip_position "left" y ptheme = wjpr_theme(). Se conservan sus capas (geom_col apilado = barra + pista 0-100 %,
## geom_text de los valores), su faceta (paneles de alto libre) y su barra nacional; los complementos van marcados
## "# complemento:". Medidas del diseño publicado: STD (04_svg.R; la 27, ST_A01 con la pista que llena los 6.5 in,
## como chart_grouped(fill_w = spec$w_in * 72)) y la columna de etiquetas LABEL_COL[["std"]]. Variantes por figura
## (spec$fig): 4 = media 0-10 sin pista y con el valor tras la barra (chart_grouped(track = FALSE, value_at_end =
## TRUE)); 27 = ST_A01 sin filas de promedio. complementos = FALSE devuelve la llamada a WJPr sola (la línea de base
## con la que se mide el efecto de los complementos). Unidades: las medidas del diseño son puntos de 1/72 in (los de
## svglite y de 04_svg.R) = "bigpts" de grid; el "pt" de grid/ggplot2::margin() es 1/72.27 in (0.4 % menor).

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

# Entrada de wjp_groupbars: una fila por categoría (grp, lvl, v = barra 0-100, lab = df$text, kind). Con complementos,
# la fila EU Average en la faceta de los promedios y, por grupo, una faceta de encabezado (una fila sin barra) antes
# de la de sus categorías. Devuelve d, el orden de facetas (group_order), level_order y el tipo de cada faceta.
wjpr_gb_input <- function(df, g, complementos = TRUE) {
  avg <- df[df$kind == "avg", ]; body <- df[df$kind %in% c("header", "row"), ]
  # complemento: WJPr solo acepta proporciones (0-1) o porcentajes (0-100) y leería la media 0-10 de la 4 como %
  # (propuesta: scale_max = 10): la barra entra como value / scale · 100 y el texto, de df$text
  pct <- function(v) as.numeric(v) / g$scale * 100
  rows <- body[body$kind == "row", ]
  groups <- unique(as.character(body$grouping))
  row_d <- function(r, grp) tibble(grp = grp, lvl = r$label, v = pct(r$value), lab = coalesce(r$text, ""), kind = r$kind)
  if (!complementos) {
    d <- row_d(rows, as.character(rows$grouping))
    lev <- setNames(lapply(groups, function(gr) rev(d$lvl[d$grp == gr])), groups)
    return(list(d = d, order = groups, lev = lev, panels = rep("rows", length(groups)), avg = avg))
  }
  extra <- if (nrow(avg)) avg[-1, ] else avg
  d <- row_d(extra, rep(WJPR_GB_NAT, nrow(extra))) %>% mutate(kind = "avg")
  order <- character(); panels <- character(); lev <- list()
  if (nrow(avg)) { order <- WJPR_GB_NAT; panels <- "avg"; lev[[WJPR_GB_NAT]] <- rev(avg$label) }
  for (gr in groups) {
    b <- body[as.character(body$grouping) == gr, ]
    h <- b[b$kind == "header", ]; r <- b[b$kind == "row", ]
    if (nrow(h)) {
      key <- paste0(WJPR_GB_HDR, gr)
      d <- bind_rows(d, row_d(h, rep(key, nrow(h))) %>% mutate(lab = ""))
      order <- c(order, key); panels <- c(panels, "header"); lev[[key]] <- rev(h$label)
    }
    if (nrow(r)) {
      d <- bind_rows(d, row_d(r, rep(gr, nrow(r))))
      order <- c(order, gr); panels <- c(panels, "rows"); lev[[gr]] <- rev(r$label)
    }
  }
  list(d = d, order = order, lev = lev, panels = panels, avg = avg)
}

wjpr_grouped_bars <- function(df, spec, complementos = TRUE, ...) {
  wjpr_require()
  df <- df[order(df$order), ]
  g <- wjpr_gb_design(df, spec); st <- g$st
  inp <- wjpr_gb_input(df, g, complementos); d <- inp$d; avg <- inp$avg
  has_nat <- nrow(avg) > 0
  nat_v <- if (has_nat) as.numeric(avg$value[1]) / g$scale * 100 else NULL
  p <- WJPr::wjp_groupbars(
    data = d, target = "v", grouping = "grp", levels = "lvl",
    colors = c(st$col$main, if (g$track) st$col$track else "transparent"),   # 4: sin pista (complemento transparente)
    labels = "lab", group_order = inp$order[inp$order != WJPR_GB_NAT], level_order = inp$lev,
    show_national = has_nat, national_value = nat_v, national_style = "bar", national_position = "top",
    national_label = if (has_nat) avg$label[1] else NULL, national_group_label = WJPR_GB_NAT,
    label_position = "end", bar_width = st$bar / st$pitch, show_axis = FALSE, strip_position = "left",
    ptheme = wjpr_theme())
  if (!complementos) return(wjpr_meta(p, base = "WJPr::wjp_groupbars", complementos = character()))

  comp <- c("fuente Inter Tight en capas y tema (wjpr_font)", if (g$scale != 100) "media 0-10 a barra 0-100")
  # complemento: WJPr no admite una segunda fila de promedio (national_value es un escalar; propuesta: national_value
  # con nombres, p. ej. c("National Average" = 61, "EU Average" = 61)): la fila UE entra como dato en la faceta del
  # promedio nacional (national_group_label), debajo de él (wjpr_gb_input).
  if (sum(d$kind == "avg")) comp <- c(comp, "fila EU Average")
  # complemento: WJPr solo dibuja los grupos como strips de faceta, a la izquierda de las etiquetas y sin alinear con
  # ellas (propuesta: group_header = "row"): cada encabezado es una faceta de una fila sin barra, en la columna de
  # etiquetas y alineado a la derecha como las categorías (wjpr_gb_input), y los strips se ocultan (tema, abajo).
  comp <- c(comp, "encabezados en la columna de etiquetas")

  # complemento: con la barra nacional WJPr pone axis.text.y = ggtext::element_markdown (S3) y con ggplot2 >= 4.0 no se
  # le puede sumar ningún theme(axis.text.y = …) ("Can't merge the axis.text.y theme element"), ni el de wjpr_font()
  # (propuesta: estilo de las etiquetas por argumento, sin element_markdown en el tema): se reemplaza (%+replace%) y se
  # vuelve a poner, completo, al final.
  p@theme <- p@theme %+replace% ggplot2::theme(axis.text.y = ggplot2::element_blank())
  p <- wjpr_font(p)
  comp <- c(comp, "axis.text.y por %+replace% (element_markdown S3 en ggplot2 4)")

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
  # superior de 0.208 em la baja a la línea base publicada (desaparece con la propuesta anterior: sin Markdown)
  p@theme <- p@theme %+replace% ggplot2::theme(
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
  wjpr_meta(p, base = "WJPr::wjp_groupbars", complementos = comp)
}
## Prueba del bloque (2026-10-23): Rscript tools/wjpr_gallery.R --figs 1,4,9,12,17,20,21,22,23,24,25,26,27,28,31,33,35,
## 38,39,40 → 20/20 ok, textos iguales, 0 dudosos, f1_publicada 1.000 (r = 0 px: 0.996-0.999; mad 0.0005-0.0011);
## complementos = FALSE (WJPr solo): f1 0.17-0.79. Posiciones en el SVG: barra en x0 = 155, etiquetas a 149, valores a
## 410 (27: 441), filas y líneas base de encabezados a ±0.05 pt de STD/ST_A01; escape Markdown ("a*b_c*", "1. …", "&").


## Bloque wjpr dumbbell — figuras 10, 15 ----------------------------------------------------------------------------
## Base: WJPr::wjp_dumbbells() con sus argumentos documentados: data larga (dos filas por línea de la figura,
## Perception / Experience, también para los encabezados, con valor NA), target, grouping (clave única por línea:
## dos categorías con el mismo nombre no se funden), cgroups, colors, labels (= df$text / df$text2 tal cual), labpos
## (posición de cada valor, a dot_r + 3 pt del punto y por fuera del par; la automática de WJPr, ±7 en la escala y
## recortada a 3-97, da NA en una fila con un solo valor: la 18-24 de la 10 perdía su 68 %), cvec (colores del
## reporte), order (el de df) y ptheme = wjpr_theme(). Se conservan sus 7 capas (blank, ribbon de franjas, segment,
## 2 point, 2 text): los complementos cambian parámetros y mapeos de esas capas, sus escalas, el clip y el tema, y la
## leyenda es la guía del color de los puntos (ninguna capa nueva). Medidas: ST_DUMB y LABEL_COL[["dumb"]], las de
## chart_dumbbell() (05_charts.R): leyenda con líneas base 10 y 24 pt, rótulos del eje con base 44, línea del eje en
## 49, filas desde 52; la figura 15 (18-24 sin ningún valor) sale como fila vacía sin rama propia.
## complementos = FALSE devuelve WJPr sin complementos (como el piloto: sin encabezados), para medir la brecha.
## Prueba (2026-10-23, tools/wjpr_gallery.R --figs 10,15): textos iguales, 0 dudosos, f1 con la publicada 1.000 en las
## dos (sin complementos: 0.128 y 0.170; solo con wjpr_font: 0.129 y 0.170, es decir, la brecha es de geometría).
## Auxiliares de este bloque (los usa también el bloque hbars): unidades del diseño → ggplot2 y edición de capas.

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

wjpr_dumbbell <- function(df, spec, legend = c("General population: perception",
                                               "People with a non-trivial problem: experience"),
                          st = ST_DUMB, complementos = TRUE) {
  wjpr_require()
  df <- df[order(df$order), , drop = FALSE]
  cg <- c("Perception", "Experience")
  cvec <- c(Perception = st$col$main, Experience = st$col$exp)
  v1 <- as.numeric(df$value); v2 <- as.numeric(df$value2)
  t1 <- coalesce(as.character(df$text), ""); t2 <- coalesce(as.character(df$text2), "")
  if (!isTRUE(complementos)) {
    r <- df$kind != "header"
    long <- bind_rows(tibble(cat = df$label[r], serie = cg[1], v = v1[r], lab = t1[r]),
                      tibble(cat = df$label[r], serie = cg[2], v = v2[r], lab = t2[r]))
    p <- WJPr::wjp_dumbbells(data = long, target = "v", grouping = "cat", cgroups = cg, colors = "serie",
                             labels = "lab", cvec = cvec, order = setNames(df$order[r], df$label[r]),
                             ptheme = wjpr_theme())
    return(wjpr_meta(p, base = "WJPr::wjp_dumbbells"))
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
  long <- bind_rows(
    tibble(key = key, serie = cg[1], v = v1, lab = t1, pos = if_else(left1, v1 - gap, v1 + gap)),
    tibble(key = key, serie = cg[2], v = v2, lab = t2, pos = if_else(left2, v2 - gap, v2 + gap)))
  p <- WJPr::wjp_dumbbells(data = long, target = "v", grouping = "key", cgroups = cg, colors = "serie",
                           labels = "lab", labpos = "pos", cvec = cvec, order = setNames(seq_along(key), key),
                           ptheme = wjpr_theme())
  p <- wjpr_font(p)
  ip <- wjpr_which_layers(p, "GeomPoint"); it <- wjpr_which_layers(p, "GeomText")
  is <- wjpr_which_layers(p, "GeomSegment")
  # complemento: tamaño de punto (r = dot_r, sin trazo): wjp_dumbbells fija size = 4.5 (r ≈ 5.5 pt), sin argumento
  # complemento: leyenda: wjp_dumbbells da a los puntos un color fijo y no dibuja leyenda; se mapea el color de cada
  # serie para que la guía de ggplot2 la dibuje con los textos de `legend`
  p <- wjpr_edit_layers(p, ip[1], params = list(size = wjpr_pt2size(st$dot_r), stroke = 0, colour = NULL),
                        mapping = ggplot2::aes(colour = "Perception"))
  p <- wjpr_edit_layers(p, ip[2], params = list(size = wjpr_pt2size(st$dot_r), stroke = 0, colour = NULL),
                        mapping = ggplot2::aes(colour = "Experience"))
  # complemento: segmento del color y grosor del reporte (wjp_dumbbells: #BFBFBF, linewidth 2.5, sin argumento)
  p <- wjpr_edit_layers(p, is, params = list(colour = st$col$track, linewidth = wjpr_pt2lw(st$segment)))
  # complemento: valores en semibold cursiva de size_value pt, anclados por fuera del punto (wjp_dumbbells: negrita de
  # 10 pt centrada en labpos, sin argumento de cara, tamaño ni alineación)
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
  p <- p +
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
    ggplot2::scale_colour_manual(values = cvec, breaks = cg, labels = legend, name = NULL) +
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
      legend.position = "top", legend.justification = "left", legend.direction = "vertical",
      legend.margin = wjpr_margin_bp(l = -shift), legend.key = ggplot2::element_blank(),
      legend.key.spacing.y = wjpr_unit_bp(0), legend.key.width = wjpr_unit_bp(2 * st$dot_r),
      legend.key.height = wjpr_unit_bp(key_h),
      legend.box.spacing = wjpr_unit_bp(geo$ax_base - ax_box - (top + 2 * key_h)),
      legend.text = ggplot2::element_text(size = st$size_legend, colour = st$col$text,
                                          margin = wjpr_margin_bp(l = 5 - shift)),
      # complemento: márgenes que ponen la pista en x0..x0 + track y el panel desde la línea del eje hasta abajo
      plot.margin = wjpr_margin_bp(t = top, r = W - x0 - track, b = 0,
                                   l = x0 - st$gap_label - wjpr_labcol_w(df$label, df$kind, st$size_label)))
  wjpr_meta(p, base = "WJPr::wjp_dumbbells",
            complementos = c("fuente (wjpr_font)", "tamaño de punto", "leyenda (color mapeado + guía)",
                             "segmento: color y grosor", "valores: cara, tamaño y anclaje",
                             "posición de los valores (labpos)", "encabezados y alturas de fila (escala discreta)",
                             "eje cada 25 % sin expansión", "sin franjas alternas", "clip off", "sin rejilla",
                             "columna de etiquetas: alineación y estilos", "eje: tamaño, línea y márgenes"))
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
## Prueba (2026-10-23, tools/wjpr_gallery.R --figs 13,14,18,19,36): textos iguales, 0 dudosos, f1 1.000 en las cinco
## (sin complementos: 0.554, 0.680, 0.709, 0.862, 0.267; lollipops sin complementos 0.26-0.49; edgebars 0.31-0.43).
wjpr_hbars <- function(df, spec, base = c("wjp_bars", "wjp_lollipops", "wjp_edgebars"), st = ST_HBARS,
                       complementos = TRUE) {
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
  comp <- "fuente (wjpr_font)"
  if (base == "wjp_edgebars") {
    # complemento: wjp_edgebars mapea family = "Lato Full" dentro de aes() en el rótulo (geom_richtext): wjpr_font
    # quita ese mapeo y el rótulo quedaría sin familia (Helvetica); se fija la del reporte como parámetro
    p <- wjpr_edit_layers(p, wjpr_which_layers(p, "GeomRichText"), params = list(family = WJPR_FAMILY))
  }
  p <- wjpr_font(p)
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
## modificada de una capa de WJPr, pista, leyenda {country} | EU Average y medidas de Inter Tight en el texto de grid.
## Fig 2: ancho 8.4 in con columna de etiquetas propia (la más ancha + 2 pt), pista hasta la columna de valores.
## Prueba (galería, 2026-10-23): fig 2 f1 1.000 (sin complementos 0.530), textos iguales, 0 dudosos.

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

# Capa de pista (barra al 100 % detrás de cada barra, 0 si el valor está suprimido) en la posición de las barras
wjpr_wb_track <- function(width, position, fill) {
  ggplot2::geom_col(data = function(x) dplyr::mutate(x, target_var = ifelse(na, 0, 100)),
                    ggplot2::aes(group = colors_var), fill = fill, width = width, position = position,
                    show.legend = FALSE, inherit.aes = TRUE)
}

# Leyenda {country} | EU Average dentro del panel, arriba a la izquierda (cuadros de 9 pt a top pt del borde, texto a
# gap_key pt del cuadro, gap_item pt entre elementos: chart_paired / chart_columns)
wjpr_wb_legend <- function(st, top = 2, key = 9, gap_key = 4, gap_item = 14) {
  ggplot2::theme(
    legend.position = "inside", legend.position.inside = c(0, 1), legend.justification.inside = c(0, 1),
    legend.direction = "horizontal", legend.title = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(), legend.key = ggplot2::element_blank(),
    legend.margin = ggplot2::margin(top, 0, 0, 0), legend.key.size = grid::unit(key, "pt"),
    legend.key.spacing.x = grid::unit(gap_item, "pt"),
    legend.text = ggplot2::element_text(size = st$size_legend, colour = st$col$text, margin = ggplot2::margin(l = gap_key)))
}

# bare = TRUE: solo wjp_bars con argumentos documentados (para medir la brecha sin complementos)
wjpr_paired_bars <- function(df, spec, bare = FALSE, ...) {
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
  p <- WJPr::wjp_bars(data = d, target = "v", grouping = "label", labels = "lab", colors = "serie",
                      cvec = stats::setNames(c(st$col$main, st$col$eu), levels(d$serie)), direction = "horizontal",
                      stacked = FALSE, lab_pos = "lab_pos", order = "order", width = w, ptheme = wjpr_theme())
  if (bare) return(wjpr_meta(p, base = "wjp_bars"))
  # complemento: wjp_bars fija position = "stack" en geom_col (stacked solo cambia el color del texto); falta un
  # argumento position = "dodge" (con su ancho y el orden de las series: el país arriba)
  dodge <- ggplot2::position_dodge(width = 2 * (st$pair_bar + 2) / st$pair_pitch, reverse = TRUE)
  # complemento: wjp_bars oculta la leyenda (show.legend = FALSE fijo); falta un argumento legend
  is_col <- wjpr_wb_is(p, "GeomCol"); is_txt <- wjpr_wb_is(p, "GeomText")
  p$layers[is_col] <- lapply(p$layers[is_col], wjpr_wb_layer, position = dodge, show.legend = TRUE,
                             aes_params = list(linewidth = 0))
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
    wjpr_wb_legend(st)
  wjpr_meta(wjpr_font(p), base = "wjp_bars",
            complementos = c("lado a lado (dodge, país arriba)", "leyenda {country} | EU Average",
                             "texto por serie (posición, color, semibold cursiva 9 pt)", "pista al 100 %",
                             "escala de valores sin expansión", "alto de fila (expansión discreta)",
                             "tema sin eje ni rejilla, etiquetas a la derecha", "fuente Inter Tight en capas"))
}

## Bloque wjpr columns — figuras 30, 37 -----------------------------------------------------------------------------
## wjp_bars vertical con país y UE lado a lado, alturas relativas al máximo de las dos series (la más alta = col_h),
## valores encima de cada columna, etiquetas debajo y línea base (chart_columns); usa los auxiliares wjpr_wb_*.
## Prueba (galería, 2026-10-23): f1 0.998 / 1.000 (sin complementos 0.675 / 0.706), textos iguales, 0 dudosos;
## la 30 desde la referencia usa value redondeado (wjpr_ref_df descarta frac): alturas a ≤ 3 pt de las publicadas.

wjpr_columns <- function(df, spec, bare = FALSE, ...) {
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
  p <- WJPr::wjp_bars(data = d, target = "v", grouping = "label", labels = "lab", colors = "serie",
                      cvec = stats::setNames(c(st$col$main, st$col$eu), levels(d$serie)), direction = "vertical",
                      stacked = FALSE, lab_pos = "lab_pos", order = "order", width = w, ptheme = wjpr_theme())
  if (bare) return(wjpr_meta(p, base = "wjp_bars"))
  # complemento: wjp_bars fija position = "stack" en geom_col; falta un argumento position = "dodge" (y su ancho)
  dodge <- ggplot2::position_dodge(width = 2 * (st$col_w + gap) / gw)
  # complemento: wjp_bars oculta la leyenda (show.legend = FALSE fijo); falta un argumento legend
  is_col <- wjpr_wb_is(p, "GeomCol"); is_txt <- wjpr_wb_is(p, "GeomText")
  p$layers[is_col] <- lapply(p$layers[is_col], wjpr_wb_layer, position = dodge, show.legend = TRUE,
                             aes_params = list(linewidth = 0))
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
    wjpr_wb_legend(st, top = 3)
  wjpr_meta(wjpr_font(p), base = "wjp_bars",
            complementos = c("lado a lado (dodge)", "leyenda {country} | EU Average",
                             "texto por serie (posición, color, semibold cursiva 9 pt)", "escala al máximo sin expansión",
                             "etiquetas con ajuste de línea", "tema sin eje ni rejilla, con línea base", "fuente Inter Tight en capas"))
}

## Bloque wjpr severity_bars — figuras 7 ----------------------------------------------------------------------------
## wjp_bars vertical con las dos series lado a lado por nivel de severidad 0-10 y la línea de corte en 3.5 (entre 3 y
## 4), como F03.R:28-45: geom_col dodge 0.8 width 0.75 y theme_minimal('inter') con textos a 13 sobre un lienzo × 1.3
## que se reduce a w_in. Aquí todo va en pt finales (pt del original × k = 1/1.3: texto a 10 pt). Usa wjpr_wb_layer().
## Prueba (galería, 2026-10-23): f1 0.999 (sin complementos 0.380), textos iguales, 0 dudosos; idéntica al F03
## original salvo los guiones de la línea de corte (svglite no escala el guion por debajo de lwd 1: 4 pt, no 3.5).

wjpr_severity_bars <- function(df, spec, bare = FALSE, ...) {
  wjpr_require()
  st <- STD; k <- 1 / 1.3
  lv <- c(spec$country, "EU Average")
  d <- df %>% filter(kind == "bar") %>%
    mutate(serie = factor(ifelse(grouping == lv[2], lv[2], lv[1]), levels = lv), sev = as.numeric(label),
           value = coalesce(value, 0), label = factor(label, levels = unique(label[order(sev)])))
  p <- WJPr::wjp_bars(data = d, target = "value", grouping = "label", colors = "serie",
                      cvec = stats::setNames(c(st$col$main, st$col$eu), lv), direction = "vertical", stacked = FALSE,
                      order = "sev", width = 0.75, ptheme = wjpr_theme())
  if (bare) return(wjpr_meta(p, base = "wjp_bars"))
  # complemento: wjp_bars fija position = "stack" en geom_col; falta un argumento position = "dodge" (y su ancho)
  dodge <- ggplot2::position_dodge(width = 0.8)
  # complemento: wjp_bars oculta la leyenda (show.legend = FALSE fijo); falta un argumento legend. linewidth: el de
  # geom_col por defecto (0.5 mm) × k; sin contorno solo reduce el cuadro de la leyenda, como en el original
  is_col <- wjpr_wb_is(p, "GeomCol")
  p$layers[is_col] <- lapply(p$layers[is_col], wjpr_wb_layer, position = dodge, show.legend = TRUE,
                             aes_params = list(linewidth = 0.5 * k))
  # complemento: WJPr no tiene líneas de referencia; falta un argumento ref_lines (posición, tipo, color, grosor)
  cut <- 3.5 - min(d$sev) + 1                        # 3.5 en la escala discreta (posición 1 = la severidad mínima)
  p$layers <- c(p$layers, list(ggplot2::geom_vline(xintercept = cut, linetype = "dashed", colour = st$col$text,
                                                   linewidth = 0.4 * k)))
  ext <- 0.8 / 4 + 0.75 / 4                          # media barra más medio desplazamiento: borde de las barras
  p <- suppressMessages(p +
    # complemento: wjp_bars fija limits = c(0, 110) y cortes cada 20 %; falta ylim / breaks / expand_mult
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), expand = ggplot2::expansion(mult = c(0, 0.08))) +
    # complemento: el eje de categorías de WJPr expande 0.6; el original es continuo (5 % del rango de las barras)
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = ext + 0.05 * (max(d$sev) - min(d$sev) + 2 * ext))))
  # complemento: WJPr fija rejilla discontinua #d1cfd1, '% of respondents', textos de 10 pt en #524F4C y sin leyenda;
  # se lleva el tema de F03 (theme_minimal base 11, textos 13, clave 1.2 líneas = 17.28 pt) a pt finales
  p <- p + ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = 13 * k, colour = st$col$text, vjust = 1, margin = ggplot2::margin(t = 4.95 * k)),
    axis.text.y = ggplot2::element_text(size = 13 * k, colour = st$col$text, hjust = 1, margin = ggplot2::margin(r = 4.95 * k)),
    axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(colour = "#eeeeee", linewidth = 0.5 * k, linetype = "solid"),
    legend.position = "top", legend.direction = "horizontal", legend.title = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(), legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 13 * k, colour = st$col$text, margin = ggplot2::margin(l = 0.5 * 13 * k)),
    legend.key.size = grid::unit(17.28 * k, "pt"), legend.key.spacing.x = grid::unit(5.5 * k, "pt"),
    legend.margin = ggplot2::margin(5.5 * k, 5.5 * k, 5.5 * k, 5.5 * k), legend.box.spacing = grid::unit(11 * k, "pt"),
    plot.margin = ggplot2::margin(5.5 * k, 5.5 * k, 5.5 * k, 5.5 * k))
  wjpr_meta(wjpr_font(p), base = "wjp_bars",
            complementos = c("lado a lado (dodge)", "leyenda {country} | EU Average", "línea de corte en 3.5",
                             "escala y en % sin límite fijo", "expansión del eje x como el original",
                             "tema de F03 (rejilla, leyenda, textos 10 pt)", "fuente Inter Tight en capas"))
}

## Bloque wjpr funnel — figuras 8, 34 -------------------------------------------------------------------------------
## WJPr no tiene embudo: wjp_bars horizontal apilada (su position = "stack" fija) con un tramo transparente antes de
## cada barra para centrarla (ancho = value / máximo) y los valores en blanco que da stacked = TRUE en el centro
## (lab_pos = 50); complementos: conectores, tipografía del valor, etiquetas con salto de línea a la derecha y la
## geometría de chart_funnel (barra 40 pt, hueco 18 pt, 4 pt arriba y abajo). Usa wjpr_wb_layer().
## Prueba (galería, 2026-10-23): f1 1.000 / 1.000 (sin complementos 0.645 / 0.650), textos iguales, 0 dudosos.

wjpr_funnel <- function(df, spec, bare = FALSE, ...) {
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
  wjpr_meta(wjpr_font(p), base = "wjp_bars",
            complementos = c("barra centrada (tramo transparente apilado)", "conectores entre etapas", "valor 11 pt semibold cursiva",
                             "escala 0-100 sin expansión", "alto de barra y hueco (expansión discreta)",
                             "etiquetas a la derecha con salto de línea", "tema sin eje ni rejilla", "fuente Inter Tight en capas"))
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

wjpr_cards <- function(df, spec, complementos = getOption("wjpr.complementos", TRUE), st = ST_CARDS) {
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
## diagonal y en las N/A: celda blanca), ordenadas por su valor mínimo. Los complementos lo llevan al diseño de
## run_F01 (src/hungary/F01.R:24-41: theme_minimal 'inter', bandas COL_BG / COL_LIGHT / COL_MAIN, texto blanco en la
## banda alta, columnas arriba a 40°, títulos bold cursiva main, leyenda arriba), que se dibujó en un lienzo
## k = base_pt / 10 veces mayor (render_figure.R:29-38; 06_export.R): aquí cada tamaño va dividido por k porque
## wjpr_render() exporta al tamaño final. Los títulos de eje son texto fijo del diseño (argumentos). La galería
## cuenta text2 (la banda) como texto esperado: se dibuja como color y leyenda, no como texto en cada celda.
wjpr_matrix <- function(df, spec, complementos = getOption("wjpr.complementos", TRUE),
                        x_title = "Among people who experienced problems with:", y_title = "People who also experienced:",
                        cols_band = c(STD$col$track, STD$col$eu, STD$col$main)) {
  wjpr_require()
  df <- df[order(df$order), ]
  rows <- unique(df$grouping); cols <- unique(df$label)
  has_band <- nzchar(coalesce(df$text2, ""))
  bands <- df[has_band, ] %>% group_by(text2) %>% summarise(m = min(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(m) %>% pull(text2)
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
  if (!isTRUE(complementos)) return(wjpr_meta(p, base = base))

  k <- (spec$base_pt %||% 10) / 10
  if (is.na(k)) k <- 1
  comp <- character()
  # complemento: WJPr solo trae una paleta categórica (wjp_palette) y el gris "#e5e8e8"; no tiene la escala ordinal
  # del reporte (COL_BG #e5e8e8, COL_LIGHT #a8a8c8, COL_MAIN #575796)
  p <- suppressMessages(p + scale_fill_manual(values = setNames(cols_band[seq_along(bands)], bands), breaks = bands,
                                              na.value = "white"))
  comp <- c(comp, "escala ordinal del reporte (3 bandas)")
  # complemento: celdas separadas por un borde blanco y texto blanco sobre la banda alta (#1A1A1A en el resto); WJPr
  # no tiene argumento de borde ni de color de texto según el fondo
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
  p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 axis.text.x.top = element_text(angle = 40, hjust = 0, vjust = 0, size = 11 / k, colour = STD$col$text),
                 axis.text.y = element_text(size = 11 / k, colour = STD$col$text),
                 axis.title.x.top = element_text(face = "bold.italic", colour = STD$col$main, hjust = 0, size = 12 / k,
                                                 margin = margin(b = 11 / 4 / k)),
                 axis.title.y = element_text(face = "bold.italic", colour = STD$col$main, size = 12 / k,
                                             margin = margin(r = 11 / 4 / k)),
                 legend.position = "top", legend.title = element_blank(), legend.text = element_text(size = 11 / k),
                 plot.margin = margin(10 / k, 45 / k, 10 / k, 10 / k), plot.background = element_rect(fill = "white", colour = NA))
  comp <- c(comp, "tema del diseño (ejes, títulos, leyenda, márgenes)")
  # complemento: WJPr fija "Lato Full" en capas y tema, sin argumento para cambiarla
  p <- wjpr_font(p); comp <- c(comp, "fuente Inter Tight (wjpr_font)")
  wjpr_meta(p, base = base, complementos = comp, compara = "text")  # text2 = banda (color)
}

## Bloque wjpr scatter — figura 3 ----------------------------------------------------------------------------------
## WJPr 1.1.0 no tiene diagrama de dispersión x-y (wjp_dots es un diagrama de puntos por categoría y wjp_lines une los
## puntos con líneas): la base es geom_smooth(lm) + geom_point + ggrepel::geom_text_repel (como wjp_lines(repel =
## TRUE): "Lato Full" bold 3.514598) + facet_wrap(grouping) con WJP_theme() y wjp_palette()[1]; x = df$value
## (prevalencia %), y = df$value2 (severidad 0-10; NA = sin punto), facetas en el orden de df$order. Los complementos lo
## llevan al diseño de run_F02 (src/hungary/F02.R:33-50: theme_minimal 'inter', destacadas en main y el resto
## #c4c4c4, punto hueco si n < 30, rótulos solo de las destacadas con seed 7, ejes 0-50 % y 4-8, rejilla #eeeeee,
## facetas bold cursiva main), dibujado en un lienzo k = base_pt / 10 mayor: aquí cada tamaño va dividido por k.
## highlight = categorías destacadas (top_cats de F02.R:36: diseño de la figura). La galería cuenta los rótulos de
## categoría como texto sobrante: el contrato del scatter no imprime valores (text = "").
wjpr_scatter <- function(df, spec, complementos = getOption("wjpr.complementos", TRUE),
                         highlight = c("Public Services", "Consumer", "Community", "Money & Debt", "Housing", "Family"),
                         x_title = "Prevalence", y_title = "Mean seriousness (0-10)") {
  wjpr_require()
  df <- df[order(df$order), ]
  small <- if ("n" %in% names(df)) coalesce(df$n < 30, FALSE) else rep(FALSE, nrow(df))   # sin n: ningún punto hueco
  d <- df %>% mutate(facet = factor(grouping, levels = unique(grouping)), destacada = label %in% highlight,
                     hueco = small | is.na(value2))
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
  if (!isTRUE(complementos)) return(wjpr_meta(p, base = base))

  k <- (spec$base_pt %||% 10) / 10
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
  p$layers[[3]] <- ggrepel::geom_text_repel(data = d %>% filter(destacada), aes(label = label), family = fam,
                                            size = 4 / k, colour = STD$col$main, seed = 7, min.segment.length = 0.3,
                                            na.rm = TRUE)
  comp <- c(comp, "rótulos de las destacadas (main, 4 mm, seed 7)")
  # complemento: ejes del diseño (0-50 % con sufijo, 4-8 cada 1); WJPr fija sus ejes dentro de cada función
  p <- p + scale_x_continuous(limits = c(0, 50), labels = function(x) paste0(x, "%")) +
    scale_y_continuous(limits = c(4, 8), breaks = 4:8)
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
  wjpr_meta(p, base = base, complementos = comp)
}

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
wjpr_upset <- function(df, spec, complementos = getOption("wjpr.complementos", TRUE),
                       y_title = "% of people within\nthe justice gap", width_ratio = 0.22) {
  wjpr_require()
  df <- df[order(df$order), ]
  inter <- df[df$kind == "row", ]; sets <- df[df$kind == "set", ]
  memb <- lapply(strsplit(inter$label, " + ", fixed = TRUE), trimws)
  bad <- setdiff(unlist(memb), sets$label)
  if (length(bad)) stop("wjpr_upset: conjuntos sin fila set en df: ", paste(unique(bad), collapse = ", "), call. = FALSE)
  k <- (spec$base_pt %||% 10) / 10
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
## layout de df (x, y de los nodos: network_graph.R:148-199, sin renormalizar). Los complementos lo llevan al diseño
## de charts5.py:196-226 network() (la publicada): lienzo = caja de los nodos ± (radio 35 + margen 6) en unidades del
## SVG fuente, que mide 518.03 unidades por unidad de layout (network_graph.R; medido en la publicada), escalado al
## ancho de la figura (k_s = W / (518.03·Δx + 82)); aristas #5D6D7E con ancho 0.8 + 3.4·t^0.8 (t = lift normalizado),
## opacidad min(0.85, (lift − 1)·0.35 + 0.2) (network_graph.R:209-210), de la más fina a la más gruesa; nodos de radio
## 35·k_s con relleno por centralidad (suma de lifts del nodo, normalizada: #A8A8C8 → #39396B, network_graph.R:117-141)
## y borde #2C3E50 1.2·k_s; rótulos bold 10.5·k_s (nombre con los cortes de network_graph.R:225-231 + text) en blanco
## si la luminancia del relleno < 0.45, interlineado 1.15. Los nodos son polígonos de 72 lados (radio exacto en datos).
## La galería cuenta los nombres de los nodos como texto sobrante: el contrato solo pone "(40%)" en text.

# Polígonos de círculos (centro x, y y radio r en unidades de datos); id = índice del círculo
wjpr_circle_df <- function(x, y, r, n = 72) {
  a <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
  bind_rows(lapply(seq_along(x), function(i) tibble(x = x[i] + r * cos(a), y = y[i] + r * sin(a), id = i)))
}

wjpr_network <- function(df, spec, complementos = getOption("wjpr.complementos", TRUE),
                         name_lines = c("Public Services" = "Public\nServices", "Citizenship & ID" = "Citizenship\n& ID",
                                        "Money & Debt" = "Money &\nDebt", "Law Enforcement" = "Law\nEnforcement"),
                         src_scale = 518.03) {
  wjpr_require()
  df <- df[order(df$order), ]
  nodes <- df[df$kind == "node", ]; edges <- df[df$kind == "edge", ]
  ab <- strsplit(edges$label, "|", fixed = TRUE)
  e <- tibble(from = vapply(ab, `[`, "", 1), to = vapply(ab, `[`, "", 2), lift = edges$value) %>%
    mutate(i = match(from, nodes$label), j = match(to, nodes$label))
  if (anyNA(c(e$i, e$j))) stop("wjpr_network: aristas con nodos que no están en df", call. = FALSE)
  e <- e %>% mutate(x = nodes$x[i], y = nodes$y[i], xend = nodes$x[j], yend = nodes$y[j])
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
  if (!isTRUE(complementos)) return(wjpr_meta(p, base = base))

  comp <- character()
  W <- spec$w_in * 72
  dx <- diff(range(nodes$x)); ks <- W / (src_scale * dx + 82)      # pt de la figura por unidad del SVG fuente
  b <- ks * src_scale                                               # pt por unidad de layout
  r <- 35 / src_scale; pad <- (35 + 6) / src_scale                  # radio y margen en unidades de layout
  lw <- function(pt) pt / (0.75 * ggplot2::.pt)                     # ancho en pt → linewidth de ggplot
  # complemento: aristas del diseño (ancho 0.8-4.2 con gamma 0.8 según el lift, opacidad según el lift, #5D6D7E,
  # extremos redondeados, de la más fina a la más gruesa); WJPr no tiene capa de aristas
  e <- e %>% mutate(t = (lift - min(lift)) / (max(lift) - min(lift)), w = 0.8 + (4.2 - 0.8) * t^0.8,
                    alpha = pmin(0.85, (lift - 1) * 0.35 + 0.2)) %>% arrange(w)
  p$layers[[1]] <- geom_segment(data = e, aes(x = x, y = y, xend = xend, yend = yend, linewidth = lw(w * ks), alpha = alpha),
                                colour = "#5D6D7E", lineend = "round")
  p <- suppressMessages(p + scale_linewidth_identity() + scale_alpha_identity())
  comp <- c(comp, "aristas: ancho y opacidad según el lift")
  # complemento: nodos de radio fijo en datos (polígonos) con relleno por centralidad (#A8A8C8 → #39396B) y borde
  # #2C3E50; WJPr no tiene escala de centralidad ni el degradado del reporte
  strength <- vapply(seq_len(nrow(nodes)), function(i) sum(e$lift[e$i == i | e$j == i]), 0)
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
  wjpr_meta(p, base = base, complementos = comp)
}

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

# Banda entre (xa, [ya0, ya1]) y (xb, [yb0, yb1]) con las curvas del SVG (M … C xm ya0 xm yb0 xb yb0 L … C … Z)
wjpr_bezier_band <- function(xa, ya0, ya1, xb, yb0, yb1, n = 40) {
  t <- seq(0, 1, length.out = n); xm <- (xa + xb) / 2
  bez <- function(p0, p1, p2, p3) (1 - t)^3 * p0 + 3 * (1 - t)^2 * t * p1 + 3 * (1 - t) * t^2 * p2 + t^3 * p3
  tibble(x = c(bez(xa, xm, xm, xb), bez(xb, xm, xm, xa)), y = c(bez(ya0, ya0, yb0, yb0), bez(yb1, yb1, ya1, ya1)))
}

wjpr_sankey <- function(df, spec, complementos = getOption("wjpr.complementos", TRUE), which = NULL, st = ST_SANKEY) {
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
  if (!isTRUE(complementos)) return(wjpr_meta(p, base = base))

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
  wjpr_meta(p, base = base, complementos = comp)
}


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
