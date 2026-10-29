## 06_export.R — etapa figures: tabla → dibujo → SVG/PNG → estado por figura -----------------------------------
## Origen: src/hungary/finales/build_finales.R:131-152 (chrome_png: captura con Chrome headless y recorte a
## píxeles exactos); render_figure.R:80 (ggsave con svglite); standard_figures.R:438-442 (outline_checked: recuento
## de textos dudosos al contornear). png_props (build_finales.R:154-157) no se porta: 07 lee las dimensiones del PNG
## con png_dims() y 08 con read_png_rgb().
## El flujo por figura (run_figures, validate_df, save_figure, figures_status.csv) es nuevo para src_reports.
## Métodos de dibujo (columna metodo del manifiesto): wjpr (el motor por defecto desde 2026-10-23: plot() devuelve el
## ggplot de wjpr_<tipo>(), R/09_wjpr.R, que se exporta como las ggplot con k = 1 y cuyos textos dibujados se comparan
## con df; si el adaptador expone lo dibujado por fila, attr(p, "wjpr")$drawn, va a drawn.csv como en svg_r), svg_r
## (respaldo: chart_out de chart_<tipo>(), R/05_charts.R), ggplot (lienzo × base_pt / text_pt) y static.
## Usa de 00_setup.R: load_figure(), figure_file(), figure_stem(), log_msg(), first_line(), check_method_deps(); de
## 01_load.R: secs(), rel_path(), ctx_key(); de 04_svg.R: svg_size(), outline_svg_text(), set_size(),
## count_live_text(); de 09_wjpr.R: wjpr_require(), wjpr_build(), wjpr_complementos(),
## wjpr_texts(), wjpr_text_diff(), wjpr_drawn(), wjpr_drawn_complete(), wjpr_norm_text().
## drawn.csv (revisión 4, contrato extendido): order, kind, grouping, label, frac, frac2, text, text2 (WJPR_DRAWN_COLS)
## + label_drawn, grouping_drawn, val, val2 (WJPR_DRAWN_OPT; NA = no expuesto), en todos los métodos.
## Nada se ejecuta al cargar. Nunca se imprimen filas de microdatos: df es ya un agregado y solo se escribe a CSV.

# Columnas fijas de df (CONTRATOS §4.4), extras permitidas y columnas de log/figures_status.csv (§4.6 + motivo,
# decisión 2 de la revisión: primera línea del error o del motivo del estado; "" si ok)
DF_COLS     <- c("order", "kind", "grouping", "label", "value", "value2", "text", "text2")
DF_EXTRAS   <- c("x", "y", "n", "lo", "hi", "series")
# + wjpr_build (wjpr_build() de la build que dibujó: "1.1.0@382aee9"; "1.1.0@local" desde un clon), wjpr_complementos
# (número de complementos de attr(p, "wjpr")), wjpr_textos ("iguales", "sin textos que comparar" o lo que falta y
# sobra; verify lo muestra) y wjpr_drawn (filas de lo dibujado por fila que expuso el adaptador y que van a drawn.csv;
# NA si no lo expone: verify sabe así si debe compararlo): NA en las figuras que no son metodo wjpr
STATUS_COLS <- c("fig", "slug", "run_id", "ctx_key", "md5_table", "md5_drawn", "md5_svg", "md5_png", "n_text",
                 "width_in", "height_in", "png_px", "estado", "segundos", "fecha", "motivo",
                 "wjpr_build", "wjpr_complementos", "wjpr_textos", "wjpr_drawn")


## Rutas y utilidades --------------------------------------------------------------------------------------------

# Rutas de salida de una figura, todas bajo output/<slug>/ (figures/, tables/, log/)
figure_paths <- function(spec, cfg) {
  stem <- figure_stem(spec); out <- cfg$paths$output
  list(
    stem   = stem,
    table  = file.path(out, "tables", paste0(stem, ".csv")),
    drawn  = file.path(out, "tables", paste0(stem, ".drawn.csv")),
    svg    = file.path(out, "figures", paste0(stem, ".svg")),
    png    = file.path(out, "figures", paste0(stem, ".png")),
    status = file.path(out, "log", "figures_status.csv")
  )
}

# drawn vacío con encabezado (figuras ggplot y static, y wjpr cuyo adaptador no lo expone: no hay barras medidas).
# corregido (revisión 4): con las columnas opcionales del contrato extendido (WJPR_DRAWN_OPT, 09_wjpr.R), para que
# verify distinga un drawn.csv escrito con este contrato de uno anterior
empty_drawn <- function() {
  tibble(order = integer(), kind = character(), grouping = character(), label = character(),
         frac = numeric(), frac2 = numeric(), text = character(), text2 = character(),
         label_drawn = character(), grouping_drawn = character(), val = numeric(), val2 = numeric())
}

fmt_in <- function(x) formatC(x, format = "f", digits = 3)
md5_or_na <- function(f) if (!is.na(f) && file.exists(f)) unname(tools::md5sum(f)) else NA_character_


## Contrato de df -------------------------------------------------------------------------------------------------

# Comprueba lo que main.R escribe en tables/ y verify compara (§4.4): las 8 columnas fijas en orden, solo extras
# permitidas, tipos (chr sin NA; value/value2 numéricas, NA = suprimido) y order = 1..n sin repetir.
# Devuelve df invisible; error legible si no cumple (la figura queda en estado "error").
validate_df <- function(df) {
  fail <- function(...) stop("df no cumple el contrato (§4.4): ", ..., call. = FALSE)
  if (!is.data.frame(df)) fail("data(ctx) debe devolver un tibble, no un objeto de clase ", class(df)[1])
  if (nrow(df) == 0) fail("sin filas")
  nm <- names(df)
  if (length(nm) < length(DF_COLS) || !identical(nm[seq_along(DF_COLS)], DF_COLS)) {
    fail("las primeras columnas deben ser ", paste(DF_COLS, collapse = ", "), "; se obtuvo ", paste(nm, collapse = ", "))
  }
  extra <- setdiff(nm, c(DF_COLS, DF_EXTRAS))
  if (length(extra) > 0) fail("columnas no permitidas: ", paste(extra, collapse = ", "), " (extras: ", paste(DF_EXTRAS, collapse = ", "), ")")
  for (v in c("kind", "grouping", "label", "text", "text2")) {
    if (!is.character(df[[v]])) fail("'", v, "' debe ser character")
    if (anyNA(df[[v]])) fail("'", v, "' tiene NA (use \"\" cuando no aplica)")
  }
  for (v in c("value", "value2")) {
    x <- df[[v]]
    if (!(is.numeric(x) || (is.logical(x) && all(is.na(x))))) fail("'", v, "' debe ser numérica (NA si está suprimido)")
  }
  o <- df$order
  if (!is.numeric(o) || anyNA(o) || any(o != round(o))) fail("'order' debe ser entero sin NA")
  if (anyDuplicated(o) || !setequal(o, seq_len(nrow(df)))) fail("'order' debe ser 1..n sin repetir (n = ", nrow(df), ")")
  invisible(df)
}

# Texto dibujado distinto del de df (§4.5: drawn$text/text2 = df$text/df$text2), emparejado por order y no por
# posición: fila de df con texto que drawn no tiene o dibuja distinto, y fila de drawn con un texto que df no
# declara. Devuelve tibble(campo, label, df, drawn), sin filas si coinciden.
# corregido (revisión 4): también el nombre impreso (contrato extendido, WJPR_DRAWN_OPT): label_drawn frente a
# df$label y grouping_drawn frente a df$grouping, exactos tras normalizar espacios y saltos de línea
# (wjpr_norm_text); NA = el adaptador no lo expone y no se compara. Antes dos etiquetas intercambiadas o un
# encabezado cambiado pasaban: label y grouping de drawn salen de la posición, no de lo impreso.
drawn_text_diff <- function(drawn, df) {
  chr <- function(v) coalesce(as.character(v), "")
  i <- match(df$order, drawn$order)
  pick <- function(v) ifelse(is.na(i), "", chr(v)[i])
  pick_na <- function(v) if (is.null(v)) rep(NA_character_, length(i)) else ifelse(is.na(i), NA_character_, as.character(v)[i])
  d1 <- pick(drawn$text); d2 <- pick(drawn$text2)
  l1 <- pick_na(drawn[["label_drawn"]]); g1 <- pick_na(drawn[["grouping_drawn"]])
  bad_l <- !is.na(l1) & wjpr_norm_text(l1) != wjpr_norm_text(df$label)
  bad_g <- !is.na(g1) & wjpr_norm_text(g1) != wjpr_norm_text(df$grouping)
  ld <- if (is.null(drawn[["label_drawn"]])) rep(NA_character_, nrow(drawn)) else as.character(drawn[["label_drawn"]])
  extra <- !(drawn$order %in% df$order) & (nzchar(chr(drawn$text)) | nzchar(chr(drawn$text2)) | (!is.na(ld) & nzchar(ld)))
  bind_rows(
    tibble(campo = "text", label = df$label, df = df$text, drawn = d1)[d1 != df$text, ],
    tibble(campo = "text2", label = df$label, df = df$text2, drawn = d2)[d2 != df$text2, ],
    tibble(campo = "label_drawn", label = df$label, df = df$label, drawn = l1)[bad_l, ],
    tibble(campo = "grouping_drawn", label = df$label, df = df$grouping, drawn = g1)[bad_g, ],
    tibble(campo = "text", label = chr(drawn$label[extra]), df = "",
           drawn = paste0(chr(drawn$text[extra]), ifelse(!is.na(ld[extra]) & nzchar(ld[extra]), paste0(" [", ld[extra], "]"), "")))
  )
}

# Valor dibujado distinto del de df (contrato extendido, revisión 4): val/val2 de lo dibujado por fila (la geometría
# decodificada a las unidades de df$value/value2) frente a df$value/df$value2, emparejados por order, con tolerancia
# max(1e-6, 0.005 · máx |value| de la figura) (y la de value2 con máx |value2|). val NA no se compara (el adaptador no
# lo codifica o no lo expone); un val finito donde df trae NA (una barra donde el valor está suprimido) es error, como
# un val finito en una posición sin fila en df. Devuelve tibble(campo, label, df, drawn, tol), sin filas si coinciden.
drawn_value_diff <- function(drawn, df) {
  num <- function(v) if (is.null(v)) rep(NA_real_, nrow(drawn)) else suppressWarnings(as.numeric(v))
  tol_of <- function(v) { m <- suppressWarnings(max(abs(v), na.rm = TRUE)); max(1e-6, if (is.finite(m)) 0.005 * m else 0) }
  i <- match(df$order, drawn$order)
  one <- function(campo, dv, ref) {
    ref <- suppressWarnings(as.numeric(ref)); dvi <- ifelse(is.na(i), NA_real_, dv[i]); tol <- tol_of(ref)
    bad <- is.finite(dvi) & (is.na(ref) | abs(dvi - ref) > tol + 1e-12)
    extra <- !(drawn$order %in% df$order) & is.finite(dv)
    bind_rows(
      tibble(campo = campo, label = df$label[bad], df = as.character(formatC(ref[bad], digits = 6, format = "fg")),
             drawn = as.character(formatC(dvi[bad], digits = 6, format = "fg")), tol = tol),
      tibble(campo = campo, label = as.character(drawn$label[extra]), df = "", drawn = formatC(dv[extra], digits = 6, format = "fg"), tol = tol))
  }
  out <- bind_rows(one("val", num(drawn[["val"]]), df$value), one("val2", num(drawn[["val2"]]), df$value2))
  mutate(out, df = trimws(df), drawn = trimws(drawn))
}

# Textos de un ggplot de wjpr_<tipo>() frente a df; tools/wjpr_gallery.R usa esta misma función (una sola regla):
# faltan = textos de las columnas attr(p, "wjpr")$compara (por defecto text y text2; p. ej. en la matriz text2 es la
# banda de color, no un texto) y rótulos del diseño que el adaptador declara (attr(p, "wjpr")$rotulos) que no se
# dibujan; sobran = valores dibujados (números, %, "(40%)", "+16 points", "N/A", "–") que df no trae, o textos
# esperados dibujados más veces. Los demás rótulos fijos (leyendas, encabezados, ejes) no cuentan. Es un multiconjunto,
# sin la fila de cada texto: la comparación por fila la hace drawn_text_diff() sobre lo dibujado por fila
# (wjpr_drawn), en los adaptadores que lo exponen. b = ggplot_build(p) si ya está construido. Devuelve list(faltan,
# sobran, n_esperados, n_comparados, resumen = "iguales" | "sin textos que comparar" | "faltan n (…) · sobran m (…)").
# corregido (V3-07): sin textos esperados ni valores dibujados (figs 3 y 7) el resumen era "iguales" y verify lo daba
# por comprobado; ahora es "sin textos que comparar" (verify: suprimido con nota)
WJPR_VALUE_RE <- "^(\\(?[−+±-]?[0-9]+([.,][0-9]+)?%?\\)?( points)?|N/A|–)$"
drawn_text_diff_wjpr <- function(p, df, b = NULL) {
  meta <- attr(p, "wjpr")
  cols <- intersect(meta$compara %||% c("text", "text2"), names(df))
  expected <- c(unlist(df[cols], use.names = FALSE), meta$rotulos %||% character())
  expected <- expected[!is.na(expected) & nzchar(expected)]
  drawn <- wjpr_texts(p, b)
  used <- drawn[grepl(WJPR_VALUE_RE, drawn) | drawn %in% expected]
  td <- wjpr_text_diff(used, expected)
  td$n_esperados <- length(expected); td$n_comparados <- length(used)
  ej <- function(x) paste(head(unique(x), 3), collapse = " ")
  td$resumen <- if (length(td$faltan) + length(td$sobran) > 0) {
    paste0("faltan ", length(td$faltan), " (", ej(td$faltan), ") · sobran ", length(td$sobran), " (", ej(td$sobran), ")")
  } else if (length(expected) == 0) "sin textos que comparar" else "iguales"
  td
}

# Lo dibujado por fila de WJPr (order = posición visual) con el order de la fila de df que tiene su misma clave
# (kind, grouping, label; las repetidas por orden de aparición): drawn_text_diff() compara así cada texto con el de su
# fila. Una posición sin fila en df queda con order NA (sobrante si imprime algo); el orden visual lo mira verify.
wjpr_drawn_by_key <- function(drawn, df) {
  key <- function(x) {                                   # clave + n.º de repetición, contado en el orden de order
    k <- paste(x$kind, x$grouping, x$label, sep = "\r"); o <- order(x$order); dup <- integer(length(k))
    dup[o] <- stats::ave(seq_along(o), k[o], FUN = seq_along)
    paste(k, dup, sep = "\r")
  }
  drawn$order <- df$order[match(key(drawn), key(df))]
  drawn
}


## Rasterización con Chrome ----------------------------------------------------------------------------------------

# URL file:// de una ruta absoluta con cada tramo codificado: un '#', '%', '?' o un carácter no ASCII en TMPDIR
# rompía la URL sin codificar, y URLencode(reserved = FALSE) no codifica '#' ni toca una ruta que ya tenga "%xx"
file_url <- function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  paste0("file://", paste(vapply(parts, utils::URLencode, "", reserved = TRUE, repeated = TRUE), collapse = "/"))
}

# origen: build_finales.R:131-152 (chrome_png). Port endurecido: el SVG se abre directo con file:// desde una copia
# en tempdir() (ruta sin espacios; el SVG ya trae width/height en pulgadas, que Chrome convierte a px CSS a 96 dpi),
# perfil de usuario temporal (no choca con un Chrome abierto), --no-first-run, timeout de 60 s, canal alfa compuesto
# sobre blanco y recorte a round(w_in·dpi) × round(h_in·dpi) con la resolución guardada en el PNG. La ventana lleva
# +2 px CSS de margen como el original (build_finales.R:140). Devuelve c(ancho_px, alto_px); error si Chrome falla,
# no termina o entrega una captura menor que la pedida.
chrome_png <- function(svg, png, w_in, h_in, dpi = 300, chrome = find_chrome()) {
  if (is.na(chrome) || !file.exists(chrome)) stop("Chrome no disponible para rasterizar ", basename(svg), call. = FALSE)
  chrome <- path.expand(chrome)                     # file.exists() expande "~", pero el shell no lo hace tras shQuote()
  if (!file.exists(svg)) stop("No existe ", svg, call. = FALSE)
  if (anyNA(c(w_in, h_in)) || w_in <= 0 || h_in <= 0) stop("chrome_png: w_in/h_in deben ser positivos", call. = FALSE)
  work <- tempfile("chrome_png_"); dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  tmp_svg <- file.path(work, "figura.svg"); shot <- file.path(work, "captura.png"); perfil <- file.path(work, "perfil")
  if (!file.copy(svg, tmp_svg)) stop("No se pudo copiar ", svg, " a ", work, call. = FALSE)
  css_w <- w_in * 96; css_h <- h_in * 96
  args <- c("--headless=new", "--disable-gpu", "--hide-scrollbars", "--no-first-run", "--force-color-profile=srgb",
            paste0("--user-data-dir=", perfil),
            sprintf("--force-device-scale-factor=%.6f", dpi / 96),
            sprintf("--window-size=%d,%d", ceiling(css_w) + 2L, ceiling(css_h) + 2L),
            paste0("--screenshot=", shot), file_url(normalizePath(tmp_svg)))
  # único uso de Sys.info() en la carpeta: Chrome como root (contenedores) exige --no-sandbox (CONTRATOS §5.E)
  if (identical(unname(Sys.info()[["effective_user"]]), "root")) args <- c(args, "--no-sandbox")
  # exec: Chrome reemplaza al sh intermedio de system2(), así una muerte por señal no deja el aviso del sh
  # ("sh: line 1: … Segmentation fault") en la consola, fuera de la tabla; el timeout (124) funciona igual
  status <- suppressWarnings(system2("exec", c(shQuote(chrome), shQuote(args)), stdout = FALSE, stderr = FALSE, timeout = 60))
  if (identical(as.integer(status), 124L)) stop("Chrome superó los 60 s rasterizando ", basename(svg), call. = FALSE)
  if (!file.exists(shot)) stop("Chrome no pudo rasterizar ", basename(svg), " (código ", status, ")", call. = FALSE)
  img <- png::readPNG(shot)
  if (length(dim(img)) == 2) img <- array(img, c(dim(img), 3))      # escala de grises → RGB
  if (dim(img)[3] == 4) {                                             # alfa compuesto sobre blanco
    a <- as.vector(img[, , 4])
    img <- img[, , 1:3, drop = FALSE] * a + (1 - a)
  }
  # Recorte al tamaño exacto en píxeles de la figura y resolución en el PNG (build_finales.R:146-149). Una captura
  # menor que lo pedido (Chrome que ignora --force-device-scale-factor o recorta la ventana) es un fallo: recortarla
  # con min() dejaba un PNG de otro tamaño con estado "ok"
  px_w <- round(w_in * dpi); px_h <- round(h_in * dpi)
  if (dim(img)[1] < px_h || dim(img)[2] < px_w) {
    stop(sprintf("Chrome capturó %d×%d px; se esperaban al menos %d×%d", dim(img)[2], dim(img)[1], px_w, px_h), call. = FALSE)
  }
  img <- img[seq_len(px_h), seq_len(px_w), 1:3, drop = FALSE]
  png::writePNG(img, png, dpi = dpi)
  c(dim(img)[2], dim(img)[1])
}


## Escritura del SVG -----------------------------------------------------------------------------------------------

# Filas dudosas del registro de outline_svg_text() (columna doubtful); sin filas si no hay registro
doubtful_texts <- function(lg) {
  if (!is.data.frame(lg) || !"doubtful" %in% names(lg)) return(tibble(text = character()))
  lg[lg$doubtful %in% TRUE, , drop = FALSE]
}

# ggplot → SVG: svglite sobre un lienzo escalado por k = base_pt / text_pt (al fijar el ancho final en w_in el
# texto queda a text_pt), texto en trazos y tamaño final en pulgadas. origen: render_figure.R:80 (ggsave con
# svglite) + svg_text.R:109 (outline_svg_text) + set_size (04_svg.R). El SVG se arma en tempdir() y se copia.
# Devuelve (invisible) el registro de outline_svg_text(): como outline_checked (standard_figures.R:438-442), los
# textos dudosos (medidos con otra familia que Inter o con textLength fuera de ±3 %) se anotan en el log y
# save_figure da estado "texto"; antes el registro se descartaba y la figura quedaba "ok".
# metodo wjpr: k = 1 sin base_pt (WJPr y sus complementos dan los tamaños en pt finales: 3.514598 mm = 10 pt) y el alto
# del manifiesto también en set_size(), como wjpr_render() de la galería (09_wjpr.R): el mismo SVG que mide su f1.
save_ggplot_svg <- function(plot, spec, cfg, svg) {
  if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_auto(FALSE)   # svglite debe emitir <text>
  wjpr <- identical(spec$metodo, "wjpr")
  if (!wjpr && is.na(spec$base_pt)) stop("metodo ggplot requiere base_pt en el manifiesto (fig ", spec$fig, ")", call. = FALSE)
  k <- if (wjpr) 1 else spec$base_pt / cfg$layout$text_pt
  tmp <- tempfile(fileext = ".svg")
  ggplot2::ggsave(tmp, plot, device = svglite::svglite, width = spec$w_in * k, height = spec$h_in * k, units = "in")
  lg <- outline_svg_text(tmp)
  dud <- doubtful_texts(lg)
  if (nrow(dud) > 0) {
    fam <- if ("family" %in% names(dud)) paste(unique(coalesce(dud$family, "sin familia")), collapse = "/") else "?"
    log_msg(sprintf("fig %02d", spec$fig), ": ", nrow(dud), " de ", nrow(lg), " textos dudosos al contornear (familia ",
            fam, "): ", paste(head(dud$text, 8), collapse = " | "), if (nrow(dud) > 8) " | …" else "")
  }
  set_size(tmp, spec$w_in, if (wjpr) spec$h_in)
  if (!file.copy(tmp, svg, overwrite = TRUE)) stop("No se pudo mover el SVG a ", svg, call. = FALSE)
  unlink(tmp)
  invisible(lg)
}

# Escribe figures/Figure_NN_slug.svg (y .png) y tables/Figure_NN_slug.drawn.csv a partir de lo que devolvió
# plot(): ggplot (wjpr o ggplot), chart_out (svg_r) o nada (static: se copia static/Figure_NN_slug.svg). Luego comprueba
# el archivo, con esta precedencia de estados: ancho exacto (±0.001 in) y alto ± cfg$verify$size_tol_in → "tamaño";
# <text> vivos, texto dibujado ≠ df$text/text2 (svg_r: drawn.csv; wjpr: drawn_text_diff_wjpr y, si el adaptador expone
# lo dibujado por fila, también fila a fila con drawn.csv, con el nombre impreso: label_drawn/grouping_drawn ≠
# df$label/grouping) o textos dudosos al contornear (ggplot y wjpr) → "texto"; valor que codifica la geometría
# (val/val2 de lo dibujado por fila) ≠ df$value/value2 fuera de max(1e-6, 0.005 · máx |value|), o val finito donde df
# trae NA, o una marca fija del diseño ausente (attr(p, "wjpr")$marcas, la línea de corte de la 7) → "dibujo"; PNG (solo si png && hay Chrome) fallido → "png". El dibujo no se corrige.
# motivo junta todos los problemas, el del estado primero. Devuelve list(estado, motivo, svg, png, width_in,
# height_in, n_text, png_px, drawn, wjpr); wjpr = list(build, complementos, textos, drawn = filas de lo dibujado por
# fila o NA) en metodo wjpr, NULL en el resto.
save_figure <- function(out, df, spec, cfg, png = TRUE) {
  p <- figure_paths(spec, cfg); tag <- sprintf("fig %02d", spec$fig)
  if (anyNA(c(spec$w_in, spec$h_in))) stop("w_in/h_in faltan en el manifiesto (fig ", spec$fig, ")", call. = FALSE)
  dif <- NULL; vdif <- NULL; mk <- character(); talla <- NA_character_; dud <- NULL; wj <- NULL; wt <- NULL
  if (identical(spec$metodo, "static")) {
    src <- file.path(cfg$paths$root, "static", paste0(p$stem, ".svg"))
    if (!file.exists(src)) stop("No existe static/", basename(src), " (metodo static)", call. = FALSE)
    if (!file.copy(src, p$svg, overwrite = TRUE)) stop("No se pudo copiar ", src, call. = FALSE)
    drawn <- empty_drawn()
  } else if (inherits(out, "chart_out")) {
    if (!identical(spec$metodo, "svg_r")) {
      stop("plot() devolvió chart_out pero el manifiesto dice metodo '", spec$metodo, "'",
           if (identical(spec$metodo, "wjpr")) {
             " (con WJPr: plot = function(df, spec) wjpr_<tipo>(df, spec); chart_<tipo>() es el respaldo, metodo svg_r)"
           }, call. = FALSE)
    }
    if (!is.data.frame(out$drawn)) stop("chart_out sin tabla drawn (§4.5)", call. = FALSE)
    con <- file(p$svg, open = "w", encoding = "UTF-8"); writeLines(out$svg, con); close(con)
    drawn <- out$drawn
    # el texto dibujado que no es el de df ya no es solo un aviso: la figura queda en estado "texto" (abajo)
    dif <- drawn_text_diff(drawn, df)
    if (nrow(dif) > 0) {
      log_msg(tag, ": texto dibujado ≠ df en ", nrow(dif), if (nrow(dif) == 1) " fila: " else " filas: ",
              paste(head(sprintf("%s %s: %s→%s", dif$campo, dif$label, dif$df, dif$drawn), 5), collapse = "; "))
    }
  } else if (inherits(out, "ggplot")) {
    if (!spec$metodo %in% c("ggplot", "wjpr")) {
      stop("plot() devolvió ggplot pero el manifiesto dice metodo '", spec$metodo, "'", call. = FALSE)
    }
    dud <- doubtful_texts(save_ggplot_svg(out, spec, cfg, p$svg))
    drawn <- empty_drawn()
    if (identical(spec$metodo, "wjpr")) {
      meta <- attr(out, "wjpr")
      if (is.null(meta)) log_msg(tag, ": el ggplot no trae attr(, \"wjpr\") (¿no sale de wjpr_<tipo>()?): complementos sin contar")
      b <- suppressWarnings(ggplot2::ggplot_build(out))     # una construcción para los textos y lo dibujado por fila
      wt <- drawn_text_diff_wjpr(out, df, b)
      # corregido (V3-02): lo dibujado por fila (attr(p, "wjpr")$drawn; 8 adaptadores, 29 figuras) va a drawn.csv
      # como el de svg_r: aquí el texto de cada fila frente al de df (estado "texto") y en verify frac, texto y orden
      # frente a la referencia. Antes solo había el multiconjunto de textos, que no ve un valor en la fila equivocada.
      # Un drawn que falla es un error de la figura (no se ignora: la comprobación por fila se perdería sin aviso).
      dr <- tryCatch(wjpr_drawn(out, b), error = function(e) {
        stop("lo dibujado por fila (attr(p, \"wjpr\")$drawn): ", conditionMessage(e), call. = FALSE)
      })
      if (!is.null(dr)) {
        drawn <- dr
        by_key <- wjpr_drawn_by_key(dr, df)
        dif <- drawn_text_diff(by_key, df)
        if (nrow(dif) > 0) {
          log_msg(tag, ": texto dibujado ≠ df en ", nrow(dif), if (nrow(dif) == 1) " fila: " else " filas: ",
                  paste(head(sprintf("%s %s: %s→%s", dif$campo, dif$label, dif$df, dif$drawn), 5), collapse = "; "))
        }
        # corregido (revisión 4): el valor que codifica la geometría (val/val2, contrato extendido) frente a df →
        # estado "dibujo"; antes una barra que no era la del df solo la veía verify (frac) frente a la referencia
        vdif <- drawn_value_diff(by_key, df)
        if (nrow(vdif) > 0) {
          log_msg(tag, ": dibujo ≠ df en ", nrow(vdif), if (nrow(vdif) == 1) " fila" else " filas", " (tol ", trimws(formatC(vdif$tol[1], digits = 3, format = "fg")), "): ",
                  paste(head(sprintf("%s %s: %s→%s", vdif$campo, vdif$label, vdif$df, vdif$drawn), 5), collapse = "; "))
        }
        if (is.unsorted(by_key$order[!is.na(by_key$order)])) {
          log_msg(tag, ": las filas dibujadas, de arriba abajo, no siguen el orden de df (verify lo compara con la referencia)")
        }
      }
      # corregido (revisión 4): marcas fijas del diseño sin fila en df (attr(p, "wjpr")$marcas, p. ej. la línea de corte
      # de la 7): una que falta en el dibujo → estado "dibujo"; antes solo la medía el parecido local (0.118 sin la
      # línea: ok). Una función de marcas que falla es un error de la figura, como drawn
      if (is.function(meta$marcas)) {
        mk <- tryCatch(as.character(meta$marcas(b)), error = function(e) {
          stop("marcas fijas del diseño (attr(p, \"wjpr\")$marcas): ", conditionMessage(e), call. = FALSE)
        })
        if (length(mk)) log_msg(tag, ": marca fija del diseño ausente del dibujo: ", paste(mk, collapse = "; "))
      }
      # corregido (cierre, revisión de usabilidad): el alto que pide el diseño (attr(p, "wjpr")$alto, hoy las agrupadas)
      # frente al h_in del manifiesto: menor → estado "tamaño" (el dibujo se recorta abajo); mayor → aviso con el h_in
      # que pide (sobra espacio abajo; un país con menos filas, p. ej. small_sample). Antes los dos quedaban ok y, sin
      # figura publicada (una figura nueva), nada lo veía
      if (is.numeric(meta$alto) && length(meta$alto) == 1 && is.finite(meta$alto)) {
        d_in <- spec$h_in - meta$alto; tol_in <- cfg$verify$size_tol_in %||% 0.01
        if (d_in < -tol_in) {
          talla <- sprintf("el diseño pide h_in = %.3f in y el manifiesto da %s: se recorta abajo", meta$alto, fmt_in(spec$h_in))
        } else if (d_in > tol_in) {
          log_msg(tag, " aviso: el diseño pide h_in = ", sprintf("%.3f", meta$alto), " in y el manifiesto da ", fmt_in(spec$h_in),
                  " (sobra espacio abajo): fíjelo en manifest/figures_<slug>.csv con la fila «", as.integer(spec$fig), ",",
                  sprintf("%.3f", meta$alto), "» (columna h_in)")
        }
      }
      wj <- list(build = wjpr_build(), complementos = if (is.null(meta)) NA_integer_ else length(meta$complementos),
                 textos = wt$resumen, drawn = if (is.null(dr)) NA_integer_ else nrow(dr))
      log_msg(tag, ": WJPr ", wj$build, " · base ", meta$base %||% "?", " · ", wj$complementos, " complementos (",
              paste(meta$complementos, collapse = "; "), ") · textos ", wt$resumen, " (", wt$n_comparados, " dibujados de ",
              wt$n_esperados, " esperados) · por fila: ", if (is.null(dr)) "el adaptador no lo expone" else paste(nrow(dr), "filas en drawn.csv"))
    }
  } else {
    stop("plot() devolvió un objeto de clase '", class(out)[1], "' (se esperaba chart_out o ggplot)", call. = FALSE)
  }
  # corregido (revisión 4): drawn.csv siempre con las columnas del contrato extendido (las opcionales NA donde el
  # método o el adaptador no las da, p. ej. el drawn de chart_out en svg_r): verify reconoce así un drawn.csv anterior
  write_csv(wjpr_drawn_complete(drawn), p$drawn, na = "NA")

  est <- character(); mot <- character()                       # problemas, en orden de precedencia
  flag <- function(e, m) { est <<- c(est, e); mot <<- c(mot, m) }
  size <- svg_size(p$svg)
  if (abs(size$width_in - spec$w_in) > 0.001 || abs(size$height_in - spec$h_in) > cfg$verify$size_tol_in) {
    flag("tamaño", sprintf("tamaño %s×%s in ≠ %s×%s del manifiesto", fmt_in(size$width_in), fmt_in(size$height_in), fmt_in(spec$w_in), fmt_in(spec$h_in)))
  }
  if (!is.na(talla)) flag("tamaño", talla)
  n_text <- count_live_text(p$svg)
  if (n_text > 0) {
    flag("texto", sprintf("%d <text> vivos en el SVG", n_text))
    log_msg(tag, ": ", n_text, " <text> sin contornear")
  }
  if (!is.null(dif) && nrow(dif) > 0) {
    flag("texto", sprintf("texto dibujado ≠ df en %d %s · %s %s: %s→%s", nrow(dif), if (nrow(dif) == 1) "fila" else "filas",
                          dif$campo[1], dif$label[1], dif$df[1], dif$drawn[1]))
  }
  if (!is.null(wt) && length(wt$faltan) + length(wt$sobran) > 0) flag("texto", paste0("textos dibujados ≠ df: ", wt$resumen))
  if (!is.null(dud) && nrow(dud) > 0) {
    flag("texto", sprintf("%d textos dudosos al contornear (familia ≠ Inter o textLength fuera de ±3 %%)", nrow(dud)))
  }
  if (!is.null(vdif) && nrow(vdif) > 0) {
    flag("dibujo", sprintf("dibujo ≠ df en %d %s · %s %s: %s→%s", nrow(vdif), if (nrow(vdif) == 1) "fila" else "filas",
                           vdif$campo[1], vdif$label[1], vdif$df[1], vdif$drawn[1]))
  }
  if (length(mk)) flag("dibujo", paste0("marca fija ausente: ", paste(mk, collapse = "; ")))
  png_px <- NA_character_; png_path <- NA_character_
  if (png && !is.na(cfg$paths$chrome)) {
    px <- tryCatch(chrome_png(p$svg, p$png, spec$w_in, spec$h_in, dpi = cfg$layout$dpi, chrome = cfg$paths$chrome),
                   error = function(e) e)
    if (inherits(px, "error")) {
      log_msg(tag, " PNG: ", conditionMessage(px))
      flag("png", paste0("PNG: ", first_line(conditionMessage(px), 55)))
    } else {
      png_px <- sprintf("%dx%d", px[1], px[2]); png_path <- p$png
    }
  }
  estado <- if (length(est) > 0) est[1] else "ok"
  list(estado = estado, motivo = paste(mot, collapse = " · "), svg = p$svg, png = png_path, width_in = size$width_in,
       height_in = size$height_in, n_text = n_text, png_px = png_px, drawn = drawn, wjpr = wj)
}


## Etapa figures ------------------------------------------------------------------------------------------------

# figures_status.csv escrito con un esquema anterior (sin motivo): se reescribe con STATUS_COLS (lo que falte queda
# NA) antes de anexar, para que las filas nuevas no tengan más campos que el encabezado
migrate_status <- function(f) {
  if (!file.exists(f)) return(invisible(FALSE))
  rd <- function(...) read_csv(f, col_types = cols(.default = col_character()), show_col_types = FALSE, progress = FALSE, ...)
  hdr <- names(rd(n_max = 0))
  if (identical(hdr, STATUS_COLS)) return(invisible(FALSE))
  tmpl <- as_tibble(setNames(rep(list(character()), length(STATUS_COLS)), STATUS_COLS))
  write_csv(bind_rows(tmpl, rd())[, STATUS_COLS], f, na = "NA")
  log_msg("figures_status.csv: esquema anterior (", length(hdr), " columnas) reescrito con ", length(STATUS_COLS))
  invisible(TRUE)
}

# Añade la fila de la figura a output/<slug>/log/figures_status.csv (§4.6; una por figura y corrida). ctx_key es
# la clave del ctx (decisión 6 de la revisión: ctx$meta$ctx_key; si el ctx no la trae, ctx_key() de 01_load.R sobre
# ctx$meta$key), el mismo texto que cache/ctx_<slug>.key, para que verify lo compare. motivo: ver STATUS_COLS.
append_status <- function(spec, cfg, run_id, ctx, res, estado, t0, motivo = "") {
  p <- figure_paths(spec, cfg)
  row <- tibble(
    fig = as.integer(spec$fig), slug = spec$slug, run_id = run_id,
    ctx_key = ctx$meta$ctx_key %||% ctx_key(cfg, ctx$meta$key %||% NA_character_),
    md5_table = md5_or_na(p$table), md5_drawn = md5_or_na(p$drawn), md5_svg = md5_or_na(p$svg), md5_png = md5_or_na(p$png),
    n_text = as.integer(res$n_text %||% NA_integer_), width_in = as.numeric(res$width_in %||% NA_real_),
    height_in = as.numeric(res$height_in %||% NA_real_), png_px = as.character(res$png_px %||% NA_character_),
    estado = estado, segundos = round(secs(t0), 1), fecha = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    motivo = as.character(motivo %||% ""), wjpr_build = as.character(res$wjpr$build %||% NA_character_),
    wjpr_complementos = as.integer(res$wjpr$complementos %||% NA_integer_),
    wjpr_textos = as.character(res$wjpr$textos %||% NA_character_),
    wjpr_drawn = as.integer(res$wjpr$drawn %||% NA_integer_)
  )[, STATUS_COLS]
  migrate_status(p$status)
  write_csv(row, p$status, append = file.exists(p$status), na = "NA")
  invisible(row)
}

# Una figura seleccionada: borra sus salidas anteriores (nada viejo sobrevive a un fallo), corre data(ctx) →
# validate_df → tabla → plot → save_figure dentro de tryCatch (avisos y mensajes al log; error → estado "error"
# y se sigue con la siguiente) y registra la fila en figures_status.csv. Devuelve la fila de consola (§4.7).
export_figure <- function(spec, ctx, cfg, run_id, png = TRUE) {
  t0 <- Sys.time(); p <- figure_paths(spec, cfg); tag <- sprintf("fig %02d", spec$fig)
  unlink(c(p$table, p$drawn, p$svg, p$png))
  res <- NULL; n_rows <- NA_integer_
  err <- tryCatch({
    withCallingHandlers({
      fig <- load_figure(spec, cfg)
      df <- fig$data(ctx)
      validate_df(df)
      n_rows <- nrow(df)
      write_csv(df, p$table, na = "NA")                       # la tabla se escribe ANTES de dibujar
      log_msg(tag, ": tabla ", n_rows, " filas → ", rel_path(p$table, cfg$paths$root))
      out <- fig$plot(df, spec)
      res <- save_figure(out, df, spec, cfg, png)
    },
    warning = function(w) { log_msg(tag, " aviso: ", conditionMessage(w)); invokeRestart("muffleWarning") },
    message = function(m) { log_msg(tag, " mensaje: ", trimws(conditionMessage(m))); invokeRestart("muffleMessage") })
    NULL
  }, error = function(e) e)
  if (!is.null(err)) {
    msg <- conditionMessage(err)
    log_msg(tag, " error: ", msg)
    estado <- "error"; verif <- first_line(msg, 60)
    motivo <- first_line(msg, 1000)                            # la línea entera: verify la muestra ("figures: …")
    archivo <- if (file.exists(p$svg)) p$svg else figure_file(spec, cfg)
  } else {
    estado <- res$estado
    verif <- if (estado == "ok") {
      sprintf("%d filas · %s×%s in · %s · %d <text>%s", n_rows, fmt_in(res$width_in), fmt_in(res$height_in),
              if (is.na(res$png_px)) "sin PNG" else paste0(sub("x", "×", res$png_px, fixed = TRUE), " px"), res$n_text,
              if (is.null(res$wjpr)) "" else if (identical(res$wjpr$textos, "sin textos que comparar")) " · sin textos que comparar"
              else paste0(" · textos ", res$wjpr$textos))
    } else res$motivo
    motivo <- if (estado == "ok") "" else res$motivo
    archivo <- p$svg
  }
  log_msg(tag, ": ", estado, " · ", verif, " · ", round(secs(t0), 1), " s")
  append_status(spec, cfg, run_id, ctx, res, estado, t0, motivo)
  tibble(fig = sprintf("%02d", spec$fig), metodo = spec$metodo, estado = estado, verificacion = verif,
         archivo = rel_path(archivo, cfg$paths$root))
}

# Etapa figures. Devuelve tibble(fig, metodo, estado, verificacion, archivo), una fila por spec en el orden del
# manifiesto; spec$selected == FALSE → "omitida". Estados: ok | error | png | tamaño | texto | dibujo | omitida
# (dibujo: el valor que codifica la geometría, val/val2 de lo dibujado por fila, no es el de df; revisión 4).
# Antes de dibujar, check_method_deps() (00_setup.R) detiene la etapa con un error legible si una figura seleccionada
# con metodo wjpr no tiene WJPr o ggtext; con WJPr, wjpr_require() exige WJPr ≥ 1.2.0 y fija options(wjpr.family)
# (como la galería) y el log registra la build que dibuja.
run_figures <- function(specs, ctx, cfg, run_id, png = TRUE) {
  if (png && is.na(cfg$paths$chrome)) log_msg("Chrome no encontrado: se omiten los PNG")
  if (!png) log_msg("PNG omitidos (png = FALSE)")
  check_method_deps(specs, cfg)
  if (any(vapply(specs, function(s) isTRUE(s$selected %||% TRUE) && identical(s$metodo, "wjpr"), logical(1)))) {
    wjpr_require()
    log_msg("WJPr ", wjpr_build(), " · complementos ", wjpr_complementos())
  }
  rows <- lapply(specs, function(spec) {
    if (!isTRUE(spec$selected %||% TRUE)) {
      return(tibble(fig = sprintf("%02d", spec$fig), metodo = spec$metodo, estado = "omitida", verificacion = "", archivo = ""))
    }
    export_figure(spec, ctx, cfg, run_id, png)
  })
  out <- bind_rows(rows)
  log_msg("figures: ", sum(out$estado == "ok"), " ok de ", sum(out$estado != "omitida"), " seleccionadas (",
          sum(out$estado == "omitida"), " omitidas)")
  out
}


## Prueba ------------------------------------------------------------------------------------------------------
## Etapa 1: cd /tmp && Rscript --vanilla SCRATCH/agente_E/prueba.R (cfg con paths$root/output redirigidos a
## SCRATCH/agente_E/root): chart_out mínimo de 2×1 in (fig 99) → ok, SVG + PNG 600×300 px a 300 dpi, tabla y
## drawn.csv; stub figures/fig_09_* → "error" ("pendiente: fig_09 sin implementar"); spec no seleccionada →
## "omitida"; w_in distinto → "tamaño"; <text> vivo → "texto"; png = FALSE → "sin PNG"; static → ok; df sin
## columna → "error" de validate_df; chrome_png directo → c(600, 300).
## Revisión (2026-10-23): cd /tmp && Rscript --vanilla SCRATCH/fix/G4/tests/t_06.R antes|despues (carga R/00-05 de la
## copia y 06-08 de orig/ o de la copia; t_06_child.R para TMPDIR y SIGSEGV; salidas en SCRATCH/fix/G4/tests/
## out_06_{antes,despues}.txt). Antes → después:
##   drawn$text[National Average] = "99%" con df "61%" (fig 09): ok → "texto" ("texto dibujado ≠ df en 1 fila · text
##   National Average: 61%→99%"); drawn sin la fila Male: ok → "texto"; drawn en otro orden físico con el mismo order:
##   ok con un aviso falso en el log → ok sin aviso; <text> vivo + texto ≠ df: "texto" con los dos motivos;
##   Chrome falso que captura 585×806: chrome_png devolvía c(585, 806) y save_figure "ok" → error "Chrome capturó
##   585×806 px; se esperaban al menos 1821×2512" y estado "png" (captura de 1828×2519 → c(1821, 2512) en ambas);
##   ggplot sin base_family (Arial): 13 de 13 textos dudosos, "ok" y 0 líneas de log → "texto" y una línea de log
##   (con base_family = "inter": ok en ambas); Chrome en "~/../../private/…": código 127 → c(1821, 2512);
##   TMPDIR con '#' y '%20' y Chrome real: "(código 0)" → c(600, 300) con los píxeles esperados; Chrome que muere por
##   SIGSEGV: "sh: line 1: … Segmentation fault" en el stderr del proceso R → stderr vacío, error en ambas;
##   figures_status.csv: 15 → 16 columnas (motivo "pendiente: fig_12 sin implementar"; un archivo de 15 columnas se
##   reescribe con 16 al anexar, motivo NA en la fila vieja) y ctx_key = ctx$meta$ctx_key ("CK") si existe, si no
##   ctx_key(cfg, ctx$meta$key); código muerto (sin uso en main.R, R/, figures/ ni tools/): png_props 1 → 0 líneas
##   de código, VERIFY_FIELDS/RESULTS/STATES de 07 3 → 0 y el comentario huérfano de 08 1 → 0.
##   probe_r.R: el timeout de system2() da 124 y no deja procesos vivos con exec y sin exec.
## Adopción de WJPr (2026-10-23): cd /tmp && Rscript <copia>/main.R --stage all --figs 09,10,11 → 3/3 con metodo wjpr
##   (WJPr 1.1.0@382aee9; 12, 13 y 6 complementos; textos iguales; 0 <text>; 6.069×8.375, 6.500×5.250 y 8.400×5.167 in);
##   SVG y PNG idénticos (md5) a los de tools/wjpr_gallery.R --figs 9,10,11; figures_status.csv de 16 → 19 columnas
##   (migrate_status). SCRATCH/adopt_M3/pruebas/t_adopcion.R: un texto de df cambiado ("61%" → "99%") → estado "texto ·
##   textos dibujados ≠ df: faltan 1 (99%) · sobran 1 (61%)"; chart_out con metodo wjpr → error con la pista de plot =
##   wjpr_<tipo>(); ggplot sin attr(, "wjpr") → ok con complementos NA y aviso en el log; metodo ggplot sin base_pt
##   sigue en error. Sin WJPr o ggtext la etapa se detiene con el mensaje de check_method_deps() (00_setup.R).
## Revisión 3 (2026-10-23, F1: V3-02, V3-07): cd /tmp && Rscript <copia>/main.R --stage all --figs 01,02,03,04,05,06,07,
##   09,10,11 → 10/10 con 1.1.0@382aee9 y con main (SRC_REPORTS_LIB); tablas, SVG y PNG idénticos (md5) a los de antes;
##   drawn.csv de 01, 04 y 09 con 18, 17 y 36 filas (antes vacíos), igual a la referencia (|Δfrac| ≤ 0.0028, textos y
##   orden visual iguales, también con main); figures_status.csv 19 → 20 columnas (wjpr_drawn). SCRATCH/fix3/F1/pruebas/
##   run_exp.sh (solo plot() alterado): E7, textos de dos filas intercambiados (01: 65-100 ↔ 45-54; 04: 65-100 ↔ 18-24):
##   ok "textos iguales" → texto "texto dibujado ≠ df en 2 filas · text 65-100: 61%→72%" (04: 3.5→6.4); 03 y 07:
##   "textos iguales" → "sin textos que comparar". p11_unit.R: wjpr_drawn_by_key (b→2, a→1, sobrante → NA),
##   drawn_text_diff con una fila sobrante (1 fila) y con dos textos intercambiados (2 filas).
