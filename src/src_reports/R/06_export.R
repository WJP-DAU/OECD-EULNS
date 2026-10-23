## 06_export.R — etapa figures: tabla → dibujo → SVG/PNG → estado por figura -----------------------------------
## Origen: src/hungary/finales/build_finales.R:131-152 (chrome_png: captura con Chrome headless y recorte a
## píxeles exactos); render_figure.R:80 (ggsave con svglite); standard_figures.R:438-442 (outline_checked: recuento
## de textos dudosos al contornear). png_props (build_finales.R:154-157) no se porta: 07 lee las dimensiones del PNG
## con png_dims() y 08 con read_png_rgb().
## El flujo por figura (run_figures, validate_df, save_figure, figures_status.csv) es nuevo para src_reports.
## Usa de 00_setup.R: load_figure(), figure_file(), figure_stem(), log_msg(), first_line(); de 01_load.R: secs(),
## rel_path(), ctx_key(); de 04_svg.R: svg_size(), outline_svg_text(), set_size(), count_live_text().
## Nada se ejecuta al cargar. Nunca se imprimen filas de microdatos: df es ya un agregado y solo se escribe a CSV.

# Columnas fijas de df (CONTRATOS §4.4), extras permitidas y columnas de log/figures_status.csv (§4.6 + motivo,
# decisión 2 de la revisión: primera línea del error o del motivo del estado; "" si ok)
DF_COLS     <- c("order", "kind", "grouping", "label", "value", "value2", "text", "text2")
DF_EXTRAS   <- c("x", "y", "n", "lo", "hi", "series")
STATUS_COLS <- c("fig", "slug", "run_id", "ctx_key", "md5_table", "md5_drawn", "md5_svg", "md5_png", "n_text",
                 "width_in", "height_in", "png_px", "estado", "segundos", "fecha", "motivo")


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

# drawn vacío con encabezado (figuras ggplot y static: no hay barras medidas)
empty_drawn <- function() {
  tibble(order = integer(), kind = character(), grouping = character(), label = character(),
         frac = numeric(), frac2 = numeric(), text = character(), text2 = character())
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
drawn_text_diff <- function(drawn, df) {
  chr <- function(v) coalesce(as.character(v), "")
  i <- match(df$order, drawn$order)
  pick <- function(v) ifelse(is.na(i), "", chr(v)[i])
  d1 <- pick(drawn$text); d2 <- pick(drawn$text2)
  extra <- !(drawn$order %in% df$order) & (nzchar(chr(drawn$text)) | nzchar(chr(drawn$text2)))
  bind_rows(
    tibble(campo = "text", label = df$label, df = df$text, drawn = d1)[d1 != df$text, ],
    tibble(campo = "text2", label = df$label, df = df$text2, drawn = d2)[d2 != df$text2, ],
    tibble(campo = "text", label = chr(drawn$label[extra]), df = "", drawn = chr(drawn$text[extra]))
  )
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
save_ggplot_svg <- function(plot, spec, cfg, svg) {
  if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_auto(FALSE)   # svglite debe emitir <text>
  if (is.na(spec$base_pt)) stop("metodo ggplot requiere base_pt en el manifiesto (fig ", spec$fig, ")", call. = FALSE)
  k <- spec$base_pt / cfg$layout$text_pt
  tmp <- tempfile(fileext = ".svg")
  ggplot2::ggsave(tmp, plot, device = svglite::svglite, width = spec$w_in * k, height = spec$h_in * k, units = "in")
  lg <- outline_svg_text(tmp)
  dud <- doubtful_texts(lg)
  if (nrow(dud) > 0) {
    fam <- if ("family" %in% names(dud)) paste(unique(coalesce(dud$family, "sin familia")), collapse = "/") else "?"
    log_msg(sprintf("fig %02d", spec$fig), ": ", nrow(dud), " de ", nrow(lg), " textos dudosos al contornear (familia ",
            fam, "): ", paste(head(dud$text, 8), collapse = " | "), if (nrow(dud) > 8) " | …" else "")
  }
  set_size(tmp, spec$w_in)
  if (!file.copy(tmp, svg, overwrite = TRUE)) stop("No se pudo mover el SVG a ", svg, call. = FALSE)
  unlink(tmp)
  invisible(lg)
}

# Escribe figures/Figure_NN_slug.svg (y .png) y tables/Figure_NN_slug.drawn.csv a partir de lo que devolvió
# plot(): chart_out (svg_r), ggplot o nada (static: se copia static/Figure_NN_slug.svg). Luego comprueba el
# archivo, con esta precedencia de estados: ancho exacto (±0.001 in) y alto ± cfg$verify$size_tol_in → "tamaño";
# <text> vivos, texto dibujado ≠ df$text/text2 (svg_r) o textos dudosos al contornear (ggplot) → "texto"; PNG
# (solo si png && hay Chrome) fallido → "png". El dibujo no se corrige. motivo junta todos los problemas, el del
# estado primero. Devuelve list(estado, motivo, svg, png, width_in, height_in, n_text, png_px, drawn).
save_figure <- function(out, df, spec, cfg, png = TRUE) {
  p <- figure_paths(spec, cfg); tag <- sprintf("fig %02d", spec$fig)
  if (anyNA(c(spec$w_in, spec$h_in))) stop("w_in/h_in faltan en el manifiesto (fig ", spec$fig, ")", call. = FALSE)
  dif <- NULL; dud <- NULL
  if (identical(spec$metodo, "static")) {
    src <- file.path(cfg$paths$root, "static", paste0(p$stem, ".svg"))
    if (!file.exists(src)) stop("No existe static/", basename(src), " (metodo static)", call. = FALSE)
    if (!file.copy(src, p$svg, overwrite = TRUE)) stop("No se pudo copiar ", src, call. = FALSE)
    drawn <- empty_drawn()
  } else if (inherits(out, "chart_out")) {
    if (!identical(spec$metodo, "svg_r")) stop("plot() devolvió chart_out pero el manifiesto dice metodo '", spec$metodo, "'", call. = FALSE)
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
    if (!identical(spec$metodo, "ggplot")) stop("plot() devolvió ggplot pero el manifiesto dice metodo '", spec$metodo, "'", call. = FALSE)
    dud <- doubtful_texts(save_ggplot_svg(out, spec, cfg, p$svg))
    drawn <- empty_drawn()
  } else {
    stop("plot() devolvió un objeto de clase '", class(out)[1], "' (se esperaba chart_out o ggplot)", call. = FALSE)
  }
  write_csv(drawn, p$drawn, na = "NA")

  est <- character(); mot <- character()                       # problemas, en orden de precedencia
  flag <- function(e, m) { est <<- c(est, e); mot <<- c(mot, m) }
  size <- svg_size(p$svg)
  if (abs(size$width_in - spec$w_in) > 0.001 || abs(size$height_in - spec$h_in) > cfg$verify$size_tol_in) {
    flag("tamaño", sprintf("tamaño %s×%s in ≠ %s×%s del manifiesto", fmt_in(size$width_in), fmt_in(size$height_in), fmt_in(spec$w_in), fmt_in(spec$h_in)))
  }
  n_text <- count_live_text(p$svg)
  if (n_text > 0) {
    flag("texto", sprintf("%d <text> vivos en el SVG", n_text))
    log_msg(tag, ": ", n_text, " <text> sin contornear")
  }
  if (!is.null(dif) && nrow(dif) > 0) {
    flag("texto", sprintf("texto dibujado ≠ df en %d %s · %s %s: %s→%s", nrow(dif), if (nrow(dif) == 1) "fila" else "filas",
                          dif$campo[1], dif$label[1], dif$df[1], dif$drawn[1]))
  }
  if (!is.null(dud) && nrow(dud) > 0) {
    flag("texto", sprintf("%d textos dudosos al contornear (familia ≠ Inter o textLength fuera de ±3 %%)", nrow(dud)))
  }
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
       height_in = size$height_in, n_text = n_text, png_px = png_px, drawn = drawn)
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
    motivo = as.character(motivo %||% "")
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
      sprintf("%d filas · %s×%s in · %s · %d <text>", n_rows, fmt_in(res$width_in), fmt_in(res$height_in),
              if (is.na(res$png_px)) "sin PNG" else paste0(sub("x", "×", res$png_px, fixed = TRUE), " px"), res$n_text)
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
# manifiesto; spec$selected == FALSE → "omitida". Estados: ok | error | png | tamaño | texto | omitida.
run_figures <- function(specs, ctx, cfg, run_id, png = TRUE) {
  if (png && is.na(cfg$paths$chrome)) log_msg("Chrome no encontrado: se omiten los PNG")
  if (!png) log_msg("PNG omitidos (png = FALSE)")
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
